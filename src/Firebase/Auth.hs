-- |
-- Module      : Firebase.Auth
-- Description : Firebase ID token verification
-- License     : BSD-3-Clause
--
-- Verify Firebase Authentication ID tokens (JWTs) against Google's
-- public keys using crypton for RS256 signature verification.
--
-- @
-- import Firebase.Auth
--
-- main :: IO ()
-- main = do
--   cache <- newTlsKeyCache
--   let cfg = defaultFirebaseConfig \"my-project-id\"
--   result <- verifyIdTokenCached cache cfg someJwtBytes
--   case result of
--     Left err   -> putStrLn (\"Auth failed: \" ++ show err)
--     Right user -> putStrLn (\"Welcome, \" ++ show (fuUid user))
-- @
module Firebase.Auth
  ( -- * One-shot verification
    verifyIdToken,

    -- * Cached verification
    KeyCache,
    newKeyCache,
    newTlsKeyCache,
    verifyIdTokenCached,

    -- * Configuration
    FirebaseConfig (..),
    defaultFirebaseConfig,

    -- * Authenticated user
    FirebaseUser (..),
    lookupClaim,
    hasClaim,

    -- * Errors
    AuthError (..),
    authErrorMessage,

    -- * Utilities
    parseCacheMaxAge,
  )
where

import Control.Exception (try)
import Control.Monad (guard)
import Crypto.Hash.Algorithms (SHA256 (..))
import Crypto.PubKey.RSA.PKCS15 (verify)
import Crypto.PubKey.RSA.Types (PublicKey)
import Data.Aeson (FromJSON (..), (.!=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as Aeson
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Firebase.Auth.Internal (padBase64Url)
import Firebase.Auth.Types
import Network.HTTP.Client
  ( HttpException,
    Manager,
    Response,
    httpLbs,
    parseRequest,
    responseBody,
    responseHeaders,
  )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Header (ResponseHeaders, hCacheControl)

-- ---------------------------------------------------------------------------
-- JWT internal types
-- ---------------------------------------------------------------------------

data JwtHeader = JwtHeader
  { jhAlg :: !Text,
    jhKid :: !Text
  }

instance FromJSON JwtHeader where
  parseJSON = Aeson.withObject "JwtHeader" $ \o ->
    JwtHeader
      <$> o .: "alg"
      <*> o .: "kid"

data JwtPayload = JwtPayload
  { jpSub :: !(Maybe Text),
    jpIss :: !(Maybe Text),
    jpAud :: !(Maybe Text),
    jpExp :: !(Maybe Integer),
    jpIat :: !(Maybe Integer),
    jpAuthTime :: !(Maybe Integer),
    jpEmail :: !(Maybe Text),
    jpEmailVerified :: !Bool,
    jpName :: !(Maybe Text),
    jpPicture :: !(Maybe Text),
    jpSignInProvider :: !(Maybe Text),
    jpCustomClaims :: !(Map Text Aeson.Value)
  }

instance FromJSON JwtPayload where
  parseJSON = Aeson.withObject "JwtPayload" $ \o ->
    JwtPayload
      <$> o .:? "sub"
      <*> o .:? "iss"
      <*> o .:? "aud"
      <*> o .:? "exp"
      <*> o .:? "iat"
      <*> o .:? "auth_time"
      <*> o .:? "email"
      <*> o .:? "email_verified" .!= False
      <*> o .:? "name"
      <*> o .:? "picture"
      <*> parseSignInProvider o
      <*> pure (customClaims o)

-- | Read @firebase.sign_in_provider@ out of the nested Firebase claim.
parseSignInProvider :: Aeson.Object -> Aeson.Parser (Maybe Text)
parseSignInProvider o =
  o .:? "firebase"
    >>= maybe (pure Nothing) (Aeson.withObject "firebase" (.:? "sign_in_provider"))

-- | Claim names Firebase reserves, which 'setCustomUserClaims' may not set.
--
-- Everything outside this set is a custom claim and is surfaced as one.
reservedClaims :: Set Text
reservedClaims =
  Set.fromList
    [ "acr",
      "amr",
      "at_hash",
      "aud",
      "auth_time",
      "azp",
      "c_hash",
      "cnf",
      "exp",
      "email",
      "email_verified",
      "firebase",
      "iat",
      "iss",
      "jti",
      "name",
      "nbf",
      "nonce",
      "phone_number",
      "picture",
      "sub",
      "user_id"
    ]

-- | Every claim outside 'reservedClaims'.
customClaims :: Aeson.Object -> Map Text Aeson.Value
customClaims payload =
  Map.fromList
    [ (name, value)
    | (key, value) <- KM.toList payload,
      let name = Key.toText key,
      not (Set.member name reservedClaims)
    ]

-- | The three dot-separated parts of a compact JWT, still base64url-encoded.
data JwtParts = JwtParts
  { jwtHeaderPart :: !BS.ByteString,
    jwtPayloadPart :: !BS.ByteString,
    jwtSignaturePart :: !BS.ByteString
  }

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

googleJwkUrl :: String
googleJwkUrl =
  "https://www.googleapis.com/service_accounts/v1/jwk/securetoken@system.gserviceaccount.com"

-- | Cache lifetime assumed when Google's response carries no @max-age@.
defaultCacheDurationSeconds :: NominalDiffTime
defaultCacheDurationSeconds = 3600

firebaseIssuerPrefix :: Text
firebaseIssuerPrefix = "https://securetoken.google.com/"

expectedAlgorithm :: Text
expectedAlgorithm = "RS256"

-- | The @Cache-Control@ directive carrying a response's lifetime in seconds.
maxAgeDirective :: BS.ByteString
maxAgeDirective = "max-age="

-- | The separator between the three parts of a compact JWT.
jwtPartSeparator :: Char
jwtPartSeparator = '.'

-- ---------------------------------------------------------------------------
-- One-shot verification
-- ---------------------------------------------------------------------------

-- | Verify a Firebase ID token, fetching Google's public keys fresh.
--
-- For production servers, prefer 'verifyIdTokenCached' to avoid
-- re-fetching keys on every call.
verifyIdToken ::
  Manager ->
  FirebaseConfig ->
  BS.ByteString ->
  IO (Either AuthError FirebaseUser)
verifyIdToken mgr config token = do
  fetched <- fetchGoogleKeys mgr
  either (pure . Left) (verifyWithKeys config token . fst) fetched

-- ---------------------------------------------------------------------------
-- Cached verification
-- ---------------------------------------------------------------------------

-- | Create a key cache backed by the given HTTP manager.
--
-- The cache starts empty and expired, so the first verification fetches
-- Google's keys.
newKeyCache :: Manager -> IO KeyCache
newKeyCache mgr = do
  epoch <- getCurrentTime
  ref <- newIORef (JwkSet [], epoch)
  pure KeyCache {kcKeysRef = ref, kcManager = mgr}

-- | Create a key cache with a fresh TLS-enabled HTTP manager.
newTlsKeyCache :: IO KeyCache
newTlsKeyCache = newTlsManager >>= newKeyCache

-- | Verify a Firebase ID token using cached public keys.
--
-- Keys are refreshed automatically when the cache expires (per Google's
-- @Cache-Control: max-age@ response header).
verifyIdTokenCached ::
  KeyCache ->
  FirebaseConfig ->
  BS.ByteString ->
  IO (Either AuthError FirebaseUser)
verifyIdTokenCached cache config token = do
  keys <- currentKeys cache
  either (pure . Left) (verifyWithKeys config token) keys

-- | The cached key set, refetching first if it has expired.
currentKeys :: KeyCache -> IO (Either AuthError JwkSet)
currentKeys cache = do
  now <- getCurrentTime
  (keys, expiry) <- readIORef (kcKeysRef cache)
  if now < expiry
    then pure (Right keys)
    else refreshKeys cache

-- | Fetch a fresh key set and install it in the cache.
refreshKeys :: KeyCache -> IO (Either AuthError JwkSet)
refreshKeys cache = do
  fetched <- fetchGoogleKeys (kcManager cache)
  traverse install fetched
  where
    install entry = do
      atomicModifyIORef' (kcKeysRef cache) (\cached -> (laterExpiring cached entry, ()))
      pure (fst entry)

-- | Of two cache entries, the one that stays valid longer.
--
-- Concurrent verifications can each refresh on expiry; keeping the later
-- expiry stops a slow response from displacing a newer key set.
laterExpiring :: (JwkSet, UTCTime) -> (JwkSet, UTCTime) -> (JwkSet, UTCTime)
laterExpiring cached fetched
  | snd fetched > snd cached = fetched
  | otherwise = cached

-- ---------------------------------------------------------------------------
-- Key fetching
-- ---------------------------------------------------------------------------

-- | Fetch Google's current public keys.
--
-- Only 'HttpException' becomes a 'KeyFetchError': anything else
-- (asynchronous cancellation, for instance) is not a fetch result and
-- propagates.
fetchGoogleKeys :: Manager -> IO (Either AuthError (JwkSet, UTCTime))
fetchGoogleKeys mgr = do
  req <- parseRequest googleJwkUrl
  fetched <- try (httpLbs req mgr) :: IO (Either HttpException (Response LBS.ByteString))
  case fetched of
    Left err -> pure (Left (KeyFetchError (T.pack (show err))))
    Right resp -> do
      now <- getCurrentTime
      pure (fmap (,cacheExpiry now resp) (decodeKeys (responseBody resp)))

-- | When a key response stops being usable, per its @Cache-Control@ header.
cacheExpiry :: UTCTime -> Response LBS.ByteString -> UTCTime
cacheExpiry now resp = addUTCTime duration now
  where
    duration =
      maybe defaultCacheDurationSeconds fromIntegral $
        parseCacheMaxAge (responseHeaders resp)

decodeKeys :: LBS.ByteString -> Either AuthError JwkSet
decodeKeys = first (KeyFetchError . T.pack) . Aeson.eitherDecode

-- | Parse the @max-age@ directive from a @Cache-Control@ response header.
--
-- Yields 'Nothing' when the directive is absent, unparseable, or
-- non-positive, leaving the caller to apply its own default.
--
-- >>> parseCacheMaxAge [("cache-control", "public, max-age=19845, must-revalidate")]
-- Just 19845
parseCacheMaxAge :: ResponseHeaders -> Maybe Int
parseCacheMaxAge headers = do
  cacheControl <- lookup hCacheControl headers
  let (_before, fromDirective) = BS.breakSubstring maxAgeDirective cacheControl
  (seconds, _rest) <- BS8.readInt (BS.drop (BS.length maxAgeDirective) fromDirective)
  seconds <$ guard (seconds > 0)

-- ---------------------------------------------------------------------------
-- JWT verification
-- ---------------------------------------------------------------------------

-- | Verify a token against a key set. Reads the clock, then decides purely.
verifyWithKeys ::
  FirebaseConfig ->
  BS.ByteString ->
  JwkSet ->
  IO (Either AuthError FirebaseUser)
verifyWithKeys config tokenBytes jwks = do
  now <- getCurrentTime
  pure (validateToken config now jwks tokenBytes)

validateToken ::
  FirebaseConfig ->
  UTCTime ->
  JwkSet ->
  BS.ByteString ->
  Either AuthError FirebaseUser
validateToken config now jwks tokenBytes = do
  parts <- splitCompactJwt tokenBytes
  header <- decodeSegment "header" (jwtHeaderPart parts) >>= decodeJson "header"
  payload <- decodeSegment "payload" (jwtPayloadPart parts) >>= decodeJson "payload"
  signature <- decodeSegment "signature" (jwtSignaturePart parts)
  validateAlgorithm header
  pubKey <- findKeyByKid (jhKid header) jwks
  verifySignature pubKey (signedData parts) signature
  validateClaims config now payload
  extractUser payload

-- | The bytes a JWT signature covers: the header and payload, as sent.
signedData :: JwtParts -> BS.ByteString
signedData parts =
  jwtHeaderPart parts <> BS8.singleton jwtPartSeparator <> jwtPayloadPart parts

-- ---------------------------------------------------------------------------
-- JWT parsing
-- ---------------------------------------------------------------------------

splitCompactJwt :: BS.ByteString -> Either AuthError JwtParts
splitCompactJwt token =
  case BS8.split jwtPartSeparator token of
    [header, payload, signature] -> Right (JwtParts header payload signature)
    _ -> Left (MalformedToken "expected 3 dot-separated parts")

decodeSegment :: Text -> BS.ByteString -> Either AuthError BS.ByteString
decodeSegment label = first (malformed label) . B64URL.decode . padBase64Url

decodeJson :: (FromJSON a) => Text -> BS.ByteString -> Either AuthError a
decodeJson label = first (malformed label) . Aeson.eitherDecodeStrict

malformed :: Text -> String -> AuthError
malformed label detail = MalformedToken (label <> ": " <> T.pack detail)

-- ---------------------------------------------------------------------------
-- Signature verification
-- ---------------------------------------------------------------------------

validateAlgorithm :: JwtHeader -> Either AuthError ()
validateAlgorithm header
  | jhAlg header == expectedAlgorithm = Right ()
  | otherwise = Left (MalformedToken ("unsupported algorithm: " <> jhAlg header))

findKeyByKid :: Text -> JwkSet -> Either AuthError PublicKey
findKeyByKid kid (JwkSet keys) =
  maybe (Left InvalidSignature) (Right . jkKey) (find ((== kid) . jkKid) keys)

verifySignature :: PublicKey -> BS.ByteString -> BS.ByteString -> Either AuthError ()
verifySignature pubKey payload signature
  | verify (Just SHA256) pubKey payload signature = Right ()
  | otherwise = Left InvalidSignature

-- ---------------------------------------------------------------------------
-- Claims validation
-- ---------------------------------------------------------------------------

validateClaims :: FirebaseConfig -> UTCTime -> JwtPayload -> Either AuthError ()
validateClaims config now payload = do
  requireClaim "iss" (jpIss payload) (== expectedIssuer) (InvalidClaims "issuer mismatch")
  requireClaim "aud" (jpAud payload) (== projectId) (InvalidClaims "audience mismatch")
  requireExpiry skew now (jpExp payload)
  requireIssuedAt skew now (jpIat payload)
  where
    projectId = fcProjectId config
    expectedIssuer = firebaseIssuerPrefix <> projectId
    skew = fcClockSkew config

requireClaim :: Text -> Maybe Text -> (Text -> Bool) -> AuthError -> Either AuthError ()
requireClaim label mVal predicate err =
  case mVal of
    Just val | predicate val -> Right ()
    Just _ -> Left err
    Nothing -> Left (InvalidClaims ("missing " <> label <> " claim"))

requireExpiry :: NominalDiffTime -> UTCTime -> Maybe Integer -> Either AuthError ()
requireExpiry _skew _now Nothing = Left (InvalidClaims "missing exp claim")
requireExpiry skew now (Just expSeconds)
  | addUTCTime skew (posixSecondsToUTCTime (fromInteger expSeconds)) >= now = Right ()
  | otherwise = Left TokenExpired

requireIssuedAt :: NominalDiffTime -> UTCTime -> Maybe Integer -> Either AuthError ()
requireIssuedAt _skew _now Nothing = Left (InvalidClaims "missing iat claim")
requireIssuedAt skew now (Just iatSeconds)
  | diffUTCTime (posixSecondsToUTCTime (fromInteger iatSeconds)) now <= skew = Right ()
  | otherwise = Left (InvalidClaims "token issued in the future")

-- ---------------------------------------------------------------------------
-- User extraction
-- ---------------------------------------------------------------------------

extractUser :: JwtPayload -> Either AuthError FirebaseUser
extractUser payload =
  case jpSub payload of
    Nothing -> Left (InvalidClaims "missing sub claim")
    Just sub
      | T.null sub -> Left (InvalidClaims "empty sub claim")
      | otherwise ->
          Right
            FirebaseUser
              { fuUid = sub,
                fuEmail = jpEmail payload,
                fuEmailVerified = jpEmailVerified payload,
                fuName = jpName payload,
                fuPicture = jpPicture payload,
                fuAuthTime = posixSecondsToUTCTime . fromInteger <$> jpAuthTime payload,
                fuSignInProvider = jpSignInProvider payload,
                fuCustomClaims = jpCustomClaims payload
              }
