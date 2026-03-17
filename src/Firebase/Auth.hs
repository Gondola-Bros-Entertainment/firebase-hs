{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

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
    newKeyCache,
    newTlsKeyCache,
    verifyIdTokenCached,

    -- * Utilities
    parseCacheMaxAge,

    -- * Re-exports
    module Firebase.Auth.Types,
  )
where

import Control.Concurrent.STM (atomically, newTVarIO, readTVarIO, writeTVar)
import Control.Exception (SomeException, evaluate, try)
import Crypto.Hash.Algorithms (SHA256 (..))
import Crypto.PubKey.RSA.PKCS15 (verify)
import Crypto.PubKey.RSA.Types (PublicKey)
import Data.Aeson (FromJSON (..), (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Firebase.Auth.Types
import Network.HTTP.Client
  ( Manager,
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
    jpEmail :: !(Maybe Text),
    jpName :: !(Maybe Text)
  }

instance FromJSON JwtPayload where
  parseJSON = Aeson.withObject "JwtPayload" $ \o ->
    JwtPayload
      <$> o .:? "sub"
      <*> o .:? "iss"
      <*> o .:? "aud"
      <*> o .:? "exp"
      <*> o .:? "iat"
      <*> o .:? "email"
      <*> o .:? "name"

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

googleJwkUrl :: String
googleJwkUrl =
  "https://www.googleapis.com/service_accounts/v1/jwk/securetoken@system.gserviceaccount.com"

defaultCacheDurationSeconds :: NominalDiffTime
defaultCacheDurationSeconds = 3600

firebaseIssuerPrefix :: Text
firebaseIssuerPrefix = "https://securetoken.google.com/"

expectedAlgorithm :: Text
expectedAlgorithm = "RS256"

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
  keysResult <- fetchGoogleKeys mgr
  case keysResult of
    Left err -> pure (Left err)
    Right (jwks, _expiry) -> verifyWithKeys jwks config token

-- ---------------------------------------------------------------------------
-- Cached verification
-- ---------------------------------------------------------------------------

-- | Create a key cache backed by the given HTTP manager.
newKeyCache :: Manager -> IO KeyCache
newKeyCache mgr = do
  epoch <- getCurrentTime
  ref <- newTVarIO (JwkSet [], epoch)
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
  now <- getCurrentTime
  (jwks, expiry) <- readTVarIO (kcKeysRef cache)
  keysResult <-
    if now >= expiry
      then do
        result <- fetchGoogleKeys (kcManager cache)
        case result of
          Left err -> pure (Left err)
          Right (newKeys, newExpiry) -> do
            atomically $ writeTVar (kcKeysRef cache) (newKeys, newExpiry)
            pure (Right newKeys)
      else pure (Right jwks)
  case keysResult of
    Left err -> pure (Left err)
    Right keys -> verifyWithKeys keys config token

-- ---------------------------------------------------------------------------
-- Key fetching
-- ---------------------------------------------------------------------------

fetchGoogleKeys :: Manager -> IO (Either AuthError (JwkSet, UTCTime))
fetchGoogleKeys mgr = do
  req <- parseRequest googleJwkUrl
  respResult <- try (httpLbs req mgr) :: IO (Either SomeException (Response LBS.ByteString))
  case respResult of
    Left err -> pure (Left (KeyFetchError (T.pack (show err))))
    Right resp -> do
      now <- getCurrentTime
      let body = responseBody resp
          maxAge = parseCacheMaxAge (responseHeaders resp)
          duration = maybe defaultCacheDurationSeconds fromIntegral maxAge
          expiry = addUTCTime duration now
      case Aeson.eitherDecode body of
        Left err -> pure (Left (KeyFetchError (T.pack err)))
        Right jwks -> do
          !_ <- evaluate jwks
          pure (Right (jwks, expiry))

-- | Parse the @max-age@ directive from a @Cache-Control@ response header.
--
-- >>> parseCacheMaxAge [("cache-control", "public, max-age=19845, must-revalidate")]
-- Just 19845
parseCacheMaxAge :: ResponseHeaders -> Maybe Int
parseCacheMaxAge headers = do
  cc <- lookup hCacheControl headers
  let (_before, rest) = BS.breakSubstring "max-age=" cc
  if BS.null rest
    then Nothing
    else do
      let numPart = BS.drop maxAgePrefixLen rest
      case BS8.readInt numPart of
        Just (n, _) | n > 0 -> Just n
        _ -> Nothing
  where
    maxAgePrefixLen :: Int
    maxAgePrefixLen = 8

-- ---------------------------------------------------------------------------
-- JWT verification
-- ---------------------------------------------------------------------------

verifyWithKeys ::
  JwkSet ->
  FirebaseConfig ->
  BS.ByteString ->
  IO (Either AuthError FirebaseUser)
verifyWithKeys jwks config tokenBytes = do
  now <- getCurrentTime
  pure $ do
    (headerB64, payloadB64, sigBytes, header, payload) <- parseCompactJwt tokenBytes
    validateAlgorithm header
    pubKey <- findKeyByKid (jhKid header) jwks
    verifySignature pubKey (headerB64 <> "." <> payloadB64) sigBytes
    validateClaims config now payload
    extractUser payload

-- ---------------------------------------------------------------------------
-- JWT parsing
-- ---------------------------------------------------------------------------

parseCompactJwt ::
  BS.ByteString ->
  Either AuthError (BS.ByteString, BS.ByteString, BS.ByteString, JwtHeader, JwtPayload)
parseCompactJwt token =
  case BS8.split '.' token of
    [headerB64, payloadB64, sigB64] -> do
      headerBytes <- decodeSegment "header" headerB64
      payloadBytes <- decodeSegment "payload" payloadB64
      sigBytes <- decodeSegment "signature" sigB64
      header <- decodeJson "header" headerBytes
      payload <- decodeJson "payload" payloadBytes
      Right (headerB64, payloadB64, sigBytes, header, payload)
    _ -> Left (MalformedToken "expected 3 dot-separated parts")

decodeSegment :: Text -> BS.ByteString -> Either AuthError BS.ByteString
decodeSegment label input =
  case B64URL.decode (padBase64Url input) of
    Left err -> Left (MalformedToken (label <> ": " <> T.pack err))
    Right bs -> Right bs

decodeJson :: (FromJSON a) => Text -> BS.ByteString -> Either AuthError a
decodeJson label bytes =
  case Aeson.eitherDecodeStrict bytes of
    Left err -> Left (MalformedToken (label <> ": " <> T.pack err))
    Right val -> Right val

-- ---------------------------------------------------------------------------
-- Signature verification
-- ---------------------------------------------------------------------------

validateAlgorithm :: JwtHeader -> Either AuthError ()
validateAlgorithm header
  | jhAlg header == expectedAlgorithm = Right ()
  | otherwise = Left (MalformedToken ("unsupported algorithm: " <> jhAlg header))

findKeyByKid :: Text -> JwkSet -> Either AuthError PublicKey
findKeyByKid kid (JwkSet keys) =
  case filter (\k -> jkKid k == kid) keys of
    (matched : _) -> Right (jkKey matched)
    [] -> Left InvalidSignature

verifySignature :: PublicKey -> BS.ByteString -> BS.ByteString -> Either AuthError ()
verifySignature pubKey signedData sigBytes
  | verify (Just SHA256) pubKey signedData sigBytes = Right ()
  | otherwise = Left InvalidSignature

-- ---------------------------------------------------------------------------
-- Claims validation
-- ---------------------------------------------------------------------------

validateClaims :: FirebaseConfig -> UTCTime -> JwtPayload -> Either AuthError ()
validateClaims config now payload = do
  let projectId = fcProjectId config
      expectedIssuer = firebaseIssuerPrefix <> projectId
      skew = fcClockSkew config
  requireClaim "iss" (jpIss payload) (== expectedIssuer) (InvalidClaims "issuer mismatch")
  requireClaim "aud" (jpAud payload) (== projectId) (InvalidClaims "audience mismatch")
  requireExpiry skew now (jpExp payload)
  requireIssuedAt skew now (jpIat payload)

requireClaim :: Text -> Maybe Text -> (Text -> Bool) -> AuthError -> Either AuthError ()
requireClaim label mVal predicate err =
  case mVal of
    Just val | predicate val -> Right ()
    Just _ -> Left err
    Nothing -> Left (InvalidClaims ("missing " <> label <> " claim"))

requireExpiry :: NominalDiffTime -> UTCTime -> Maybe Integer -> Either AuthError ()
requireExpiry _skew _now Nothing = Left (InvalidClaims "missing exp claim")
requireExpiry skew now (Just expSeconds)
  | addUTCTime skew expTime >= now = Right ()
  | otherwise = Left TokenExpired
  where
    expTime = posixSecondsToUTCTime (fromInteger expSeconds)

requireIssuedAt :: NominalDiffTime -> UTCTime -> Maybe Integer -> Either AuthError ()
requireIssuedAt _skew _now Nothing = Left (InvalidClaims "missing iat claim")
requireIssuedAt skew now (Just iatSeconds)
  | diffUTCTime iatTime now <= skew = Right ()
  | otherwise = Left (InvalidClaims "token issued in the future")
  where
    iatTime = posixSecondsToUTCTime (fromInteger iatSeconds)

-- ---------------------------------------------------------------------------
-- User extraction
-- ---------------------------------------------------------------------------

extractUser :: JwtPayload -> Either AuthError FirebaseUser
extractUser payload =
  case jpSub payload of
    Just sub
      | not (T.null sub) ->
          Right FirebaseUser {fuUid = sub, fuEmail = jpEmail payload, fuName = jpName payload}
    Just _ -> Left (InvalidClaims "empty sub claim")
    Nothing -> Left (InvalidClaims "missing sub claim")
