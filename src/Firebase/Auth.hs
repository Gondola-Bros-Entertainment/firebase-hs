{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : Firebase.Auth
-- Description : Firebase ID token verification
-- License     : MIT
--
-- Verify Firebase Authentication ID tokens (JWTs) against Google's
-- public keys. Supports both one-shot verification and cached key
-- stores for production servers.
--
-- @
-- import Firebase.Auth
-- import Network.HTTP.Client.TLS (newTlsManager)
--
-- main :: IO ()
-- main = do
--   mgr   <- newTlsManager
--   cache <- newKeyCache mgr
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

import Control.Lens (view, (&), (.~))
import Control.Monad.Trans.Except (ExceptT, runExceptT, withExceptT)
import Crypto.JOSE.Error (Error)
import Crypto.JOSE.JWK (JWKSet (..))
import Crypto.JWT
  ( ClaimsSet,
    HasClaimsSet (..),
    JWTError (..),
    JWTValidationSettings,
    SignedJWT,
    StringOrURI,
    claimSub,
    decodeCompact,
    defaultJWTValidationSettings,
    jwtValidationSettingsAllowedSkew,
    jwtValidationSettingsIssuerPredicate,
    verifyJWT,
  )
import Data.Aeson (FromJSON (..), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.String (fromString)
import qualified Data.Text as T
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Firebase.Auth.Types
import Network.HTTP.Client
  ( Manager,
    httpLbs,
    parseRequest,
    responseBody,
    responseHeaders,
  )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Header (ResponseHeaders, hCacheControl)

-- ---------------------------------------------------------------------------
-- Firebase JWT claims (jose subtype pattern)
-- ---------------------------------------------------------------------------

-- | JWT claims carrying both the standard 'ClaimsSet' and Firebase-specific
-- custom fields. This is the jose-recommended subtype approach: 'verifyJWT'
-- decodes and validates everything in one pass, with the types driving
-- which fields get extracted.
data FirebaseClaims = FirebaseClaims
  { _fcClaimsSet :: !ClaimsSet,
    _fcEmail :: !(Maybe T.Text),
    _fcName :: !(Maybe T.Text)
  }

instance HasClaimsSet FirebaseClaims where
  claimsSet f (FirebaseClaims cs e n) =
    (\cs' -> FirebaseClaims cs' e n) <$> f cs

instance FromJSON FirebaseClaims where
  parseJSON = Aeson.withObject "FirebaseClaims" $ \o ->
    FirebaseClaims
      <$> parseJSON (Aeson.Object o)
      <*> o .:? "email"
      <*> o .:? "name"

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

-- | Google's JWK endpoint for Firebase token verification.
googleJwkUrl :: String
googleJwkUrl =
  "https://www.googleapis.com/service_accounts/v1/jwk/securetoken@system.gserviceaccount.com"

-- | Fallback cache duration when @Cache-Control@ header is missing (1 hour).
defaultCacheDurationSeconds :: NominalDiffTime
defaultCacheDurationSeconds = 3600

-- | Firebase issuer URL prefix. Full issuer is this plus the project ID.
firebaseIssuerPrefix :: T.Text
firebaseIssuerPrefix = "https://securetoken.google.com/"

-- ---------------------------------------------------------------------------
-- One-shot verification
-- ---------------------------------------------------------------------------

-- | Verify a Firebase ID token, fetching Google's public keys fresh.
--
-- For production servers handling many requests, prefer 'verifyIdTokenCached'
-- to avoid re-fetching keys on every call.
verifyIdToken ::
  Manager ->
  FirebaseConfig ->
  -- | Raw JWT bytes (compact serialization)
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
--
-- The manager must support HTTPS (e.g. from @Network.HTTP.Client.TLS@).
-- Keys are fetched lazily on first verification and refreshed when expired.
newKeyCache :: Manager -> IO KeyCache
newKeyCache mgr = do
  epoch <- getCurrentTime
  ref <- newIORef (JWKSet [], epoch)
  pure
    KeyCache
      { kcKeysRef = ref,
        kcManager = mgr
      }

-- | Create a key cache with a fresh TLS-enabled HTTP manager.
--
-- Convenience wrapper around 'newKeyCache' for the common case.
newTlsKeyCache :: IO KeyCache
newTlsKeyCache = newTlsManager >>= newKeyCache

-- | Verify a Firebase ID token using cached public keys.
--
-- Keys are refreshed automatically when the cache expires (per Google's
-- @Cache-Control: max-age@ response header).
verifyIdTokenCached ::
  KeyCache ->
  FirebaseConfig ->
  -- | Raw JWT bytes (compact serialization)
  BS.ByteString ->
  IO (Either AuthError FirebaseUser)
verifyIdTokenCached cache config token = do
  now <- getCurrentTime
  (jwks, expiry) <- readIORef (kcKeysRef cache)
  keysResult <-
    if now >= expiry
      then do
        result <- fetchGoogleKeys (kcManager cache)
        case result of
          Left err -> pure (Left err)
          Right (newKeys, newExpiry) -> do
            writeIORef (kcKeysRef cache) (newKeys, newExpiry)
            pure (Right newKeys)
      else pure (Right jwks)
  case keysResult of
    Left err -> pure (Left err)
    Right keys -> verifyWithKeys keys config token

-- ---------------------------------------------------------------------------
-- Key fetching
-- ---------------------------------------------------------------------------

-- | Fetch Google's public JWK set and compute the cache expiry from the
-- @Cache-Control: max-age@ response header.
fetchGoogleKeys :: Manager -> IO (Either AuthError (JWKSet, UTCTime))
fetchGoogleKeys mgr = do
  req <- parseRequest googleJwkUrl
  resp <- httpLbs req mgr
  now <- getCurrentTime
  let body = responseBody resp
      maxAge = parseCacheMaxAge (responseHeaders resp)
      duration = maybe defaultCacheDurationSeconds fromIntegral maxAge
      expiry = addUTCTime duration now
  case Aeson.eitherDecode body of
    Left err -> pure (Left (KeyFetchError (T.pack err)))
    Right jwks -> pure (Right (jwks, expiry))

-- | Parse the @max-age@ directive from a @Cache-Control@ response header.
--
-- Returns 'Nothing' if the header is absent or has no valid @max-age@ value.
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

-- | Verify a JWT against the given JWK set and Firebase configuration.
--
-- Uses jose's polymorphic 'verifyJWT' with our 'FirebaseClaims' subtype,
-- so signature verification, claims validation, and custom field extraction
-- all happen in a single type-directed pass.
verifyWithKeys ::
  JWKSet ->
  FirebaseConfig ->
  BS.ByteString ->
  IO (Either AuthError FirebaseUser)
verifyWithKeys jwks config tokenBytes = do
  let token = LBS.fromStrict tokenBytes
      projectId = fcProjectId config
      expectedIssuer = firebaseIssuerPrefix <> projectId
      settings = makeValidationSettings projectId expectedIssuer (fcClockSkew config)
  result <- runExceptT $ do
    jwt <-
      withExceptT
        mapDecodeError
        (decodeCompact token :: ExceptT Error IO SignedJWT)
    withExceptT
      mapVerifyError
      (verifyJWT settings jwks jwt :: ExceptT JWTError IO FirebaseClaims)
  pure $ case result of
    Left err -> Left err
    Right fc -> toFirebaseUser fc

-- | Build JWT validation settings for Firebase token verification.
--
-- Validates: audience = project ID, issuer = Firebase issuer URL,
-- expiry and issued-at within allowed clock skew.
makeValidationSettings ::
  T.Text -> T.Text -> NominalDiffTime -> JWTValidationSettings
makeValidationSettings projectId expectedIssuer skew =
  defaultJWTValidationSettings (== textToStringOrURI projectId)
    & jwtValidationSettingsIssuerPredicate .~ (== textToStringOrURI expectedIssuer)
    & jwtValidationSettingsAllowedSkew .~ skew

-- | Convert verified 'FirebaseClaims' to a 'FirebaseUser'.
--
-- The @sub@ claim is guaranteed present by Firebase's token contract,
-- but we handle the missing case defensively.
toFirebaseUser :: FirebaseClaims -> Either AuthError FirebaseUser
toFirebaseUser fc = do
  sub <- maybe (Left (InvalidClaims "missing sub claim")) Right (view claimSub fc)
  uid <- maybe (Left (InvalidClaims "empty sub claim")) Right (stringOrUriToText sub)
  Right
    FirebaseUser
      { fuUid = uid,
        fuEmail = _fcEmail fc,
        fuName = _fcName fc
      }

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Convert 'Data.Text.Text' to jose 'StringOrURI' via the 'Data.String.IsString' instance.
textToStringOrURI :: T.Text -> StringOrURI
textToStringOrURI = fromString . T.unpack

-- | Extract text from a jose 'StringOrURI' by round-tripping through JSON.
stringOrUriToText :: StringOrURI -> Maybe T.Text
stringOrUriToText s = case Aeson.toJSON s of
  Aeson.String t | not (T.null t) -> Just t
  _ -> Nothing

-- | Map a JOSE decode error to 'AuthError'.
mapDecodeError :: Error -> AuthError
mapDecodeError = MalformedToken . T.pack . show

-- | Map a JWT verification error to 'AuthError'.
mapVerifyError :: JWTError -> AuthError
mapVerifyError JWTExpired = TokenExpired
mapVerifyError JWTNotInAudience = InvalidClaims "audience mismatch"
mapVerifyError JWTNotInIssuer = InvalidClaims "issuer mismatch"
mapVerifyError JWTIssuedAtFuture = InvalidClaims "token issued in the future"
mapVerifyError JWTNotYetValid = InvalidClaims "token not yet valid"
mapVerifyError (JWTClaimsSetDecodeError msg) = MalformedToken (T.pack msg)
mapVerifyError (JWSError _) = InvalidSignature
