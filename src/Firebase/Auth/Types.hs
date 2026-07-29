-- |
-- Module      : Firebase.Auth.Types
-- Description : Types for Firebase JWT verification
-- License     : BSD-3-Clause
--
-- Core data types for Firebase ID token verification: configuration,
-- authenticated user, error reporting, and key caching.
module Firebase.Auth.Types
  ( -- * Configuration
    FirebaseConfig (..),
    defaultFirebaseConfig,

    -- * Authenticated User
    FirebaseUser (..),

    -- * Errors
    AuthError (..),
    authErrorMessage,

    -- * Key Cache
    -- $keycache
    KeyCache (..),

    -- * JWK Types
    JwkKey (..),
    JwkSet (..),
  )
where

import Crypto.PubKey.RSA.Types (PublicKey (..))
import Data.Aeson (FromJSON (..), Object, withObject, (.:))
import Data.Aeson.Types (Parser)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (NominalDiffTime, UTCTime)
import Firebase.Auth.Internal (padBase64Url)
import Network.HTTP.Client (Manager)

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | Configuration for Firebase ID token verification.
data FirebaseConfig = FirebaseConfig
  { -- | Firebase project ID (e.g. @\"gondola-bros-hub\"@).
    fcProjectId :: !Text,
    -- | Maximum allowed clock skew between server and Google.
    -- Default: 300 seconds.
    fcClockSkew :: !NominalDiffTime
  }

-- | Default clock skew allowance: 300 seconds.
defaultClockSkewSeconds :: NominalDiffTime
defaultClockSkewSeconds = 300

-- | Create a t'FirebaseConfig' with a 300-second clock skew allowance.
defaultFirebaseConfig ::
  -- | Firebase project ID
  Text ->
  FirebaseConfig
defaultFirebaseConfig projectId =
  FirebaseConfig
    { fcProjectId = projectId,
      fcClockSkew = defaultClockSkewSeconds
    }

-- ---------------------------------------------------------------------------
-- Authenticated user
-- ---------------------------------------------------------------------------

-- | An authenticated Firebase user, extracted from a verified ID token.
data FirebaseUser = FirebaseUser
  { -- | Firebase UID (the token's @sub@ claim).
    fuUid :: !Text,
    -- | Email address, if present in token claims.
    fuEmail :: !(Maybe Text),
    -- | Display name, if present in token claims.
    fuName :: !(Maybe Text)
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Errors
-- ---------------------------------------------------------------------------

-- | Errors that can occur during token verification.
data AuthError
  = -- | Failed to fetch Google's public keys.
    KeyFetchError !Text
  | -- | JWT signature does not match any Google public key.
    InvalidSignature
  | -- | Token has expired (past @exp@ claim minus allowed skew).
    TokenExpired
  | -- | Token claims are invalid (wrong issuer, audience, empty subject, etc.).
    InvalidClaims !Text
  | -- | Token is not valid compact JWT serialization.
    MalformedToken !Text
  deriving (Eq, Show)

-- | Render an 'AuthError' as a message safe to return to a client.
--
-- The 'Text' payloads carried by 'KeyFetchError', 'InvalidClaims', and
-- 'MalformedToken' name internal specifics, so they are deliberately
-- dropped here: they belong in your logs, not in a 401 body.
authErrorMessage :: AuthError -> LBS.ByteString
authErrorMessage (KeyFetchError _) = "Authentication service unavailable"
authErrorMessage InvalidSignature = "Invalid token signature"
authErrorMessage TokenExpired = "Token expired"
authErrorMessage (InvalidClaims _) = "Invalid token claims"
authErrorMessage (MalformedToken _) = "Malformed token"

-- ---------------------------------------------------------------------------
-- Key cache
-- ---------------------------------------------------------------------------

-- $keycache
-- The cache is created by "Firebase.Auth", which re-exports t'KeyCache' as an
-- abstract type. Its fields are visible here only so the verifier can reach
-- them, and are not part of the supported API.

-- | Cached store of Google's public JWKs.
--
-- Create with 'Firebase.Auth.newKeyCache' or 'Firebase.Auth.newTlsKeyCache'.
-- Keys are refreshed automatically when expired (per Google's
-- @Cache-Control: max-age@ header). Safe to share across threads: updates go
-- through 'Data.IORef.atomicModifyIORef''.
data KeyCache = KeyCache
  { -- | Cached JWK set paired with the instant it stops being valid.
    kcKeysRef :: !(IORef (JwkSet, UTCTime)),
    -- | HTTP manager for fetching keys from Google.
    kcManager :: !Manager
  }

-- ---------------------------------------------------------------------------
-- JWK types
-- ---------------------------------------------------------------------------

-- | An RSA public key from Google's JWK set, tagged with its key ID.
data JwkKey = JwkKey
  { -- | Key ID (@kid@ field), used to match against the JWT header.
    jkKid :: !Text,
    -- | RSA public key for signature verification.
    jkKey :: !PublicKey
  }

-- | A set of JWK keys fetched from Google's public key endpoint.
newtype JwkSet = JwkSet {unJwkSet :: [JwkKey]}

instance FromJSON JwkSet where
  parseJSON = withObject "JwkSet" $ \o ->
    JwkSet <$> o .: "keys"

instance FromJSON JwkKey where
  parseJSON = withObject "JwkKey" $ \o -> do
    kid <- o .: "kid"
    kty <- o .: "kty" :: Parser Text
    case kty of
      "RSA" -> parseRsaKey kid o
      other -> fail ("unsupported key type: " ++ T.unpack other)

-- ---------------------------------------------------------------------------
-- JWK internal helpers
-- ---------------------------------------------------------------------------

parseRsaKey :: Text -> Object -> Parser JwkKey
parseRsaKey kid o = do
  nB64 <- o .: "n"
  eB64 <- o .: "e"
  case (,) <$> decodeBase64Url nB64 <*> decodeBase64Url eB64 of
    Left err -> fail err
    Right (modulus, publicExponent) ->
      pure $
        JwkKey
          kid
          ( PublicKey
              (BS.length modulus)
              (bsToInteger modulus)
              (bsToInteger publicExponent)
          )

-- | Decode a base64url-encoded text value, restoring its padding first.
decodeBase64Url :: Text -> Either String BS.ByteString
decodeBase64Url = B64URL.decode . padBase64Url . TE.encodeUtf8

-- | Number of distinct values a byte can take, the radix of a big-endian
-- byte string read as an integer.
byteRadix :: Integer
byteRadix = 256

-- | Convert a big-endian unsigned 'BS.ByteString' to an 'Integer'.
bsToInteger :: BS.ByteString -> Integer
bsToInteger = BS.foldl' (\acc byte -> acc * byteRadix + fromIntegral byte) 0
