{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : Firebase.Auth.Types
-- Description : Types for Firebase JWT verification
-- License     : MIT
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

    -- * Key Cache
    KeyCache (..),

    -- * JWK Types
    JwkKey (..),
    JwkSet (..),

    -- * Internal (re-exported for Firebase.Auth)
    padBase64Url,
  )
where

import Control.Concurrent.STM (TVar)
import Crypto.PubKey.RSA.Types (PublicKey (..))
import Data.Aeson (FromJSON (..), Object, withObject, (.:))
import Data.Aeson.Types (Parser)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64URL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (NominalDiffTime, UTCTime)
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

-- | Create a 'FirebaseConfig' with a 300-second clock skew allowance.
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

-- ---------------------------------------------------------------------------
-- Key cache
-- ---------------------------------------------------------------------------

-- | Cached store of Google's public JWKs.
--
-- Create with 'Firebase.Auth.newKeyCache'. Keys are refreshed automatically
-- when expired (per Google's @Cache-Control: max-age@ header).
-- Thread-safe via STM — concurrent verifications compose atomically.
data KeyCache = KeyCache
  { -- | Cached JWK set and its expiry time (STM for atomic concurrent access).
    kcKeysRef :: !(TVar (JwkSet, UTCTime)),
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
    Right (nBytes, eBytes) ->
      let keySize = BS.length nBytes
       in pure $ JwkKey kid (PublicKey keySize (bsToInteger nBytes) (bsToInteger eBytes))

-- | Decode a base64url-encoded text value, adding padding as needed.
decodeBase64Url :: Text -> Either String BS.ByteString
decodeBase64Url = B64URL.decode . padBase64Url . TE.encodeUtf8

-- | Add padding to a base64url-encoded string (JWK/JWT use unpadded base64url).
padBase64Url :: BS.ByteString -> BS.ByteString
padBase64Url bs =
  let remainder = BS.length bs `mod` 4
   in if remainder == 0
        then bs
        else bs <> BS.replicate (4 - remainder) 0x3d -- '='

-- | Convert a big-endian unsigned 'BS.ByteString' to an 'Integer'.
bsToInteger :: BS.ByteString -> Integer
bsToInteger = BS.foldl' (\acc w -> acc * 256 + fromIntegral w) 0
