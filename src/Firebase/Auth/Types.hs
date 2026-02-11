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
  )
where

import Crypto.JOSE.JWK (JWKSet)
import Data.IORef (IORef)
import Data.Text (Text)
import Data.Time (NominalDiffTime, UTCTime)
import Network.HTTP.Client (Manager)

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

-- | Cached store of Google's public JWKs.
--
-- Create with 'Firebase.Auth.newKeyCache'. Keys are refreshed automatically
-- when expired (per Google's @Cache-Control: max-age@ header).
data KeyCache = KeyCache
  { -- | Cached JWK set and its expiry time.
    kcKeysRef :: !(IORef (JWKSet, UTCTime)),
    -- | HTTP manager for fetching keys from Google.
    kcManager :: !Manager
  }
