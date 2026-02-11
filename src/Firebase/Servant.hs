{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : Firebase.Servant
-- Description : Servant auth combinator for Firebase
-- License     : MIT
--
-- One-liner Firebase authentication for Servant servers. Use
-- 'firebaseAuthHandler' to create an 'AuthHandler' that verifies
-- Firebase ID tokens from the @Authorization: Bearer \<token\>@ header.
--
-- @
-- import Firebase.Auth (newTlsKeyCache, defaultFirebaseConfig)
-- import Firebase.Servant (firebaseAuthHandler)
-- import Servant.Server (Context (..))
--
-- main :: IO ()
-- main = do
--   cache <- newTlsKeyCache
--   let cfg     = defaultFirebaseConfig \"my-project-id\"
--       ctx     = firebaseAuthHandler cache cfg :. EmptyContext
--   runSettings defaultSettings (serveWithContext api ctx server)
-- @
module Firebase.Servant
  ( -- * Auth Handler
    firebaseAuthHandler,

    -- * Helpers (pure, testable)
    extractBearerToken,
    authErrorToBody,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Firebase.Auth (verifyIdTokenCached)
import Firebase.Auth.Types (AuthError (..), FirebaseConfig, FirebaseUser, KeyCache)
import Network.Wai (Request, requestHeaders)
import Servant.Server (Handler, err401, errBody, throwError)
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

-- | The @\"Bearer \"@ prefix length (7 bytes).
bearerPrefixLen :: Int
bearerPrefixLen = 7

-- ---------------------------------------------------------------------------
-- Auth Handler
-- ---------------------------------------------------------------------------

-- | Create a Servant 'AuthHandler' that verifies Firebase ID tokens.
--
-- Extracts the Bearer token from the @Authorization@ header, verifies it
-- against Google's public keys using the cached key store, and returns
-- the authenticated 'FirebaseUser'.
--
-- On failure, returns HTTP 401 with a descriptive error body.
firebaseAuthHandler ::
  KeyCache -> FirebaseConfig -> AuthHandler Request FirebaseUser
firebaseAuthHandler cache cfg = mkAuthHandler $ \req ->
  case extractBearerToken req of
    Nothing -> throw401 "Missing or malformed Authorization header"
    Just tok -> do
      result <- liftIO (verifyIdTokenCached cache cfg tok)
      either (throw401 . authErrorToBody) pure result

-- | Throw a 401 error in the Servant 'Handler' monad.
throw401 :: LBS.ByteString -> Handler a
throw401 msg = throwError (err401 {errBody = msg})

-- ---------------------------------------------------------------------------
-- Pure Helpers
-- ---------------------------------------------------------------------------

-- | Extract a Bearer token from a WAI 'Request'.
--
-- Looks for the @Authorization@ header and strips the @\"Bearer \"@ prefix.
-- Returns 'Nothing' if the header is missing or doesn't start with @\"Bearer \"@.
extractBearerToken :: Request -> Maybe BS.ByteString
extractBearerToken req = do
  hdr <- lookup "Authorization" (requestHeaders req)
  if "Bearer " `BS.isPrefixOf` hdr
    then Just (BS.drop bearerPrefixLen hdr)
    else Nothing

-- | Convert an 'AuthError' to a human-readable lazy bytestring for error bodies.
authErrorToBody :: AuthError -> LBS.ByteString
authErrorToBody (KeyFetchError msg) = "Key fetch error: " <> textToLBS msg
authErrorToBody InvalidSignature = "Invalid token signature"
authErrorToBody TokenExpired = "Token expired"
authErrorToBody (InvalidClaims msg) = "Invalid claims: " <> textToLBS msg
authErrorToBody (MalformedToken msg) = "Malformed token: " <> textToLBS msg

-- | Encode 'Text' to a lazy UTF-8 'LBS.ByteString'.
textToLBS :: T.Text -> LBS.ByteString
textToLBS = LBS.fromStrict . encodeUtf8
