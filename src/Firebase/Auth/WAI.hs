{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : Firebase.Auth.WAI
-- Description : WAI middleware for Firebase authentication
-- License     : MIT
--
-- Firebase authentication middleware for any WAI-based web server.
-- Works with Warp, Scotty, Yesod, Spock, and any other framework
-- built on WAI.
--
-- @
-- import Firebase.Auth (newTlsKeyCache, defaultFirebaseConfig)
-- import Firebase.Auth.WAI (firebaseAuth, requireAuth)
-- import Network.Wai.Handler.Warp (run)
--
-- main :: IO ()
-- main = do
--   cache <- newTlsKeyCache
--   let cfg = defaultFirebaseConfig \"my-project-id\"
--   run 3000 $ requireAuth cache cfg myApp
-- @
module Firebase.Auth.WAI
  ( -- * Middleware
    requireAuth,

    -- * Vault-based (advanced)
    firebaseAuth,
    firebaseUserKey,
    lookupFirebaseUser,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Vault.Lazy as Vault
import Firebase.Auth (verifyIdTokenCached)
import Firebase.Auth.Types (AuthError (..), FirebaseConfig, FirebaseUser, KeyCache)
import Network.HTTP.Types.Status (status401)
import Network.Wai (Middleware, Request, Response, requestHeaders, responseLBS, vault)
import System.IO.Unsafe (unsafePerformIO)

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

-- | The @\"Bearer \"@ prefix length (7 bytes).
bearerPrefixLen :: Int
bearerPrefixLen = 7

-- ---------------------------------------------------------------------------
-- Simple Middleware
-- ---------------------------------------------------------------------------

-- | Middleware that requires a valid Firebase ID token on every request.
--
-- Requests without a valid @Authorization: Bearer \<token\>@ header receive
-- a 401 response and never reach the wrapped application.
--
-- @
-- main = run 3000 $ requireAuth cache cfg myApp
-- @
requireAuth :: KeyCache -> FirebaseConfig -> Middleware
requireAuth cache cfg app req respond = do
  case extractBearer req of
    Nothing -> respond unauthorizedResponse
    Just tok -> do
      result <- verifyIdTokenCached cache cfg tok
      case result of
        Left err -> respond (errorResponse err)
        Right _user -> app req respond

-- ---------------------------------------------------------------------------
-- Vault-based Middleware (advanced)
-- ---------------------------------------------------------------------------

-- | Global vault key for storing the authenticated 'FirebaseUser'.
--
-- Uses @unsafePerformIO@ per the @vault@ library's documented pattern
-- for creating global keys.
firebaseUserKey :: Vault.Key FirebaseUser
firebaseUserKey = unsafePerformIO Vault.newKey
{-# NOINLINE firebaseUserKey #-}

-- | Middleware that verifies Firebase tokens and stores the authenticated
-- user in the WAI request vault.
--
-- Downstream handlers can retrieve the user with 'lookupFirebaseUser'.
-- Requests without valid tokens receive a 401 response.
--
-- @
-- main = run 3000 $ firebaseAuth cache cfg myApp
--
-- myHandler :: Request -> IO Response
-- myHandler req = case lookupFirebaseUser req of
--   Just user -> ...  -- authenticated
--   Nothing   -> ...  -- should not happen (middleware rejects first)
-- @
firebaseAuth :: KeyCache -> FirebaseConfig -> Middleware
firebaseAuth cache cfg app req respond = do
  case extractBearer req of
    Nothing -> respond unauthorizedResponse
    Just tok -> do
      result <- verifyIdTokenCached cache cfg tok
      case result of
        Left err -> respond (errorResponse err)
        Right user ->
          let req' = req {vault = Vault.insert firebaseUserKey user (vault req)}
           in app req' respond

-- | Look up the authenticated 'FirebaseUser' from a WAI request vault.
--
-- Returns 'Just' when the request has passed through 'firebaseAuth'.
lookupFirebaseUser :: Request -> Maybe FirebaseUser
lookupFirebaseUser = Vault.lookup firebaseUserKey . vault

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Extract a Bearer token from a request's Authorization header.
extractBearer :: Request -> Maybe BS.ByteString
extractBearer req = do
  hdr <- lookup "Authorization" (requestHeaders req)
  if "Bearer " `BS.isPrefixOf` hdr
    then Just (BS.drop bearerPrefixLen hdr)
    else Nothing

-- | 401 response for missing or invalid tokens.
unauthorizedResponse :: Response
unauthorizedResponse =
  responseLBS
    status401
    [("Content-Type", "text/plain")]
    "Missing or malformed Authorization header"

-- | 401 response with a description of the auth error.
errorResponse :: AuthError -> Response
errorResponse err =
  responseLBS
    status401
    [("Content-Type", "text/plain")]
    (authErrorMessage err)

-- | Convert an 'AuthError' to a response message.
authErrorMessage :: AuthError -> LBS8.ByteString
authErrorMessage (KeyFetchError _) = "Authentication service unavailable"
authErrorMessage InvalidSignature = "Invalid token signature"
authErrorMessage TokenExpired = "Token expired"
authErrorMessage (InvalidClaims _) = "Invalid token claims"
authErrorMessage (MalformedToken _) = "Malformed token"
