-- |
-- Module      : Firebase.Auth.WAI
-- Description : WAI middleware for Firebase authentication
-- License     : BSD-3-Clause
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
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vault.Lazy as Vault
import Firebase.Auth (FirebaseConfig, FirebaseUser, KeyCache, authErrorMessage, verifyIdTokenCached)
import Firebase.Auth.Internal (bearerChallenge, bearerToken)
import Network.HTTP.Types.Header (hContentType, hWWWAuthenticate)
import Network.HTTP.Types.Status (status401)
import Network.Wai
  ( Middleware,
    Request,
    Response,
    ResponseReceived,
    requestHeaders,
    responseLBS,
    vault,
  )
import System.IO.Unsafe (unsafePerformIO)

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

-- | Media type of the plain-text bodies this middleware returns.
textPlain :: BS.ByteString
textPlain = "text/plain; charset=utf-8"

-- | Response body when no bearer token was supplied at all.
missingHeaderMessage :: LBS.ByteString
missingHeaderMessage = "Missing or malformed Authorization header"

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
requireAuth cache cfg app req respond =
  withVerifiedUser cache cfg req respond (const (app req respond))

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
firebaseAuth cache cfg app req respond =
  withVerifiedUser cache cfg req respond $ \user ->
    app req {vault = Vault.insert firebaseUserKey user (vault req)} respond

-- | Look up the authenticated 'FirebaseUser' from a WAI request vault.
--
-- Returns 'Just' when the request has passed through 'firebaseAuth'.
lookupFirebaseUser :: Request -> Maybe FirebaseUser
lookupFirebaseUser = Vault.lookup firebaseUserKey . vault

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Verify the request's bearer token, answering 401 on any failure and
-- handing the authenticated user to the continuation otherwise.
--
-- Both middlewares share this, so they cannot drift apart on which requests
-- they admit or what they say when they refuse.
withVerifiedUser ::
  KeyCache ->
  FirebaseConfig ->
  Request ->
  (Response -> IO ResponseReceived) ->
  (FirebaseUser -> IO ResponseReceived) ->
  IO ResponseReceived
withVerifiedUser cache cfg req respond onVerified =
  case bearerToken (requestHeaders req) of
    Nothing -> respond (unauthorized missingHeaderMessage)
    Just token -> do
      result <- verifyIdTokenCached cache cfg token
      either (respond . unauthorized . authErrorMessage) onVerified result

-- | A 401 response carrying a plain-text explanation and the
-- @WWW-Authenticate@ challenge RFC 6750 requires.
unauthorized :: LBS.ByteString -> Response
unauthorized =
  responseLBS
    status401
    [(hContentType, textPlain), (hWWWAuthenticate, bearerChallenge)]
