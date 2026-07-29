-- |
-- Module      : Firebase.Servant
-- Description : Servant auth combinator for Firebase
-- License     : BSD-3-Clause
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
--
-- The token is extracted with 'Firebase.Auth.Internal.bearerToken' and
-- failures are rendered with 'Firebase.Auth.authErrorMessage', so this
-- combinator admits exactly the requests "Firebase.Auth.WAI" does and
-- refuses them with the same wording.
module Firebase.Servant
  ( -- * Auth Handler
    firebaseAuthHandler,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (throwE)
import qualified Data.ByteString.Lazy as LBS
import Firebase.Auth (FirebaseConfig, FirebaseUser, KeyCache, authErrorMessage, verifyIdTokenCached)
import Firebase.Auth.Internal (bearerToken)
import Network.Wai (Request, requestHeaders)
import Servant.Server (Handler (..), err401, errBody)
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

-- | Response body when no bearer token was supplied at all.
missingHeaderMessage :: LBS.ByteString
missingHeaderMessage = "Missing or malformed Authorization header"

-- ---------------------------------------------------------------------------
-- Auth Handler
-- ---------------------------------------------------------------------------

-- | Create a Servant 'AuthHandler' that verifies Firebase ID tokens.
--
-- Extracts the Bearer token from the @Authorization@ header, verifies it
-- against Google's public keys using the cached key store, and returns
-- the authenticated 'FirebaseUser'.
--
-- On failure, returns HTTP 401 with a body that names the failure without
-- disclosing which claim or key was at fault.
firebaseAuthHandler ::
  KeyCache -> FirebaseConfig -> AuthHandler Request FirebaseUser
firebaseAuthHandler cache cfg = mkAuthHandler $ \req ->
  case bearerToken (requestHeaders req) of
    Nothing -> throw401 missingHeaderMessage
    Just token -> do
      result <- liftIO (verifyIdTokenCached cache cfg token)
      either (throw401 . authErrorMessage) pure result

-- | Throw a 401 error in the Servant t'Handler' monad.
--
-- Built on the t'Handler' newtype rather than @MonadError@, which keeps this
-- working across servant versions without depending on @mtl@.
throw401 :: LBS.ByteString -> Handler a
throw401 msg = Handler (throwE err401 {errBody = msg})
