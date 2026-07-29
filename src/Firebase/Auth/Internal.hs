-- |
-- Module      : Firebase.Auth.Internal
-- Description : Encoding and header primitives shared across the auth layers
-- License     : BSD-3-Clause
--
-- Primitives shared by the JWT verifier and the WAI and Servant
-- integrations. Everything here is pure and depends only on the wire
-- formats, never on the authentication types, so each layer can use it
-- without pulling in the others.
module Firebase.Auth.Internal
  ( -- * Base64url
    padBase64Url,

    -- * Bearer tokens
    bearerToken,
    stripBearerPrefix,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (toLower)
import Data.Word (Word8)
import Network.HTTP.Types.Header (RequestHeaders, hAuthorization)

-- ---------------------------------------------------------------------------
-- Base64url
-- ---------------------------------------------------------------------------

-- | Base64 encodes three bytes per four characters, so a padded payload is
-- always a whole number of four-character groups.
base64GroupSize :: Int
base64GroupSize = 4

-- | ASCII @\'=\'@, the byte base64 pads a short final group with.
base64PadByte :: Word8
base64PadByte = 0x3d

-- | Restore the padding that JWT and JWK strip from their base64url payloads.
--
-- >>> padBase64Url "YWJjZA"
-- "YWJjZA=="
padBase64Url :: BS.ByteString -> BS.ByteString
padBase64Url payload
  | remainder == 0 = payload
  | otherwise = payload <> BS.replicate (base64GroupSize - remainder) base64PadByte
  where
    remainder = BS.length payload `rem` base64GroupSize

-- ---------------------------------------------------------------------------
-- Bearer tokens
-- ---------------------------------------------------------------------------

-- | The @Bearer@ authentication scheme, including the space that separates
-- it from the credentials.
bearerPrefix :: BS.ByteString
bearerPrefix = "Bearer "

-- | 'bearerPrefix' folded to lower case once, for case-insensitive matching.
bearerPrefixLower :: BS.ByteString
bearerPrefixLower = BS8.map toLower bearerPrefix

-- | Extract a bearer token from a request's headers.
--
-- Takes the header list rather than a request so that both the WAI and the
-- Servant integrations can share it without either becoming a dependency of
-- the other.
bearerToken :: RequestHeaders -> Maybe BS.ByteString
bearerToken headers = lookup hAuthorization headers >>= stripBearerPrefix

-- | Strip the @\"Bearer \"@ prefix from an @Authorization@ header value.
--
-- The scheme name is matched case-insensitively, as RFC 7235 requires; the
-- credentials that follow are returned verbatim.
--
-- >>> stripBearerPrefix "bearer abc.def.ghi"
-- Just "abc.def.ghi"
stripBearerPrefix :: BS.ByteString -> Maybe BS.ByteString
stripBearerPrefix header
  | BS8.map toLower scheme == bearerPrefixLower = Just credentials
  | otherwise = Nothing
  where
    (scheme, credentials) = BS.splitAt (BS.length bearerPrefix) header
