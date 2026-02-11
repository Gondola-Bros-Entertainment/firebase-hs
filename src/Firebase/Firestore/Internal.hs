{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : Firebase.Firestore.Internal
-- Description : Pure URL builders and request helpers for Firestore
-- License     : MIT
--
-- Internal utilities for constructing Firestore REST API URLs and requests.
-- All functions are pure and testable without IO.
module Firebase.Firestore.Internal
  ( -- * Constants
    firestoreBaseUrl,

    -- * URL Construction
    documentUrl,
    collectionUrl,
    createDocUrl,
    updateDocUrl,
    queryUrl,
    beginTransactionUrl,
    commitUrl,
    rollbackUrl,

    -- * Request Helpers
    authorizeRequest,

    -- * Response Parsing
    parseFirestoreError,
  )
where

import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Firebase.Firestore.Types
import Network.HTTP.Client (Request, requestHeaders)

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

-- | Firestore REST API base URL.
firestoreBaseUrl :: String
firestoreBaseUrl = "https://firestore.googleapis.com/v1"

-- | Firestore database path segment.
firestoreDatabasePath :: String
firestoreDatabasePath = "/databases/(default)/documents"

-- ---------------------------------------------------------------------------
-- URL Construction
-- ---------------------------------------------------------------------------

-- | URL for a specific document.
--
-- >>> documentUrl (ProjectId "p") (DocumentPath (CollectionPath "c") (DocumentId "d"))
-- "https://firestore.googleapis.com/v1/projects/p/databases/(default)/documents/c/d"
documentUrl :: ProjectId -> DocumentPath -> String
documentUrl pid dp =
  firestoreBaseUrl
    <> "/projects/"
    <> T.unpack (unProjectId pid)
    <> firestoreDatabasePath
    <> "/"
    <> T.unpack (unCollectionPath (dpCollection dp))
    <> "/"
    <> T.unpack (unDocumentId (dpDocument dp))

-- | URL for a collection (used for listing).
--
-- >>> collectionUrl (ProjectId "p") (CollectionPath "users")
-- "https://firestore.googleapis.com/v1/projects/p/databases/(default)/documents/users"
collectionUrl :: ProjectId -> CollectionPath -> String
collectionUrl pid cp =
  firestoreBaseUrl
    <> "/projects/"
    <> T.unpack (unProjectId pid)
    <> firestoreDatabasePath
    <> "/"
    <> T.unpack (unCollectionPath cp)

-- | URL for creating a document with a specific ID.
--
-- The document ID is passed as a query parameter.
createDocUrl :: ProjectId -> CollectionPath -> DocumentId -> String
createDocUrl pid cp did =
  collectionUrl pid cp
    <> "?documentId="
    <> T.unpack (unDocumentId did)

-- | URL for updating a document with an optional field mask.
--
-- Empty field list means update all fields.
updateDocUrl :: ProjectId -> DocumentPath -> [Text] -> String
updateDocUrl pid dp fields =
  documentUrl pid dp <> fieldMaskParams fields

-- | URL for running a structured query.
queryUrl :: ProjectId -> String
queryUrl pid =
  firestoreBaseUrl
    <> "/projects/"
    <> T.unpack (unProjectId pid)
    <> firestoreDatabasePath
    <> ":runQuery"

-- | URL for beginning a transaction.
beginTransactionUrl :: ProjectId -> String
beginTransactionUrl pid =
  firestoreBaseUrl
    <> "/projects/"
    <> T.unpack (unProjectId pid)
    <> firestoreDatabasePath
    <> ":beginTransaction"

-- | URL for committing a transaction.
commitUrl :: ProjectId -> String
commitUrl pid =
  firestoreBaseUrl
    <> "/projects/"
    <> T.unpack (unProjectId pid)
    <> firestoreDatabasePath
    <> ":commit"

-- | URL for rolling back a transaction.
rollbackUrl :: ProjectId -> String
rollbackUrl pid =
  firestoreBaseUrl
    <> "/projects/"
    <> T.unpack (unProjectId pid)
    <> firestoreDatabasePath
    <> ":rollback"

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Build @updateMask.fieldPaths@ query parameters.
fieldMaskParams :: [Text] -> String
fieldMaskParams [] = ""
fieldMaskParams fs =
  "?" <> intercalateAmp ["updateMask.fieldPaths=" <> T.unpack f | f <- fs]
  where
    intercalateAmp [] = ""
    intercalateAmp [x] = x
    intercalateAmp (x : xs) = x <> "&" <> intercalateAmp xs

-- ---------------------------------------------------------------------------
-- Request Helpers
-- ---------------------------------------------------------------------------

-- | Add an OAuth2 Bearer token to a request's Authorization header.
authorizeRequest :: AccessToken -> Request -> Request
authorizeRequest (AccessToken tok) req =
  req {requestHeaders = ("Authorization", "Bearer " <> tok) : requestHeaders req}

-- ---------------------------------------------------------------------------
-- Response Parsing
-- ---------------------------------------------------------------------------

-- | Parse an HTTP error response body into a 'FirestoreError'.
--
-- Firestore returns errors in the form:
--
-- @
-- { "error": { "code": 404, "message": "...", "status": "NOT_FOUND" } }
-- @
parseFirestoreError :: Int -> LBS.ByteString -> FirestoreError
parseFirestoreError status body =
  case Aeson.decode body of
    Just errObj -> parseErrorObject status errObj
    Nothing -> NetworkError ("HTTP " <> T.pack (show status))

-- | Parse a decoded Firestore error JSON object.
parseErrorObject :: Int -> Aeson.Value -> FirestoreError
parseErrorObject status = go
  where
    go (Aeson.Object o) =
      case Aeson.fromJSON <$> KM.lookup "error" o of
        Just (Aeson.Success errInner) -> parseErrorInner status errInner
        _ -> fallback
    go _ = fallback
    fallback = FirestoreApiError status "" "unknown error"

-- | Parse the inner @\"error\"@ object.
parseErrorInner :: Int -> Aeson.Value -> FirestoreError
parseErrorInner status = go
  where
    go val = case parseMaybe parseInner val of
      Just (code, grpcStatus, msg) -> classifyError code grpcStatus msg
      Nothing -> FirestoreApiError status "" "unparseable error"
    parseInner = Aeson.withObject "error" $ \o ->
      (,,) <$> o .: "code" <*> o .:? "status" <*> o .:? "message"

-- | Classify a Firestore error by HTTP code and gRPC status.
classifyError :: Int -> Maybe Text -> Maybe Text -> FirestoreError
classifyError 404 _ _ = DocumentNotFound
classifyError 403 _ msg = PermissionDenied (fromMaybe "" msg)
classifyError 409 (Just "ABORTED") msg = TransactionAborted (fromMaybe "" msg)
classifyError code grpcStatus msg =
  FirestoreApiError code (fromMaybe "" grpcStatus) (fromMaybe "" msg)
