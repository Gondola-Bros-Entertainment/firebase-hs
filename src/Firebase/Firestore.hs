{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : Firebase.Firestore
-- Description : Firestore REST API client
-- License     : MIT
--
-- CRUD operations, structured queries, and atomic transactions against the
-- Firestore REST API. All operations return @'Either' 'FirestoreError' a@ —
-- no exceptions are thrown for API-level errors.
--
-- @
-- import Firebase.Firestore
--
-- main :: IO ()
-- main = do
--   mgr <- newTlsManager
--   let pid = ProjectId \"my-project\"
--       tok = AccessToken \"ya29...\"
--       path = DocumentPath (CollectionPath \"users\") (DocumentId \"alice\")
--   result <- getDocument mgr tok pid path
--   case result of
--     Left err  -> print err
--     Right doc -> print (docFields doc)
-- @
module Firebase.Firestore
  ( -- * CRUD Operations
    getDocument,
    createDocument,
    updateDocument,
    deleteDocument,

    -- * Queries
    runQuery,

    -- * Transactions
    beginTransaction,
    commitTransaction,
    rollbackTransaction,
    runTransaction,

    -- * HTTP Manager
    newTlsManager,

    -- * Re-exports
    module Firebase.Firestore.Types,
    module Firebase.Firestore.Query,
  )
where

import Control.Exception (SomeException, try)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Firebase.Firestore.Internal
import Firebase.Firestore.Query
import Firebase.Firestore.Types
import Network.HTTP.Client
  ( Manager,
    Request,
    RequestBody (..),
    Response,
    httpLbs,
    method,
    parseRequest,
    requestBody,
    requestHeaders,
    responseBody,
    responseStatus,
  )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)

-- ---------------------------------------------------------------------------
-- HTTP status code constants
-- ---------------------------------------------------------------------------

-- | HTTP 200 OK.
httpOk :: Int
httpOk = 200

-- | Upper bound for successful HTTP status codes (exclusive).
httpSuccessUpperBound :: Int
httpSuccessUpperBound = 300

-- ---------------------------------------------------------------------------
-- CRUD Operations
-- ---------------------------------------------------------------------------

-- | Fetch a single document by path.
getDocument ::
  Manager ->
  AccessToken ->
  ProjectId ->
  DocumentPath ->
  IO (Either FirestoreError Document)
getDocument mgr tok pid dp = do
  result <- doGet mgr tok (documentUrl pid dp)
  pure $ result >>= decodeDocument

-- | Create a document with a specific ID in a collection.
createDocument ::
  Manager ->
  AccessToken ->
  ProjectId ->
  CollectionPath ->
  DocumentId ->
  Map Text FirestoreValue ->
  IO (Either FirestoreError Document)
createDocument mgr tok pid cp did fields = do
  let body = Aeson.encode (Aeson.object ["fields" .= Aeson.toJSON fields])
  result <- doPost mgr tok (createDocUrl pid cp did) body
  pure $ result >>= decodeDocument

-- | Update a document's fields. Pass field names to update specific fields,
-- or an empty list to replace all fields.
updateDocument ::
  Manager ->
  AccessToken ->
  ProjectId ->
  DocumentPath ->
  [Text] ->
  Map Text FirestoreValue ->
  IO (Either FirestoreError Document)
updateDocument mgr tok pid dp fieldPaths fields = do
  let body = Aeson.encode (Aeson.object ["fields" .= Aeson.toJSON fields])
  result <- doPatch mgr tok (updateDocUrl pid dp fieldPaths) body
  pure $ result >>= decodeDocument

-- | Delete a document by path.
deleteDocument ::
  Manager ->
  AccessToken ->
  ProjectId ->
  DocumentPath ->
  IO (Either FirestoreError ())
deleteDocument mgr tok pid dp = do
  result <- doRequest mgr tok (documentUrl pid dp) "DELETE" Nothing
  pure $ result >> Right ()

-- ---------------------------------------------------------------------------
-- Queries
-- ---------------------------------------------------------------------------

-- | Run a structured query against the Firestore REST API.
runQuery ::
  Manager ->
  AccessToken ->
  ProjectId ->
  StructuredQuery ->
  IO (Either FirestoreError [Document])
runQuery mgr tok pid sq = do
  let body = Aeson.encode (encodeQuery sq)
  result <- doPost mgr tok (queryUrl pid) body
  pure $ result >>= decodeQueryResults

-- ---------------------------------------------------------------------------
-- Transactions
-- ---------------------------------------------------------------------------

-- | Begin a new transaction.
beginTransaction ::
  Manager ->
  AccessToken ->
  ProjectId ->
  TransactionMode ->
  IO (Either FirestoreError TransactionId)
beginTransaction mgr tok pid mode = do
  let body = Aeson.encode (encodeTransactionOptions mode)
  result <- doPost mgr tok (beginTransactionUrl pid) body
  pure $ result >>= decodeTransactionId

-- | Commit a transaction with a list of write operations.
commitTransaction ::
  Manager ->
  AccessToken ->
  ProjectId ->
  TransactionId ->
  [Aeson.Value] ->
  IO (Either FirestoreError ())
commitTransaction mgr tok pid (TransactionId txnId) writes = do
  let body =
        Aeson.encode
          (Aeson.object ["writes" .= writes, "transaction" .= txnId])
  result <- doPost mgr tok (commitUrl pid) body
  pure $ result >> Right ()

-- | Roll back a transaction without committing.
rollbackTransaction ::
  Manager ->
  AccessToken ->
  ProjectId ->
  TransactionId ->
  IO (Either FirestoreError ())
rollbackTransaction mgr tok pid (TransactionId txnId) = do
  let body = Aeson.encode (Aeson.object ["transaction" .= txnId])
  result <- doPost mgr tok (rollbackUrl pid) body
  pure $ result >> Right ()

-- | Run an atomic transaction. The callback receives the transaction ID
-- and should return a list of writes to commit.
--
-- On success, all writes are applied atomically. On failure (including
-- callback errors), the transaction is rolled back automatically.
-- Use 'RetryWith' to retry an aborted transaction.
--
-- @
-- runTransaction mgr tok pid ReadWrite $ \\txnId -> do
--   doc <- getDocument mgr tok pid somePath
--   pure [updateWrite, deleteWrite]
-- @
runTransaction ::
  Manager ->
  AccessToken ->
  ProjectId ->
  TransactionMode ->
  (TransactionId -> IO (Either FirestoreError [Aeson.Value])) ->
  IO (Either FirestoreError ())
runTransaction mgr tok pid mode action = do
  txnResult <- beginTransaction mgr tok pid mode
  case txnResult of
    Left err -> pure (Left err)
    Right txnId -> do
      writesResult <- action txnId
      case writesResult of
        Left err -> do
          _ <- rollbackTransaction mgr tok pid txnId
          pure (Left err)
        Right writes -> do
          commitResult <- commitTransaction mgr tok pid txnId writes
          case commitResult of
            Left err -> do
              _ <- rollbackTransaction mgr tok pid txnId
              pure (Left err)
            Right () -> pure (Right ())

-- ---------------------------------------------------------------------------
-- HTTP Helpers
-- ---------------------------------------------------------------------------

-- | Perform an authorized GET request.
doGet ::
  Manager -> AccessToken -> String -> IO (Either FirestoreError LBS.ByteString)
doGet mgr tok url = doRequest mgr tok url "GET" Nothing

-- | Perform an authorized POST request with a JSON body.
doPost ::
  Manager ->
  AccessToken ->
  String ->
  LBS.ByteString ->
  IO (Either FirestoreError LBS.ByteString)
doPost mgr tok url body = doRequest mgr tok url "POST" (Just body)

-- | Perform an authorized PATCH request with a JSON body.
doPatch ::
  Manager ->
  AccessToken ->
  String ->
  LBS.ByteString ->
  IO (Either FirestoreError LBS.ByteString)
doPatch mgr tok url body = doRequest mgr tok url "PATCH" (Just body)

-- | Core HTTP request executor.
doRequest ::
  Manager ->
  AccessToken ->
  String ->
  BS.ByteString ->
  Maybe LBS.ByteString ->
  IO (Either FirestoreError LBS.ByteString)
doRequest mgr tok url httpMethod mBody = do
  reqResult <- try (parseRequest url) :: IO (Either SomeException Request)
  case reqResult of
    Left err -> pure (Left (NetworkError (T.pack (show err))))
    Right baseReq -> do
      let req =
            authorizeRequest tok $
              baseReq
                { method = httpMethod,
                  requestHeaders =
                    ("Content-Type", "application/json")
                      : requestHeaders baseReq,
                  requestBody = maybe mempty RequestBodyLBS mBody
                }
      respResult <-
        try (httpLbs req mgr) ::
          IO (Either SomeException (Response LBS.ByteString))
      case respResult of
        Left err -> pure (Left (NetworkError (T.pack (show err))))
        Right resp ->
          let status = statusCode (responseStatus resp)
              body = responseBody resp
           in if status >= httpOk && status < httpSuccessUpperBound
                then pure (Right body)
                else pure (Left (parseFirestoreError status body))

-- ---------------------------------------------------------------------------
-- Response Decoders (pure)
-- ---------------------------------------------------------------------------

-- | Decode a response body as a 'Document'.
decodeDocument :: LBS.ByteString -> Either FirestoreError Document
decodeDocument body = case Aeson.eitherDecode body of
  Left err -> Left (InvalidResponse (T.pack err))
  Right doc -> Right doc

-- | Decode a query response (array of @{\"document\": ...}@ objects).
decodeQueryResults :: LBS.ByteString -> Either FirestoreError [Document]
decodeQueryResults body = case Aeson.eitherDecode body of
  Left err -> Left (InvalidResponse (T.pack err))
  Right results -> Right (extractDocuments results)

-- | Extract documents from query result objects, skipping entries without
-- a @\"document\"@ field (e.g. the final @\"readTime\"@-only entry).
extractDocuments :: [Aeson.Value] -> [Document]
extractDocuments = concatMap go
  where
    go (Aeson.Object o) = case KM.lookup "document" o of
      Just v -> case Aeson.fromJSON v of
        Aeson.Success doc -> [doc]
        _ -> []
      Nothing -> []
    go _ = []

-- | Decode a beginTransaction response to extract the transaction ID.
decodeTransactionId :: LBS.ByteString -> Either FirestoreError TransactionId
decodeTransactionId body = case Aeson.eitherDecode body of
  Left err -> Left (InvalidResponse (T.pack err))
  Right val -> case extractTxnId val of
    Just txnId -> Right txnId
    Nothing -> Left (InvalidResponse "missing transaction field")
  where
    extractTxnId (Aeson.Object o) = case KM.lookup "transaction" o of
      Just (Aeson.String t) -> Just (TransactionId t)
      _ -> Nothing
    extractTxnId _ = Nothing
