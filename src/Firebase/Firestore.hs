-- |
-- Module      : Firebase.Firestore
-- Description : Firestore REST API client
-- License     : BSD-3-Clause
--
-- CRUD operations, structured queries, and atomic transactions against the
-- Firestore REST API. All operations return @'Either' 'FirestoreError' a@ -
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

    -- * Transaction Writes
    mkUpdateWrite,
    mkDeleteWrite,

    -- * HTTP Manager
    newTlsManager,

    -- * Re-exports
    module Firebase.Firestore.Types,
    module Firebase.Firestore.Query,
  )
where

import Control.Exception (SomeException, try)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Functor (void)
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
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
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Method
  ( Method,
    methodDelete,
    methodGet,
    methodPatch,
    methodPost,
  )
import Network.HTTP.Types.Status (statusCode, statusIsSuccessful)

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

-- | Media type of every request body this client sends.
applicationJson :: BS.ByteString
applicationJson = "application/json"

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
getDocument mgr tok pid dp =
  fmap (>>= decodeBody) (doGet mgr tok (documentUrl pid dp))

-- | Create a document with a specific ID in a collection.
createDocument ::
  Manager ->
  AccessToken ->
  ProjectId ->
  CollectionPath ->
  DocumentId ->
  Map Text FirestoreValue ->
  IO (Either FirestoreError Document)
createDocument mgr tok pid cp did fields =
  fmap (>>= decodeBody) (doPost mgr tok (createDocUrl pid cp did) (encodeFields fields))

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
updateDocument mgr tok pid dp fieldPaths fields =
  fmap (>>= decodeBody) (doPatch mgr tok (updateDocUrl pid dp fieldPaths) (encodeFields fields))

-- | Delete a document by path.
deleteDocument ::
  Manager ->
  AccessToken ->
  ProjectId ->
  DocumentPath ->
  IO (Either FirestoreError ())
deleteDocument mgr tok pid dp =
  fmap void (doRequest mgr tok (documentUrl pid dp) methodDelete Nothing)

-- | Encode a field map as a Firestore write body.
encodeFields :: Map Text FirestoreValue -> LBS.ByteString
encodeFields fields = Aeson.encode (Aeson.object ["fields" .= fields])

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
runQuery mgr tok pid sq =
  fmap (>>= decodeQueryResults) (doPost mgr tok (queryUrl pid) body)
  where
    body = Aeson.encode (encodeQuery sq)

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
beginTransaction mgr tok pid mode =
  fmap (>>= decodeTransactionId) (doPost mgr tok (beginTransactionUrl pid) body)
  where
    body = Aeson.encode (encodeTransactionOptions mode)

-- | Commit a transaction with a list of write operations.
commitTransaction ::
  Manager ->
  AccessToken ->
  ProjectId ->
  TransactionId ->
  [Aeson.Value] ->
  IO (Either FirestoreError ())
commitTransaction mgr tok pid (TransactionId txnId) writes =
  fmap void (doPost mgr tok (commitUrl pid) body)
  where
    body = Aeson.encode (Aeson.object ["writes" .= writes, "transaction" .= txnId])

-- | Roll back a transaction without committing.
rollbackTransaction ::
  Manager ->
  AccessToken ->
  ProjectId ->
  TransactionId ->
  IO (Either FirestoreError ())
rollbackTransaction mgr tok pid (TransactionId txnId) =
  fmap void (doPost mgr tok (rollbackUrl pid) body)
  where
    body = Aeson.encode (Aeson.object ["transaction" .= txnId])

-- | Run an atomic transaction. The callback receives the transaction ID
-- and should return a list of writes to commit.
--
-- On success, all writes are applied atomically. On failure (including
-- callback errors), the transaction is rolled back automatically.
-- Use 'RetryWith' to retry an aborted transaction.
--
-- @
-- runTransaction mgr tok pid ReadWrite $ \\_txnId -> runExceptT $ do
--   doc <- ExceptT $ getDocument mgr tok pid somePath
--   pure [mkUpdateWrite pid somePath (bumpVersion (docFields doc))]
-- @
runTransaction ::
  Manager ->
  AccessToken ->
  ProjectId ->
  TransactionMode ->
  (TransactionId -> IO (Either FirestoreError [Aeson.Value])) ->
  IO (Either FirestoreError ())
runTransaction mgr tok pid mode action = do
  started <- beginTransaction mgr tok pid mode
  either (pure . Left) commitOrRollback started
  where
    commitOrRollback txnId = do
      result <- runExceptT $ do
        writes <- ExceptT (action txnId)
        ExceptT (commitTransaction mgr tok pid txnId writes)
      either (rollbackWith txnId) (pure . Right) result

    -- The original failure is what the caller needs; a rollback that also
    -- fails must not mask it.
    rollbackWith txnId err = do
      _ <- rollbackTransaction mgr tok pid txnId
      pure (Left err)

-- ---------------------------------------------------------------------------
-- Transaction Writes
-- ---------------------------------------------------------------------------

-- | A write that sets a document's fields, creating it if absent.
--
-- 'commitTransaction' and the callback to 'runTransaction' take writes in
-- Firestore's wire form; these constructors build that form so callers do
-- not hand-assemble it.
mkUpdateWrite ::
  ProjectId ->
  DocumentPath ->
  Map Text FirestoreValue ->
  Aeson.Value
mkUpdateWrite pid dp fields =
  Aeson.object
    [ "update"
        .= Aeson.object
          [ "name" .= documentResourceName pid dp,
            "fields" .= fields
          ]
    ]

-- | A write that deletes a document.
mkDeleteWrite :: ProjectId -> DocumentPath -> Aeson.Value
mkDeleteWrite pid dp =
  Aeson.object ["delete" .= documentResourceName pid dp]

-- ---------------------------------------------------------------------------
-- HTTP Helpers
-- ---------------------------------------------------------------------------

-- | Perform an authorized GET request.
doGet ::
  Manager -> AccessToken -> String -> IO (Either FirestoreError LBS.ByteString)
doGet mgr tok url = doRequest mgr tok url methodGet Nothing

-- | Perform an authorized POST request with a JSON body.
doPost ::
  Manager ->
  AccessToken ->
  String ->
  LBS.ByteString ->
  IO (Either FirestoreError LBS.ByteString)
doPost mgr tok url body = doRequest mgr tok url methodPost (Just body)

-- | Perform an authorized PATCH request with a JSON body.
doPatch ::
  Manager ->
  AccessToken ->
  String ->
  LBS.ByteString ->
  IO (Either FirestoreError LBS.ByteString)
doPatch mgr tok url body = doRequest mgr tok url methodPatch (Just body)

-- | Core HTTP request executor.
doRequest ::
  Manager ->
  AccessToken ->
  String ->
  Method ->
  Maybe LBS.ByteString ->
  IO (Either FirestoreError LBS.ByteString)
doRequest mgr tok url httpMethod mBody = runExceptT $ do
  baseReq <- ExceptT (tryNetwork (parseRequest url))
  resp <- ExceptT (tryNetwork (httpLbs (buildRequest tok httpMethod mBody baseReq) mgr))
  ExceptT (pure (responseOrError resp))

-- | Run an action that may throw, reporting any exception as a network error.
tryNetwork :: IO a -> IO (Either FirestoreError a)
tryNetwork action = first describe <$> try action
  where
    describe :: SomeException -> FirestoreError
    describe = NetworkError . T.pack . show

-- | Apply the method, headers, and body to a parsed request.
buildRequest :: AccessToken -> Method -> Maybe LBS.ByteString -> Request -> Request
buildRequest tok httpMethod mBody req =
  authorizeRequest
    tok
    req
      { method = httpMethod,
        requestHeaders = (hContentType, applicationJson) : requestHeaders req,
        requestBody = maybe mempty RequestBodyLBS mBody
      }

-- | A 2xx response yields its body; anything else is a Firestore error.
responseOrError :: Response LBS.ByteString -> Either FirestoreError LBS.ByteString
responseOrError resp
  | statusIsSuccessful status = Right body
  | otherwise = Left (parseFirestoreError (statusCode status) body)
  where
    status = responseStatus resp
    body = responseBody resp

-- ---------------------------------------------------------------------------
-- Response Decoders (pure)
-- ---------------------------------------------------------------------------

-- | Decode a response body, reporting parse failures as 'InvalidResponse'.
decodeBody :: (Aeson.FromJSON a) => LBS.ByteString -> Either FirestoreError a
decodeBody = first (InvalidResponse . T.pack) . Aeson.eitherDecode

-- | Decode a query response: a stream of result objects, of which only some
-- carry a document.
decodeQueryResults :: LBS.ByteString -> Either FirestoreError [Document]
decodeQueryResults = fmap extractDocuments . decodeBody

-- | Extract documents from query result objects, skipping entries without
-- a @\"document\"@ field (e.g. the final @\"readTime\"@-only entry).
extractDocuments :: [Aeson.Value] -> [Document]
extractDocuments = mapMaybe resultDocument
  where
    resultDocument (Aeson.Object o) = KM.lookup "document" o >>= fromResult . Aeson.fromJSON
    resultDocument _ = Nothing

    fromResult (Aeson.Success doc) = Just doc
    fromResult (Aeson.Error _) = Nothing

-- | Decode a beginTransaction response to extract the transaction ID.
decodeTransactionId :: LBS.ByteString -> Either FirestoreError TransactionId
decodeTransactionId body =
  decodeBody body >>= maybe (Left missingTransaction) Right . transactionField
  where
    missingTransaction = InvalidResponse "missing transaction field"

    transactionField (Aeson.Object o) = case KM.lookup "transaction" o of
      Just (Aeson.String txnId) -> Just (TransactionId txnId)
      _ -> Nothing
    transactionField _ = Nothing
