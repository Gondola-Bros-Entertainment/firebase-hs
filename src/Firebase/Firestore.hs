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
-- import qualified Data.Map.Strict as Map
-- import Firebase.Firestore
--
-- main :: IO ()
-- main = do
--   fs <- newFirestore (ProjectId \"my-project\") (AccessToken \"ya29...\")
--   let path = DocumentPath (CollectionPath \"users\") (DocumentId \"alice\")
--   result <- getDocument fs path
--   case result of
--     Left err  -> print err
--     Right doc -> print (docFields doc)
-- @
module Firebase.Firestore
  ( -- * Handle
    newFirestore,
    withToken,

    -- * CRUD Operations
    getDocument,
    createDocument,
    updateDocument,
    deleteDocument,

    -- * Listing
    listDocuments,

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
  ( Request,
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
-- Handle
-- ---------------------------------------------------------------------------

-- | Build a t'Firestore' handle with a fresh TLS-enabled HTTP manager.
--
-- Build one and share it: the manager pools connections, so a handle per
-- request throws that away.
newFirestore :: ProjectId -> AccessToken -> IO Firestore
newFirestore pid tok = do
  mgr <- newTlsManager
  pure Firestore {fsManager = mgr, fsProject = pid, fsToken = tok}

-- | Replace the access token, keeping the manager and project.
--
-- OAuth2 tokens expire; swap in a fresh one rather than rebuilding the
-- handle and discarding the connection pool with it.
withToken :: AccessToken -> Firestore -> Firestore
withToken tok fs = fs {fsToken = tok}

-- ---------------------------------------------------------------------------
-- CRUD Operations
-- ---------------------------------------------------------------------------

-- | Fetch a single document by path.
getDocument :: Firestore -> DocumentPath -> IO (Either FirestoreError Document)
getDocument fs dp =
  fmap (>>= decodeBody) (doGet fs (documentUrl (fsProject fs) dp))

-- | Create a document with a specific ID in a collection.
createDocument ::
  Firestore ->
  CollectionPath ->
  DocumentId ->
  Map Text FirestoreValue ->
  IO (Either FirestoreError Document)
createDocument fs cp did fields =
  fmap (>>= decodeBody) (doPost fs (createDocUrl (fsProject fs) cp did) (encodeFields fields))

-- | Update a document's fields. Pass field names to update specific fields,
-- or an empty list to replace all fields.
updateDocument ::
  Firestore ->
  DocumentPath ->
  [Text] ->
  Map Text FirestoreValue ->
  IO (Either FirestoreError Document)
updateDocument fs dp fieldPaths fields =
  fmap (>>= decodeBody) (doPatch fs (updateDocUrl (fsProject fs) dp fieldPaths) (encodeFields fields))

-- | Delete a document by path.
deleteDocument :: Firestore -> DocumentPath -> IO (Either FirestoreError ())
deleteDocument fs dp =
  fmap void (doRequest fs (documentUrl (fsProject fs) dp) methodDelete Nothing)

-- | Encode a field map as a Firestore write body.
encodeFields :: Map Text FirestoreValue -> LBS.ByteString
encodeFields fields = Aeson.encode (Aeson.object ["fields" .= fields])

-- ---------------------------------------------------------------------------
-- Listing
-- ---------------------------------------------------------------------------

-- | List the documents in a collection.
--
-- Returns the first page Firestore sends, which is capped server-side. For
-- anything larger, or for a specific ordering, use 'runQuery'.
listDocuments :: Firestore -> CollectionPath -> IO (Either FirestoreError [Document])
listDocuments fs cp =
  fmap (>>= decodeDocumentList) (doGet fs (collectionUrl (fsProject fs) cp))

-- ---------------------------------------------------------------------------
-- Queries
-- ---------------------------------------------------------------------------

-- | Run a structured query against the Firestore REST API.
runQuery :: Firestore -> StructuredQuery -> IO (Either FirestoreError [Document])
runQuery fs sq =
  fmap (>>= decodeQueryResults) (doPost fs (queryUrl (fsProject fs)) body)
  where
    body = Aeson.encode (encodeQuery sq)

-- ---------------------------------------------------------------------------
-- Transactions
-- ---------------------------------------------------------------------------

-- | Begin a new transaction.
beginTransaction :: Firestore -> TransactionMode -> IO (Either FirestoreError TransactionId)
beginTransaction fs mode =
  fmap (>>= decodeTransactionId) (doPost fs (beginTransactionUrl (fsProject fs)) body)
  where
    body = Aeson.encode (encodeTransactionOptions mode)

-- | Commit a transaction with a list of write operations.
commitTransaction ::
  Firestore -> TransactionId -> [Aeson.Value] -> IO (Either FirestoreError ())
commitTransaction fs (TransactionId txnId) writes =
  fmap void (doPost fs (commitUrl (fsProject fs)) body)
  where
    body = Aeson.encode (Aeson.object ["writes" .= writes, "transaction" .= txnId])

-- | Roll back a transaction without committing.
rollbackTransaction :: Firestore -> TransactionId -> IO (Either FirestoreError ())
rollbackTransaction fs (TransactionId txnId) =
  fmap void (doPost fs (rollbackUrl (fsProject fs)) body)
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
-- runTransaction fs ReadWrite $ \\_txnId -> runExceptT $ do
--   doc <- ExceptT $ getDocument fs somePath
--   pure [mkUpdateWrite (fsProject fs) somePath (applyDebit 100 (docFields doc))]
-- @
runTransaction ::
  Firestore ->
  TransactionMode ->
  (TransactionId -> IO (Either FirestoreError [Aeson.Value])) ->
  IO (Either FirestoreError ())
runTransaction fs mode action = do
  started <- beginTransaction fs mode
  either (pure . Left) commitOrRollback started
  where
    commitOrRollback txnId = do
      result <- runExceptT $ do
        writes <- ExceptT (action txnId)
        ExceptT (commitTransaction fs txnId writes)
      either (rollbackWith txnId) (pure . Right) result

    -- The original failure is what the caller needs; a rollback that also
    -- fails must not mask it.
    rollbackWith txnId err = do
      _ <- rollbackTransaction fs txnId
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
doGet :: Firestore -> String -> IO (Either FirestoreError LBS.ByteString)
doGet fs url = doRequest fs url methodGet Nothing

-- | Perform an authorized POST request with a JSON body.
doPost ::
  Firestore -> String -> LBS.ByteString -> IO (Either FirestoreError LBS.ByteString)
doPost fs url body = doRequest fs url methodPost (Just body)

-- | Perform an authorized PATCH request with a JSON body.
doPatch ::
  Firestore -> String -> LBS.ByteString -> IO (Either FirestoreError LBS.ByteString)
doPatch fs url body = doRequest fs url methodPatch (Just body)

-- | Core HTTP request executor.
doRequest ::
  Firestore ->
  String ->
  Method ->
  Maybe LBS.ByteString ->
  IO (Either FirestoreError LBS.ByteString)
doRequest fs url httpMethod mBody = runExceptT $ do
  baseReq <- ExceptT (tryNetwork (parseRequest url))
  let req = buildRequest (fsToken fs) httpMethod mBody baseReq
  resp <- ExceptT (tryNetwork (httpLbs req (fsManager fs)))
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

-- | Decode a @:list@ response. An empty collection omits @documents@.
decodeDocumentList :: LBS.ByteString -> Either FirestoreError [Document]
decodeDocumentList body = decodeBody body >>= documentsField
  where
    documentsField (Aeson.Object o) =
      maybe (Right []) fromResult (KM.lookup "documents" o)
    documentsField _ = Left (InvalidResponse "expected a JSON object")

    fromResult value = case Aeson.fromJSON value of
      Aeson.Success docs -> Right docs
      Aeson.Error err -> Left (InvalidResponse (T.pack err))

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
