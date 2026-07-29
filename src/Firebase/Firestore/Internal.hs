-- |
-- Module      : Firebase.Firestore.Internal
-- Description : Pure URL builders, request helpers, and response decoders
-- License     : BSD-3-Clause
--
-- Internal utilities for constructing Firestore REST API URLs and requests
-- and for decoding its responses. All functions are pure and testable
-- without IO.
--
-- Every caller-supplied component is percent-encoded on the way into a URL,
-- so a document ID or field path containing @?@, @#@, @%@, a space, or any
-- non-ASCII character addresses the document it names instead of corrupting
-- the request.
module Firebase.Firestore.Internal
  ( -- * Constants
    firestoreBaseUrl,

    -- * Collection Paths
    splitCollectionPath,

    -- * URL Construction
    databaseUrl,
    documentUrl,
    documentInTransactionUrl,
    collectionUrl,
    createDocUrl,
    updateDocUrl,
    runQueryUrl,
    beginTransactionUrl,
    commitUrl,
    rollbackUrl,

    -- * Resource Names
    documentResourceName,

    -- * Percent Encoding
    encodePathSegment,
    encodeQueryValue,

    -- * Request Helpers
    authorizeRequest,

    -- * Response Decoding
    parseFirestoreError,
    decodeBody,
    decodeDocumentList,
    decodeQueryResults,
    decodeTransactionId,
  )
where

import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (parseMaybe)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Firebase.Firestore.Types
import Network.HTTP.Client (Request, requestHeaders)
import Network.HTTP.Types.Header (hAuthorization)
import Network.HTTP.Types.URI (urlEncode)

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

-- | Firestore REST API base URL.
firestoreBaseUrl :: String
firestoreBaseUrl = "https://firestore.googleapis.com/v1"

-- | Path of the project's default database, the root of every document path.
firestoreDatabasePath :: String
firestoreDatabasePath = "/databases/(default)/documents"

-- | Path segment introducing a project.
projectsSegment :: String
projectsSegment = "projects/"

-- | Separator between path segments.
pathSeparator :: String
pathSeparator = "/"

-- | Separator between a URL's path and its query string.
querySeparator :: String
querySeparator = "?"

-- | Separator between query parameters.
paramSeparator :: String
paramSeparator = "&"

-- | The @Bearer@ scheme prefix for the @Authorization@ header.
bearerScheme :: BS.ByteString
bearerScheme = "Bearer "

-- ---------------------------------------------------------------------------
-- Database-level endpoints
--
-- Firestore names these with a @:verb@ suffix on the database path rather
-- than a path segment of their own.
-- ---------------------------------------------------------------------------

runQueryEndpoint :: String
runQueryEndpoint = ":runQuery"

beginTransactionEndpoint :: String
beginTransactionEndpoint = ":beginTransaction"

commitEndpoint :: String
commitEndpoint = ":commit"

rollbackEndpoint :: String
rollbackEndpoint = ":rollback"

-- ---------------------------------------------------------------------------
-- Percent encoding
-- ---------------------------------------------------------------------------

-- | Percent-encode one path segment.
--
-- Escapes @\/@ as well, so a caller-supplied value can never introduce a
-- path segment of its own.
--
-- >>> encodePathSegment "a/b c"
-- "a%2Fb%20c"
encodePathSegment :: Text -> String
encodePathSegment = BS8.unpack . urlEncode False . TE.encodeUtf8

-- | Percent-encode a query-string value.
--
-- Escapes @&@ and @=@ in addition to the path set, so a value cannot open a
-- parameter of its own.
--
-- >>> encodeQueryValue "a&b=c"
-- "a%26b%3Dc"
encodeQueryValue :: Text -> String
encodeQueryValue = BS8.unpack . urlEncode True . TE.encodeUtf8

-- | Percent-encode a @\/@-separated path. The separators are structural
-- and survive; each segment between them is encoded.
encodeSlashPath :: Text -> String
encodeSlashPath =
  intercalate pathSeparator
    . map encodePathSegment
    . T.splitOn (T.pack pathSeparator)

-- | Percent-encode a collection path.
--
-- Subcollections are addressed as @users\/abc\/posts@, so the @\/@
-- separators survive; each segment between them is encoded.
encodeCollectionPath :: CollectionPath -> String
encodeCollectionPath = encodeSlashPath . unCollectionPath

-- ---------------------------------------------------------------------------
-- Collection paths
-- ---------------------------------------------------------------------------

-- | Split a collection path into its parent document path, if any, and its
-- collection ID: the final @\/@-separated segment.
--
-- A structured query names these separately: the parent addresses the
-- @:runQuery@ URL and the collection ID travels in the query body.
--
-- >>> splitCollectionPath (CollectionPath "users")
-- (Nothing,"users")
--
-- >>> splitCollectionPath (CollectionPath "users/abc/posts")
-- (Just "users/abc","posts")
splitCollectionPath :: CollectionPath -> (Maybe Text, Text)
splitCollectionPath (CollectionPath path) =
  case T.breakOnEnd separator path of
    (parentAndSlash, collectionId)
      | T.null parentAndSlash -> (Nothing, collectionId)
      | otherwise -> (Just (T.dropEnd (T.length separator) parentAndSlash), collectionId)
  where
    separator = T.pack pathSeparator

-- ---------------------------------------------------------------------------
-- URL Construction
-- ---------------------------------------------------------------------------

-- | URL of a project's default database: the prefix every other URL builds on.
--
-- >>> databaseUrl (ProjectId "p")
-- "https://firestore.googleapis.com/v1/projects/p/databases/(default)/documents"
databaseUrl :: ProjectId -> String
databaseUrl pid =
  firestoreBaseUrl
    <> pathSeparator
    <> projectsSegment
    <> encodePathSegment (unProjectId pid)
    <> firestoreDatabasePath

-- | URL for a collection (used for listing).
--
-- >>> collectionUrl (ProjectId "p") (CollectionPath "users")
-- "https://firestore.googleapis.com/v1/projects/p/databases/(default)/documents/users"
collectionUrl :: ProjectId -> CollectionPath -> String
collectionUrl pid cp =
  databaseUrl pid <> pathSeparator <> encodeCollectionPath cp

-- | URL for a specific document.
--
-- >>> documentUrl (ProjectId "p") (DocumentPath (CollectionPath "c") (DocumentId "d"))
-- "https://firestore.googleapis.com/v1/projects/p/databases/(default)/documents/c/d"
documentUrl :: ProjectId -> DocumentPath -> String
documentUrl pid dp =
  collectionUrl pid (dpCollection dp)
    <> pathSeparator
    <> encodePathSegment (unDocumentId (dpDocument dp))

-- | URL for fetching a document inside a transaction, pinning the read to
-- the transaction's snapshot.
--
-- Transaction IDs are base64, so the value is percent-encoded on its way
-- into the query string.
documentInTransactionUrl :: ProjectId -> TransactionId -> DocumentPath -> String
documentInTransactionUrl pid txn dp =
  documentUrl pid dp
    <> querySeparator
    <> transactionParam
    <> encodeQueryValue (unTransactionId txn)

-- | URL for creating a document with a specific ID.
--
-- The document ID is passed as a query parameter.
createDocUrl :: ProjectId -> CollectionPath -> DocumentId -> String
createDocUrl pid cp did =
  collectionUrl pid cp
    <> querySeparator
    <> documentIdParam
    <> encodeQueryValue (unDocumentId did)

-- | URL for updating a document with an optional field mask.
--
-- Empty field list means update all fields.
updateDocUrl :: ProjectId -> DocumentPath -> [Text] -> String
updateDocUrl pid dp fields =
  documentUrl pid dp <> fieldMaskParams fields

-- | URL for running a structured query: the @:runQuery@ verb on the
-- queried collection's parent, which is the database root for a top-level
-- collection and the enclosing document for a subcollection.
--
-- The collection's own ID travels in the query body, not the URL; see
-- 'Firebase.Firestore.Query.encodeQuery'.
runQueryUrl :: ProjectId -> CollectionPath -> String
runQueryUrl pid cp =
  case splitCollectionPath cp of
    (Nothing, _) -> databaseEndpoint runQueryEndpoint pid
    (Just parent, _) ->
      databaseUrl pid
        <> pathSeparator
        <> encodeSlashPath parent
        <> runQueryEndpoint

-- | URL for beginning a transaction.
beginTransactionUrl :: ProjectId -> String
beginTransactionUrl = databaseEndpoint beginTransactionEndpoint

-- | URL for committing a transaction.
commitUrl :: ProjectId -> String
commitUrl = databaseEndpoint commitEndpoint

-- | URL for rolling back a transaction.
rollbackUrl :: ProjectId -> String
rollbackUrl = databaseEndpoint rollbackEndpoint

-- ---------------------------------------------------------------------------
-- Resource Names
-- ---------------------------------------------------------------------------

-- | Full resource name of a document, as it appears in a document's @name@
-- field and in the writes a transaction commits.
--
-- A resource name is not a URL. It travels inside a JSON string, so its
-- components are carried verbatim rather than percent-encoded.
--
-- >>> documentResourceName (ProjectId "p") (DocumentPath (CollectionPath "c") (DocumentId "d"))
-- "projects/p/databases/(default)/documents/c/d"
documentResourceName :: ProjectId -> DocumentPath -> Text
documentResourceName pid dp =
  T.concat
    [ T.pack projectsSegment,
      unProjectId pid,
      T.pack firestoreDatabasePath,
      separator,
      unCollectionPath (dpCollection dp),
      separator,
      unDocumentId (dpDocument dp)
    ]
  where
    separator = T.pack pathSeparator

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | A database-level endpoint, named by its @:method@ suffix.
databaseEndpoint :: String -> ProjectId -> String
databaseEndpoint verb pid = databaseUrl pid <> verb

-- | Query parameter naming the ID a created document should take.
documentIdParam :: String
documentIdParam = "documentId="

-- | Query parameter naming one field an update should touch.
fieldMaskParam :: String
fieldMaskParam = "updateMask.fieldPaths="

-- | Query parameter pinning a read to a transaction's snapshot.
transactionParam :: String
transactionParam = "transaction="

-- | Build the @updateMask.fieldPaths@ query parameters.
--
-- No fields means no mask, which Firestore reads as "replace them all".
fieldMaskParams :: [Text] -> String
fieldMaskParams [] = ""
fieldMaskParams fields =
  querySeparator
    <> intercalate paramSeparator [fieldMaskParam <> encodeQueryValue f | f <- fields]

-- ---------------------------------------------------------------------------
-- Request Helpers
-- ---------------------------------------------------------------------------

-- | Add an OAuth2 Bearer token to a request's Authorization header.
authorizeRequest :: AccessToken -> Request -> Request
authorizeRequest (AccessToken tok) req =
  req {requestHeaders = (hAuthorization, bearerScheme <> tok) : requestHeaders req}

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
--
-- Bodies that do not match fall back to the HTTP status, which is always
-- known.
parseFirestoreError :: Int -> LBS.ByteString -> FirestoreError
parseFirestoreError status body =
  case Aeson.decode body >>= parseMaybe errorFields of
    Just (code, grpcStatus, msg) -> classifyError code grpcStatus msg
    Nothing -> unrecognized
  where
    -- A body that is not JSON, or not shaped like a Firestore error, tells
    -- us nothing the HTTP status has not already.
    unrecognized = NetworkError ("HTTP " <> T.pack (show status))

    errorFields = Aeson.withObject "response" $ \o ->
      o .: "error" >>= Aeson.withObject "error" errorTriple

    errorTriple e = (,,) <$> e .: "code" <*> e .:? "status" <*> e .:? "message"

-- | Classify a Firestore error by HTTP code and gRPC status.
classifyError :: Int -> Maybe Text -> Maybe Text -> FirestoreError
classifyError 404 _ _ = DocumentNotFound
classifyError 403 _ msg = PermissionDenied (orEmpty msg)
classifyError 409 (Just "ABORTED") msg = TransactionAborted (orEmpty msg)
classifyError code grpcStatus msg =
  FirestoreApiError code (orEmpty grpcStatus) (orEmpty msg)

-- | An absent optional error field reads as the empty string.
orEmpty :: Maybe Text -> Text
orEmpty = fromMaybe ""

-- | Decode a response body, reporting parse failures as 'InvalidResponse'.
decodeBody :: (Aeson.FromJSON a) => LBS.ByteString -> Either FirestoreError a
decodeBody = first (InvalidResponse . T.pack) . Aeson.eitherDecode

-- | Decode a @:list@ response. An empty collection omits @documents@.
decodeDocumentList :: LBS.ByteString -> Either FirestoreError [Document]
decodeDocumentList body = decodeBody body >>= documentsField
  where
    documentsField (Aeson.Object o) =
      maybe (Right []) (fromAesonResult . Aeson.fromJSON) (KM.lookup "documents" o)
    documentsField _ = Left (InvalidResponse "expected a JSON object")

-- | Decode a @:runQuery@ response: a stream of result objects, of which
-- only some carry a document (the final entry may be @readTime@-only).
--
-- A result object whose document fails to decode is an error, not a
-- skipped entry: silently dropping it would misreport what the query
-- matched.
decodeQueryResults :: LBS.ByteString -> Either FirestoreError [Document]
decodeQueryResults body =
  decodeBody body >>= fmap catMaybes . traverse resultDocument
  where
    resultDocument (Aeson.Object o) =
      traverse (fromAesonResult . Aeson.fromJSON) (KM.lookup "document" o)
    resultDocument _ = Left (InvalidResponse "expected a query result object")

-- | Decode a @:beginTransaction@ response to extract the transaction ID.
decodeTransactionId :: LBS.ByteString -> Either FirestoreError TransactionId
decodeTransactionId body =
  decodeBody body >>= maybe (Left missingTransaction) Right . transactionField
  where
    missingTransaction = InvalidResponse "missing transaction field"

    transactionField (Aeson.Object o) = case KM.lookup "transaction" o of
      Just (Aeson.String txnId) -> Just (TransactionId txnId)
      _ -> Nothing
    transactionField _ = Nothing

-- | Read an aeson conversion, reporting failure as 'InvalidResponse'.
fromAesonResult :: Aeson.Result a -> Either FirestoreError a
fromAesonResult (Aeson.Success value) = Right value
fromAesonResult (Aeson.Error err) = Left (InvalidResponse (T.pack err))
