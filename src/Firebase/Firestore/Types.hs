{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Firebase.Firestore.Types
-- Description : Types for Firestore REST API
-- License     : MIT
--
-- Newtypes, value ADT, document, and error types for the Firestore REST API.
-- 'FirestoreValue' uses custom JSON instances matching Firestore's tagged
-- wire format (@{\"stringValue\":\"...\"}@, @{\"integerValue\":\"42\"}@, etc.).
module Firebase.Firestore.Types
  ( -- * Identifiers
    ProjectId (..),
    CollectionPath (..),
    DocumentId (..),
    AccessToken (..),
    DocumentPath (..),

    -- * Values
    FirestoreValue (..),

    -- * Documents
    Document (..),

    -- * Errors
    FirestoreError (..),

    -- * Transactions
    TransactionId (..),
    TransactionMode (..),
    encodeTransactionOptions,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser)
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)

-- ---------------------------------------------------------------------------
-- Identifiers
-- ---------------------------------------------------------------------------

-- | Firebase project ID (e.g. @\"my-project-id\"@).
newtype ProjectId = ProjectId {unProjectId :: Text}
  deriving (Eq, Show)

-- | Firestore collection path (e.g. @\"users\"@ or @\"users\/abc\/posts\"@).
newtype CollectionPath = CollectionPath {unCollectionPath :: Text}
  deriving (Eq, Show)

-- | Firestore document ID within a collection.
newtype DocumentId = DocumentId {unDocumentId :: Text}
  deriving (Eq, Show)

-- | OAuth2 access token for authenticating Firestore requests.
newtype AccessToken = AccessToken {unAccessToken :: BS.ByteString}
  deriving (Eq, Show)

-- | Full path to a document: collection + document ID.
data DocumentPath = DocumentPath
  { dpCollection :: !CollectionPath,
    dpDocument :: !DocumentId
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Transactions
-- ---------------------------------------------------------------------------

-- | Opaque transaction identifier returned by Firestore.
newtype TransactionId = TransactionId {unTransactionId :: Text}
  deriving (Eq, Show)

-- | How to begin a Firestore transaction.
data TransactionMode
  = -- | Read-write transaction (the default).
    ReadWrite
  | -- | Read-write transaction that retries a previously aborted one.
    RetryWith !TransactionId
  | -- | Read-only transaction (no writes allowed).
    ReadOnly
  deriving (Eq, Show)

-- | Encode transaction options to the JSON body for @:beginTransaction@.
encodeTransactionOptions :: TransactionMode -> Aeson.Value
encodeTransactionOptions ReadWrite =
  Aeson.object
    ["options" .= Aeson.object ["readWrite" .= Aeson.object []]]
encodeTransactionOptions (RetryWith (TransactionId txn)) =
  Aeson.object
    [ "options"
        .= Aeson.object
          ["readWrite" .= Aeson.object ["retryTransaction" .= txn]]
    ]
encodeTransactionOptions ReadOnly =
  Aeson.object
    ["options" .= Aeson.object ["readOnly" .= Aeson.object []]]

-- ---------------------------------------------------------------------------
-- Firestore values
-- ---------------------------------------------------------------------------

-- | A Firestore value, mirroring the tagged JSON wire format.
--
-- Integers are transmitted as JSON strings (e.g. @{\"integerValue\":\"42\"}@),
-- not as JSON numbers. The 'FromJSON' \/ 'ToJSON' instances handle this.
data FirestoreValue
  = NullValue
  | BoolValue !Bool
  | IntegerValue !Int64
  | DoubleValue !Double
  | StringValue !Text
  | TimestampValue !UTCTime
  | ArrayValue ![FirestoreValue]
  | MapValue !(Map Text FirestoreValue)
  deriving (Eq, Show)

-- | Firestore's RFC 3339 timestamp format.
timestampFormat :: String
timestampFormat = "%Y-%m-%dT%H:%M:%S%QZ"

instance ToJSON FirestoreValue where
  toJSON NullValue = Aeson.object ["nullValue" .= Aeson.Null]
  toJSON (BoolValue b) = Aeson.object ["booleanValue" .= b]
  toJSON (IntegerValue n) = Aeson.object ["integerValue" .= show n]
  toJSON (DoubleValue d) = Aeson.object ["doubleValue" .= d]
  toJSON (StringValue s) = Aeson.object ["stringValue" .= s]
  toJSON (TimestampValue t) =
    Aeson.object
      ["timestampValue" .= formatTime defaultTimeLocale timestampFormat t]
  toJSON (ArrayValue xs) =
    Aeson.object ["arrayValue" .= Aeson.object ["values" .= xs]]
  toJSON (MapValue m) =
    Aeson.object
      ["mapValue" .= Aeson.object ["fields" .= mapToFieldsJSON m]]

instance FromJSON FirestoreValue where
  parseJSON = Aeson.withObject "FirestoreValue" $ \o ->
    case KM.toList o of
      [("nullValue", _)] -> pure NullValue
      [("booleanValue", v)] -> BoolValue <$> parseJSON v
      [("integerValue", v)] -> IntegerValue <$> parseIntegerValue v
      [("doubleValue", v)] -> DoubleValue <$> parseJSON v
      [("stringValue", v)] -> StringValue <$> parseJSON v
      [("timestampValue", v)] -> TimestampValue <$> parseTimestamp v
      [("arrayValue", v)] -> ArrayValue <$> parseArrayValue v
      [("mapValue", v)] -> MapValue <$> parseMapValue v
      _ -> fail "unrecognized FirestoreValue tag"

-- | Parse an integer value from a JSON string (Firestore's wire format).
parseIntegerValue :: Aeson.Value -> Parser Int64
parseIntegerValue = Aeson.withText "integerValue" $ \t ->
  case reads (T.unpack t) of
    [(n, "")] -> pure n
    _ -> fail ("invalid integerValue: " ++ T.unpack t)

-- | Parse a timestamp from an RFC 3339 string.
parseTimestamp :: Aeson.Value -> Parser UTCTime
parseTimestamp = Aeson.withText "timestampValue" $ \t ->
  case parseTimeM True defaultTimeLocale timestampFormat (T.unpack t) of
    Just utc -> pure utc
    Nothing -> fail ("invalid timestamp: " ++ T.unpack t)

-- | Parse an array value from @{\"values\": [...]}@.
parseArrayValue :: Aeson.Value -> Parser [FirestoreValue]
parseArrayValue = Aeson.withObject "arrayValue" $ \o ->
  o .:? "values" >>= \case
    Nothing -> pure []
    Just vs -> parseJSON vs

-- | Parse a map value from @{\"fields\": {...}}@.
parseMapValue :: Aeson.Value -> Parser (Map Text FirestoreValue)
parseMapValue = Aeson.withObject "mapValue" $ \o ->
  o .:? "fields" >>= \case
    Nothing -> pure Map.empty
    Just fieldsVal -> parseFieldsJSON fieldsVal

-- | Parse Firestore fields object to a 'Map'.
parseFieldsJSON :: Aeson.Value -> Parser (Map Text FirestoreValue)
parseFieldsJSON = Aeson.withObject "fields" $ \o ->
  Map.fromList <$> traverse parseField (KM.toList o)
  where
    parseField (k, v) = (Key.toText k,) <$> parseJSON v

-- | Encode a 'Map' to Firestore fields JSON.
mapToFieldsJSON :: Map Text FirestoreValue -> Aeson.Value
mapToFieldsJSON m =
  Aeson.object [Key.fromText k .= v | (k, v) <- Map.toList m]

-- ---------------------------------------------------------------------------
-- Documents
-- ---------------------------------------------------------------------------

-- | A Firestore document with its metadata.
data Document = Document
  { -- | Full resource name (e.g. @\"projects\/p\/databases\/(default)\/documents\/col\/doc\"@).
    docName :: !Text,
    -- | Document fields.
    docFields :: !(Map Text FirestoreValue),
    -- | Server-assigned creation time.
    docCreateTime :: !(Maybe UTCTime),
    -- | Server-assigned last update time.
    docUpdateTime :: !(Maybe UTCTime)
  }
  deriving (Eq, Show)

instance FromJSON Document where
  parseJSON = Aeson.withObject "Document" $ \o ->
    Document
      <$> o .: "name"
      <*> (o .:? "fields" >>= maybe (pure Map.empty) parseFieldsJSON)
      <*> o .:? "createTime"
      <*> o .:? "updateTime"

instance ToJSON Document where
  toJSON doc =
    Aeson.object
      [ "name" .= docName doc,
        "fields" .= mapToFieldsJSON (docFields doc)
      ]

-- ---------------------------------------------------------------------------
-- Errors
-- ---------------------------------------------------------------------------

-- | Errors that can occur during Firestore operations.
data FirestoreError
  = -- | Document does not exist (HTTP 404).
    DocumentNotFound
  | -- | Insufficient permissions (HTTP 403).
    PermissionDenied !Text
  | -- | HTTP or network-level error.
    NetworkError !Text
  | -- | Response could not be decoded as expected.
    InvalidResponse !Text
  | -- | Firestore API error with HTTP status, gRPC status, and message.
    FirestoreApiError !Int !Text !Text
  | -- | Transaction was aborted (contention or conflict).
    TransactionAborted !Text
  deriving (Eq, Show)
