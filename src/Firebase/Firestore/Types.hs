-- |
-- Module      : Firebase.Firestore.Types
-- Description : Types for Firestore REST API
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

    -- * Handle
    Firestore (..),

    -- * Values
    FirestoreValue (..),
    GeoPoint (..),

    -- * Documents
    Document (..),

    -- * Errors
    FirestoreError (..),

    -- * Transactions
    TransactionId (..),
    TransactionMode (..),
    encodeTransactionOptions,

    -- * Transaction Writes
    Write (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Network.HTTP.Client (Manager)

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
  deriving (Eq)

-- | Redacted to prevent credential leakage in logs and error messages.
instance Show AccessToken where
  show _ = "AccessToken <redacted>"

-- | Full path to a document: collection + document ID.
data DocumentPath = DocumentPath
  { dpCollection :: !CollectionPath,
    dpDocument :: !DocumentId
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Handle
-- ---------------------------------------------------------------------------

-- | Everything a Firestore operation needs: a connection pool, the project
-- to address, and the credentials to present.
--
-- Build one with 'Firebase.Firestore.newFirestore' and pass it to every
-- call. Access tokens expire, so refresh one with
-- 'Firebase.Firestore.withToken' rather than rebuilding the manager.
data Firestore = Firestore
  { -- | Connection manager, reused across requests.
    fsManager :: !Manager,
    -- | Project every path is resolved against.
    fsProject :: !ProjectId,
    -- | OAuth2 credentials presented on each request.
    fsToken :: !AccessToken
  }

-- ---------------------------------------------------------------------------
-- Transactions
-- ---------------------------------------------------------------------------

-- | Opaque transaction identifier returned by Firestore.
newtype TransactionId = TransactionId {unTransactionId :: Text}
  deriving (Eq)

-- | Redacted to prevent token leakage in logs and error messages.
instance Show TransactionId where
  show _ = "TransactionId <redacted>"

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

-- | A single write in a transaction commit, already in Firestore's wire
-- form.
--
-- Build one with 'Firebase.Firestore.mkUpdateWrite' or
-- 'Firebase.Firestore.mkDeleteWrite'. The constructor is visible so the
-- library can assemble writes, and is not part of the supported API.
newtype Write = Write {unWrite :: Aeson.Value}
  deriving (Eq, Show)

instance ToJSON Write where
  toJSON = unWrite

-- ---------------------------------------------------------------------------
-- Firestore values
-- ---------------------------------------------------------------------------

-- | A geographic point, as carried by Firestore's @geoPointValue@.
data GeoPoint = GeoPoint
  { gpLatitude :: !Double,
    gpLongitude :: !Double
  }
  deriving (Eq, Show)

instance ToJSON GeoPoint where
  toJSON gp =
    Aeson.object
      [ "latitude" .= gpLatitude gp,
        "longitude" .= gpLongitude gp
      ]

instance FromJSON GeoPoint where
  parseJSON = Aeson.withObject "geoPointValue" $ \o ->
    -- Firestore omits a coordinate that is exactly zero.
    GeoPoint <$> o .:? "latitude" .!= 0 <*> o .:? "longitude" .!= 0

-- | A Firestore value, mirroring the tagged JSON wire format.
--
-- Covers every type Firestore stores. Integers are transmitted as JSON
-- strings (e.g. @{\"integerValue\":\"42\"}@) and bytes as base64; the
-- 'FromJSON' \/ 'ToJSON' instances handle both.
data FirestoreValue
  = NullValue
  | BoolValue !Bool
  | IntegerValue !Int64
  | DoubleValue !Double
  | StringValue !Text
  | -- | Raw bytes, transmitted base64-encoded.
    BytesValue !BS.ByteString
  | -- | Full resource name of another document, as built by
    -- 'Firebase.Firestore.Internal.documentResourceName'.
    ReferenceValue !Text
  | GeoPointValue !GeoPoint
  | TimestampValue !UTCTime
  | ArrayValue ![FirestoreValue]
  | MapValue !(Map Text FirestoreValue)
  deriving (Eq, Show)

-- | Firestore's RFC 3339 timestamp format.
timestampFormat :: String
timestampFormat = "%Y-%m-%dT%H:%M:%S%QZ"

-- | The proto3 JSON spellings of the doubles a JSON number cannot carry.
nanLiteral, positiveInfinityLiteral, negativeInfinityLiteral :: Text
nanLiteral = "NaN"
positiveInfinityLiteral = "Infinity"
negativeInfinityLiteral = "-Infinity"

-- | The IEEE 754 values behind those spellings.
notANumber, positiveInfinity :: Double
notANumber = 0 / 0
positiveInfinity = 1 / 0

-- | Encode a double the way proto3 JSON does: non-finite values travel as
-- strings, everything else as a plain JSON number.
encodeDouble :: Double -> Aeson.Value
encodeDouble d
  | isNaN d = Aeson.String nanLiteral
  | isInfinite d, d > 0 = Aeson.String positiveInfinityLiteral
  | isInfinite d = Aeson.String negativeInfinityLiteral
  | otherwise = Aeson.toJSON d

-- | Parse a double, accepting the proto3 string spellings of the
-- non-finite values alongside plain JSON numbers.
parseDoubleValue :: Aeson.Value -> Parser Double
parseDoubleValue (Aeson.String t)
  | t == nanLiteral = pure notANumber
  | t == positiveInfinityLiteral = pure positiveInfinity
  | t == negativeInfinityLiteral = pure (negate positiveInfinity)
  | otherwise = fail ("invalid doubleValue: " ++ T.unpack t)
parseDoubleValue v = parseJSON v

instance ToJSON FirestoreValue where
  toJSON NullValue = Aeson.object ["nullValue" .= Aeson.Null]
  toJSON (BoolValue b) = Aeson.object ["booleanValue" .= b]
  toJSON (IntegerValue n) = Aeson.object ["integerValue" .= show n]
  toJSON (DoubleValue d) = Aeson.object ["doubleValue" .= encodeDouble d]
  toJSON (StringValue s) = Aeson.object ["stringValue" .= s]
  toJSON (BytesValue bs) = Aeson.object ["bytesValue" .= TE.decodeUtf8 (B64.encode bs)]
  toJSON (ReferenceValue name) = Aeson.object ["referenceValue" .= name]
  toJSON (GeoPointValue gp) = Aeson.object ["geoPointValue" .= gp]
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
      [("doubleValue", v)] -> DoubleValue <$> parseDoubleValue v
      [("stringValue", v)] -> StringValue <$> parseJSON v
      [("bytesValue", v)] -> BytesValue <$> parseBytesValue v
      [("referenceValue", v)] -> ReferenceValue <$> parseJSON v
      [("geoPointValue", v)] -> GeoPointValue <$> parseJSON v
      [("timestampValue", v)] -> TimestampValue <$> parseTimestamp v
      [("arrayValue", v)] -> ArrayValue <$> parseArrayValue v
      [("mapValue", v)] -> MapValue <$> parseMapValue v
      tagged -> fail ("unrecognized FirestoreValue tag: " ++ show (map fst tagged))

-- | Parse a bytes value from its base64 representation.
parseBytesValue :: Aeson.Value -> Parser BS.ByteString
parseBytesValue = Aeson.withText "bytesValue" $ \t ->
  either (fail . ("invalid bytesValue: " ++)) pure (B64.decode (TE.encodeUtf8 t))

-- | Parse an integer value from a JSON string (Firestore's wire format).
-- Uses decimal-only parsing: hex, octal, and other Haskell literals are
-- rejected rather than silently accepted.
parseIntegerValue :: Aeson.Value -> Parser Int64
parseIntegerValue = Aeson.withText "integerValue" $ \t ->
  case TR.signed TR.decimal t of
    Right (n, remaining) | T.null remaining -> pure n
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
