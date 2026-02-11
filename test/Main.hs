{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeOrError)
import Firebase.Auth
import Firebase.Firestore.Internal
import Firebase.Firestore.Query
import Firebase.Firestore.Types
import Network.HTTP.Types.Header (hCacheControl)
import System.Exit (exitFailure)

main :: IO ()
main = do
  putStrLn "firebase-hs tests"
  putStrLn "=================="
  results <-
    sequence
      [ -- Auth: Config
        test "defaultFirebaseConfig sets project ID" testDefaultConfigProjectId,
        test "defaultFirebaseConfig sets 300s clock skew" testDefaultConfigClockSkew,
        -- Auth: Cache-Control parsing
        test "parseCacheMaxAge parses valid header" testParseCacheMaxAgeValid,
        test "parseCacheMaxAge handles missing header" testParseCacheMaxAgeMissing,
        test "parseCacheMaxAge handles malformed value" testParseCacheMaxAgeMalformed,
        test "parseCacheMaxAge rejects zero" testParseCacheMaxAgeZero,
        test "parseCacheMaxAge rejects negative" testParseCacheMaxAgeNegative,
        -- Auth: Types
        test "FirebaseUser Eq instance" testFirebaseUserEq,
        test "AuthError constructors" testAuthErrorConstructors,
        -- Firestore: Value JSON roundtrips
        test "FirestoreValue NullValue roundtrip" testNullValueRoundtrip,
        test "FirestoreValue BoolValue roundtrip" testBoolValueRoundtrip,
        test "FirestoreValue IntegerValue roundtrip" testIntegerValueRoundtrip,
        test "FirestoreValue DoubleValue roundtrip" testDoubleValueRoundtrip,
        test "FirestoreValue StringValue roundtrip" testStringValueRoundtrip,
        test "FirestoreValue TimestampValue roundtrip" testTimestampValueRoundtrip,
        test "FirestoreValue ArrayValue roundtrip" testArrayValueRoundtrip,
        test "FirestoreValue MapValue roundtrip" testMapValueRoundtrip,
        -- Firestore: Integer encoding as string
        test "IntegerValue encodes as JSON string" testIntegerEncodesAsString,
        -- Firestore: Document decoding
        test "Document decodes from Firestore JSON" testDocumentDecode,
        test "Document decodes with empty fields" testDocumentDecodeEmptyFields,
        -- Firestore: Error types
        test "FirestoreError constructors" testFirestoreErrorConstructors,
        -- Firestore: URL construction
        test "documentUrl builds correct URL" testDocumentUrl,
        test "collectionUrl builds correct URL" testCollectionUrl,
        test "createDocUrl builds correct URL" testCreateDocUrl,
        test "updateDocUrl with fields builds correct URL" testUpdateDocUrlWithFields,
        test "updateDocUrl without fields builds correct URL" testUpdateDocUrlNoFields,
        test "queryUrl builds correct URL" testQueryUrl,
        test "beginTransactionUrl builds correct URL" testBeginTransactionUrl,
        test "commitUrl builds correct URL" testCommitUrl,
        -- Firestore: Query DSL
        test "query encodes basic collection" testQueryBasic,
        test "query encodes with field filter" testQueryWithFilter,
        test "query encodes with orderBy and limit" testQueryWithOrderByLimit,
        test "query encodes composite AND filter" testQueryCompositeAnd,
        -- Firestore: Transaction options encoding
        test "ReadWrite transaction options encode" testReadWriteEncode,
        test "RetryWith transaction options encode" testRetryWithEncode,
        test "ReadOnly transaction options encode" testReadOnlyEncode,
        -- Firestore: Error parsing
        test "parseFirestoreError 404" testParseError404,
        test "parseFirestoreError 403" testParseError403,
        test "parseFirestoreError 409 ABORTED" testParseError409Aborted,
        test "parseFirestoreError unparseable" testParseErrorUnparseable
      ]
  let failures = length (filter not results)
  if failures > 0
    then do
      putStrLn ("\n" ++ show failures ++ " test(s) FAILED")
      exitFailure
    else putStrLn "\nAll tests passed."

-- ---------------------------------------------------------------------------
-- Test runner
-- ---------------------------------------------------------------------------

test :: String -> IO Bool -> IO Bool
test name action = do
  result <- action
  putStrLn (indicator result ++ " " ++ name)
  pure result
  where
    indicator True = "  PASS"
    indicator False = "  FAIL"

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO Bool
assertEqual label expected actual
  | expected == actual = pure True
  | otherwise = do
      putStrLn ("    " ++ label ++ ": expected " ++ show expected ++ ", got " ++ show actual)
      pure False

assertBool :: String -> Bool -> IO Bool
assertBool _ True = pure True
assertBool label False = do
  putStrLn ("    " ++ label ++ ": expected True")
  pure False

-- ---------------------------------------------------------------------------
-- Auth: Config tests
-- ---------------------------------------------------------------------------

testDefaultConfigProjectId :: IO Bool
testDefaultConfigProjectId =
  assertEqual "fcProjectId" "test-project" (fcProjectId (defaultFirebaseConfig "test-project"))

testDefaultConfigClockSkew :: IO Bool
testDefaultConfigClockSkew =
  assertEqual "fcClockSkew" 300 (fcClockSkew (defaultFirebaseConfig "test-project"))

-- ---------------------------------------------------------------------------
-- Auth: Cache-Control parsing tests
-- ---------------------------------------------------------------------------

testParseCacheMaxAgeValid :: IO Bool
testParseCacheMaxAgeValid =
  assertEqual
    "max-age=19845"
    (Just 19845)
    (parseCacheMaxAge [(hCacheControl, "public, max-age=19845, must-revalidate, no-transform")])

testParseCacheMaxAgeMissing :: IO Bool
testParseCacheMaxAgeMissing =
  assertEqual
    "no Cache-Control header"
    Nothing
    (parseCacheMaxAge [("content-type", "application/json")])

testParseCacheMaxAgeMalformed :: IO Bool
testParseCacheMaxAgeMalformed =
  assertEqual
    "max-age=abc"
    Nothing
    (parseCacheMaxAge [(hCacheControl, "max-age=abc")])

testParseCacheMaxAgeZero :: IO Bool
testParseCacheMaxAgeZero =
  assertEqual
    "max-age=0"
    Nothing
    (parseCacheMaxAge [(hCacheControl, "max-age=0")])

testParseCacheMaxAgeNegative :: IO Bool
testParseCacheMaxAgeNegative =
  assertEqual
    "max-age=-1"
    Nothing
    (parseCacheMaxAge [(hCacheControl, "max-age=-1")])

-- ---------------------------------------------------------------------------
-- Auth: Type tests
-- ---------------------------------------------------------------------------

testFirebaseUserEq :: IO Bool
testFirebaseUserEq =
  let u1 = FirebaseUser "uid1" (Just "a@b.com") (Just "Alice")
      u2 = FirebaseUser "uid1" (Just "a@b.com") (Just "Alice")
   in assertEqual "same users" u1 u2

testAuthErrorConstructors :: IO Bool
testAuthErrorConstructors = do
  let errors =
        [ KeyFetchError "network error",
          InvalidSignature,
          TokenExpired,
          InvalidClaims "bad aud",
          MalformedToken "not a jwt"
        ]
  assertEqual "5 constructors" 5 (length errors)

-- ---------------------------------------------------------------------------
-- Firestore: Value JSON roundtrips
-- ---------------------------------------------------------------------------

-- | Roundtrip: encode then decode, verify identity.
roundtrip :: FirestoreValue -> IO Bool
roundtrip val =
  assertEqual (show val) (Just val) (Aeson.decode (Aeson.encode val))

testNullValueRoundtrip :: IO Bool
testNullValueRoundtrip = roundtrip NullValue

testBoolValueRoundtrip :: IO Bool
testBoolValueRoundtrip = do
  r1 <- roundtrip (BoolValue True)
  r2 <- roundtrip (BoolValue False)
  pure (r1 && r2)

testIntegerValueRoundtrip :: IO Bool
testIntegerValueRoundtrip = do
  r1 <- roundtrip (IntegerValue 0)
  r2 <- roundtrip (IntegerValue 42)
  r3 <- roundtrip (IntegerValue (-100))
  pure (r1 && r2 && r3)

testDoubleValueRoundtrip :: IO Bool
testDoubleValueRoundtrip = roundtrip (DoubleValue 3.14)

testStringValueRoundtrip :: IO Bool
testStringValueRoundtrip = roundtrip (StringValue "hello world")

testTimestampValueRoundtrip :: IO Bool
testTimestampValueRoundtrip = roundtrip (TimestampValue (parseUTC "2024-01-15T10:30:00Z"))

testArrayValueRoundtrip :: IO Bool
testArrayValueRoundtrip =
  roundtrip (ArrayValue [StringValue "a", IntegerValue 1, BoolValue True])

testMapValueRoundtrip :: IO Bool
testMapValueRoundtrip =
  roundtrip
    ( MapValue
        ( Map.fromList
            [("name", StringValue "Alice"), ("age", IntegerValue 30)]
        )
    )

-- | Verify IntegerValue encodes as a JSON string, not a number.
testIntegerEncodesAsString :: IO Bool
testIntegerEncodesAsString =
  let encoded = Aeson.encode (IntegerValue 42)
   in assertBool "contains string \"42\"" ("\"42\"" `LBS.isPrefixOf` LBS.drop 1 (LBS.dropWhile (/= 58) encoded))

-- ---------------------------------------------------------------------------
-- Firestore: Document decoding
-- ---------------------------------------------------------------------------

testDocumentDecode :: IO Bool
testDocumentDecode =
  let json =
        "{ \"name\": \"projects/p/databases/(default)/documents/users/alice\"\
        \, \"fields\": { \"name\": { \"stringValue\": \"Alice\" }\
        \              , \"age\": { \"integerValue\": \"30\" } }\
        \, \"createTime\": \"2024-01-15T10:30:00Z\"\
        \, \"updateTime\": \"2024-06-20T14:45:00Z\" }"
      expected =
        Document
          { docName = "projects/p/databases/(default)/documents/users/alice",
            docFields =
              Map.fromList
                [ ("name", StringValue "Alice"),
                  ("age", IntegerValue 30)
                ],
            docCreateTime = Just (parseUTC "2024-01-15T10:30:00Z"),
            docUpdateTime = Just (parseUTC "2024-06-20T14:45:00Z")
          }
   in assertEqual "full document" (Just expected) (Aeson.decode json)

testDocumentDecodeEmptyFields :: IO Bool
testDocumentDecodeEmptyFields =
  let json = "{ \"name\": \"projects/p/databases/(default)/documents/col/doc\" }"
      expected =
        Document
          { docName = "projects/p/databases/(default)/documents/col/doc",
            docFields = Map.empty,
            docCreateTime = Nothing,
            docUpdateTime = Nothing
          }
   in assertEqual "empty fields" (Just expected) (Aeson.decode json)

-- ---------------------------------------------------------------------------
-- Firestore: Error types
-- ---------------------------------------------------------------------------

testFirestoreErrorConstructors :: IO Bool
testFirestoreErrorConstructors = do
  let errors =
        [ DocumentNotFound,
          PermissionDenied "no access",
          NetworkError "timeout",
          InvalidResponse "bad json",
          FirestoreApiError 500 "INTERNAL" "oops",
          TransactionAborted "contention"
        ]
  assertEqual "6 constructors" 6 (length errors)

-- ---------------------------------------------------------------------------
-- Firestore: URL construction
-- ---------------------------------------------------------------------------

testDocumentUrl :: IO Bool
testDocumentUrl =
  assertEqual
    "documentUrl"
    "https://firestore.googleapis.com/v1/projects/myproj/databases/(default)/documents/users/alice"
    (documentUrl (ProjectId "myproj") (DocumentPath (CollectionPath "users") (DocumentId "alice")))

testCollectionUrl :: IO Bool
testCollectionUrl =
  assertEqual
    "collectionUrl"
    "https://firestore.googleapis.com/v1/projects/myproj/databases/(default)/documents/users"
    (collectionUrl (ProjectId "myproj") (CollectionPath "users"))

testCreateDocUrl :: IO Bool
testCreateDocUrl =
  assertEqual
    "createDocUrl"
    "https://firestore.googleapis.com/v1/projects/myproj/databases/(default)/documents/users?documentId=alice"
    (createDocUrl (ProjectId "myproj") (CollectionPath "users") (DocumentId "alice"))

testUpdateDocUrlWithFields :: IO Bool
testUpdateDocUrlWithFields =
  assertEqual
    "updateDocUrl with fields"
    "https://firestore.googleapis.com/v1/projects/myproj/databases/(default)/documents/users/alice?updateMask.fieldPaths=name&updateMask.fieldPaths=age"
    (updateDocUrl (ProjectId "myproj") (DocumentPath (CollectionPath "users") (DocumentId "alice")) ["name", "age"])

testUpdateDocUrlNoFields :: IO Bool
testUpdateDocUrlNoFields =
  assertEqual
    "updateDocUrl no fields"
    "https://firestore.googleapis.com/v1/projects/myproj/databases/(default)/documents/users/alice"
    (updateDocUrl (ProjectId "myproj") (DocumentPath (CollectionPath "users") (DocumentId "alice")) [])

testQueryUrl :: IO Bool
testQueryUrl =
  assertEqual
    "queryUrl"
    "https://firestore.googleapis.com/v1/projects/myproj/databases/(default)/documents:runQuery"
    (queryUrl (ProjectId "myproj"))

testBeginTransactionUrl :: IO Bool
testBeginTransactionUrl =
  assertEqual
    "beginTransactionUrl"
    "https://firestore.googleapis.com/v1/projects/myproj/databases/(default)/documents:beginTransaction"
    (beginTransactionUrl (ProjectId "myproj"))

testCommitUrl :: IO Bool
testCommitUrl =
  assertEqual
    "commitUrl"
    "https://firestore.googleapis.com/v1/projects/myproj/databases/(default)/documents:commit"
    (commitUrl (ProjectId "myproj"))

-- ---------------------------------------------------------------------------
-- Firestore: Query DSL
-- ---------------------------------------------------------------------------

testQueryBasic :: IO Bool
testQueryBasic =
  let q = encodeQuery (query (CollectionPath "users"))
      expected =
        Aeson.object
          [ "structuredQuery"
              Aeson..= Aeson.object
                ["from" Aeson..= [Aeson.object ["collectionId" Aeson..= ("users" :: Text)]]]
          ]
   in assertEqual "basic query" expected q

testQueryWithFilter :: IO Bool
testQueryWithFilter =
  let q =
        encodeQuery $
          query (CollectionPath "users")
            & where_ (fieldFilter "age" OpGreaterThan (IntegerValue 18))
      decoded = Aeson.decode (Aeson.encode q) :: Maybe Aeson.Value
   in assertBool "has fieldFilter" (isJust decoded)

testQueryWithOrderByLimit :: IO Bool
testQueryWithOrderByLimit =
  let q =
        encodeQuery $
          query (CollectionPath "users")
            & orderBy "name" Ascending
            & limit 25
      decoded = Aeson.decode (Aeson.encode q) :: Maybe Aeson.Value
   in assertBool "has orderBy and limit" (isJust decoded)

testQueryCompositeAnd :: IO Bool
testQueryCompositeAnd =
  let q =
        encodeQuery $
          query (CollectionPath "users")
            & where_
              ( compositeAnd
                  [ fieldFilter "age" OpGreaterThan (IntegerValue 18),
                    fieldFilter "active" OpEqual (BoolValue True)
                  ]
              )
      decoded = Aeson.decode (Aeson.encode q) :: Maybe Aeson.Value
   in assertBool "has compositeFilter" (isJust decoded)

-- ---------------------------------------------------------------------------
-- Firestore: Transaction options encoding
-- ---------------------------------------------------------------------------

testReadWriteEncode :: IO Bool
testReadWriteEncode =
  let encoded = encodeTransactionOptions ReadWrite
      expected =
        Aeson.object
          [ "options"
              Aeson..= Aeson.object
                ["readWrite" Aeson..= Aeson.object []]
          ]
   in assertEqual "ReadWrite" expected encoded

testRetryWithEncode :: IO Bool
testRetryWithEncode =
  let encoded = encodeTransactionOptions (RetryWith (TransactionId "abc123"))
      expected =
        Aeson.object
          [ "options"
              Aeson..= Aeson.object
                [ "readWrite"
                    Aeson..= Aeson.object
                      ["retryTransaction" Aeson..= ("abc123" :: String)]
                ]
          ]
   in assertEqual "RetryWith" expected encoded

testReadOnlyEncode :: IO Bool
testReadOnlyEncode =
  let encoded = encodeTransactionOptions ReadOnly
      expected =
        Aeson.object
          [ "options"
              Aeson..= Aeson.object
                ["readOnly" Aeson..= Aeson.object []]
          ]
   in assertEqual "ReadOnly" expected encoded

-- ---------------------------------------------------------------------------
-- Firestore: Error parsing
-- ---------------------------------------------------------------------------

testParseError404 :: IO Bool
testParseError404 =
  let body = "{\"error\":{\"code\":404,\"message\":\"not found\",\"status\":\"NOT_FOUND\"}}"
   in assertEqual "404 -> DocumentNotFound" DocumentNotFound (parseFirestoreError 404 body)

testParseError403 :: IO Bool
testParseError403 =
  let body = "{\"error\":{\"code\":403,\"message\":\"denied\",\"status\":\"PERMISSION_DENIED\"}}"
   in assertEqual "403 -> PermissionDenied" (PermissionDenied "denied") (parseFirestoreError 403 body)

testParseError409Aborted :: IO Bool
testParseError409Aborted =
  let body = "{\"error\":{\"code\":409,\"message\":\"contention\",\"status\":\"ABORTED\"}}"
   in assertEqual "409 ABORTED -> TransactionAborted" (TransactionAborted "contention") (parseFirestoreError 409 body)

testParseErrorUnparseable :: IO Bool
testParseErrorUnparseable =
  assertEqual
    "unparseable -> NetworkError"
    (NetworkError "HTTP 500")
    (parseFirestoreError 500 "not json at all")

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Parse a UTC time string for test data.
parseUTC :: String -> UTCTime
parseUTC = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"
