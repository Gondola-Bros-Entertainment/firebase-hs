module Main (main) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.Function ((&))
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeOrError)
import Firebase.Auth
import Firebase.Auth.Internal (bearerToken, padBase64Url, stripBearerPrefix)
import Firebase.Firestore (mkDeleteWrite, mkUpdateWrite)
import Firebase.Firestore.Internal
import Firebase.Firestore.Query
import Firebase.Firestore.Types
import Network.HTTP.Client (Request, parseRequest, requestHeaders)
import Network.HTTP.Types.Header (hAuthorization, hCacheControl)
import System.Exit (exitFailure)

-- ---------------------------------------------------------------------------
-- Test framework
--
-- Every check below is a pure value. IO appears once, in 'main', to report
-- the results and set the exit code.
-- ---------------------------------------------------------------------------

-- | A named check that either holds or explains why it does not.
data Test = Test !String !(Either String ())

-- | Assert that a computed value matches an expected one.
--
-- Fixed at comparison precedence, so the expected side can be built up with
-- '<>' and sequenced with '>>' without parentheses.
infix 4 `shouldBe`

shouldBe :: (Eq a, Show a) => a -> a -> Either String ()
shouldBe actual expected
  | actual == expected = Right ()
  | otherwise = Left ("expected " <> show expected <> ", got " <> show actual)

-- | Assert that a condition holds, describing it if it does not.
shouldHold :: String -> Bool -> Either String ()
shouldHold _ True = Right ()
shouldHold description False = Left ("expected " <> description)

-- | Run every check in order, reporting each and failing the suite if any did.
main :: IO ()
main = do
  putStrLn "firebase-hs tests"
  putStrLn "=================="
  mapM_ (putStrLn . report) tests
  case filter failed tests of
    [] -> putStrLn ("\nAll " <> show (length tests) <> " tests passed.")
    failures -> do
      putStrLn ("\n" <> show (length failures) <> " test(s) FAILED")
      exitFailure
  where
    failed (Test _ result) = either (const True) (const False) result

    report (Test name (Right ())) = "  PASS " <> name
    report (Test name (Left reason)) = "  FAIL " <> name <> "\n    " <> reason

-- ---------------------------------------------------------------------------
-- Test registry
-- ---------------------------------------------------------------------------

tests :: [Test]
tests =
  concat
    [ authConfigTests,
      cacheControlTests,
      authErrorTests,
      claimTests,
      bearerTokenTests,
      base64Tests,
      valueRoundtripTests,
      documentTests,
      urlTests,
      urlEncodingTests,
      resourceNameTests,
      writeTests,
      queryTests,
      transactionOptionTests,
      requestTests,
      errorParsingTests,
      decoderTests
    ]

-- ---------------------------------------------------------------------------
-- Auth: Config
-- ---------------------------------------------------------------------------

authConfigTests :: [Test]
authConfigTests =
  [ Test "defaultFirebaseConfig sets project ID" $
      fcProjectId (defaultFirebaseConfig "test-project") `shouldBe` "test-project",
    Test "defaultFirebaseConfig sets 300s clock skew" $
      fcClockSkew (defaultFirebaseConfig "test-project") `shouldBe` 300
  ]

-- ---------------------------------------------------------------------------
-- Auth: Cache-Control parsing
-- ---------------------------------------------------------------------------

cacheControlTests :: [Test]
cacheControlTests =
  [ Test "parseCacheMaxAge parses valid header" $
      parseCacheMaxAge [(hCacheControl, "public, max-age=19845, must-revalidate, no-transform")]
        `shouldBe` Just 19845,
    Test "parseCacheMaxAge handles missing header" $
      parseCacheMaxAge [("content-type", "application/json")] `shouldBe` Nothing,
    Test "parseCacheMaxAge handles malformed value" $
      parseCacheMaxAge [(hCacheControl, "max-age=abc")] `shouldBe` Nothing,
    Test "parseCacheMaxAge rejects zero" $
      parseCacheMaxAge [(hCacheControl, "max-age=0")] `shouldBe` Nothing,
    Test "parseCacheMaxAge rejects negative" $
      parseCacheMaxAge [(hCacheControl, "max-age=-1")] `shouldBe` Nothing,
    Test "parseCacheMaxAge ignores unrelated directives" $
      parseCacheMaxAge [(hCacheControl, "no-store, must-revalidate")] `shouldBe` Nothing,
    Test "parseCacheMaxAge reads a directive in final position" $
      parseCacheMaxAge [(hCacheControl, "public, max-age=60")] `shouldBe` Just 60
  ]

-- ---------------------------------------------------------------------------
-- Auth: Errors
-- ---------------------------------------------------------------------------

-- | A representative verified user, for checks that vary one field.
testUser :: FirebaseUser
testUser =
  FirebaseUser
    { fuUid = "uid1",
      fuEmail = Just "a@b.com",
      fuEmailVerified = True,
      fuName = Just "Alice",
      fuPicture = Nothing,
      fuAuthTime = Nothing,
      fuSignInProvider = Just "password",
      fuCustomClaims = Map.fromList [("admin", Aeson.Bool True), ("tier", Aeson.String "gold")]
    }

authErrorTests :: [Test]
authErrorTests =
  [ Test "FirebaseUser Eq instance" $
      testUser `shouldBe` testUser,
    Test "FirebaseUser Eq distinguishes custom claims" $
      shouldHold "differing claims compare unequal" $
        testUser /= testUser {fuCustomClaims = Map.empty},
    Test "AuthError constructors" $
      length
        [ KeyFetchError "network error",
          InvalidSignature,
          TokenExpired,
          InvalidClaims "bad aud",
          MalformedToken "not a jwt"
        ]
        `shouldBe` 5,
    Test "authErrorMessage withholds the key-fetch detail" $
      authErrorMessage (KeyFetchError "connect to 10.0.0.1 refused")
        `shouldBe` "Authentication service unavailable",
    Test "authErrorMessage withholds which claim failed" $
      authErrorMessage (InvalidClaims "audience mismatch") `shouldBe` "Invalid token claims",
    Test "authErrorMessage withholds the parse detail" $
      authErrorMessage (MalformedToken "header: invalid base64") `shouldBe` "Malformed token",
    Test "authErrorMessage distinguishes signature from expiry" $
      shouldHold "different messages" $
        authErrorMessage InvalidSignature /= authErrorMessage TokenExpired
  ]

-- ---------------------------------------------------------------------------
-- Auth: Custom claims
-- ---------------------------------------------------------------------------

claimTests :: [Test]
claimTests =
  [ Test "lookupClaim finds a custom claim" $
      lookupClaim "tier" testUser `shouldBe` Just (Aeson.String "gold"),
    Test "lookupClaim misses an absent claim" $
      lookupClaim "nope" testUser `shouldBe` Nothing,
    Test "lookupClaim does not expose reserved claims" $
      lookupClaim "email" testUser `shouldBe` Nothing,
    Test "hasClaim accepts a true claim" $
      shouldHold "admin holds" (hasClaim "admin" testUser),
    Test "hasClaim rejects an absent claim" $
      shouldHold "absent claim is false" (not (hasClaim "superuser" testUser)),
    Test "hasClaim rejects a non-boolean claim" $
      shouldHold "gold is not true" (not (hasClaim "tier" testUser)),
    Test "hasClaim rejects a false claim" $
      shouldHold "explicit false" $
        not (hasClaim "admin" testUser {fuCustomClaims = Map.fromList [("admin", Aeson.Bool False)]})
  ]

-- ---------------------------------------------------------------------------
-- Auth: Bearer token extraction
-- ---------------------------------------------------------------------------

bearerTokenTests :: [Test]
bearerTokenTests =
  [ Test "stripBearerPrefix accepts the canonical scheme" $
      stripBearerPrefix "Bearer abc.def.ghi" `shouldBe` Just "abc.def.ghi",
    Test "stripBearerPrefix accepts a lowercase scheme" $
      stripBearerPrefix "bearer abc.def.ghi" `shouldBe` Just "abc.def.ghi",
    Test "stripBearerPrefix accepts an uppercase scheme" $
      stripBearerPrefix "BEARER abc.def.ghi" `shouldBe` Just "abc.def.ghi",
    Test "stripBearerPrefix rejects another scheme" $
      stripBearerPrefix "Basic dXNlcjpwYXNz" `shouldBe` Nothing,
    Test "stripBearerPrefix rejects a bare token" $
      stripBearerPrefix "abc.def.ghi" `shouldBe` Nothing,
    Test "stripBearerPrefix rejects an empty header" $
      stripBearerPrefix "" `shouldBe` Nothing,
    Test "stripBearerPrefix keeps an empty credential" $
      stripBearerPrefix "Bearer " `shouldBe` Just "",
    Test "bearerToken reads the Authorization header" $
      bearerToken [(hAuthorization, "Bearer token123")] `shouldBe` Just "token123",
    Test "bearerToken matches the header name case-insensitively" $
      bearerToken [("authorization", "Bearer token123")] `shouldBe` Just "token123",
    Test "bearerToken ignores other headers" $
      bearerToken [("content-type", "application/json")] `shouldBe` Nothing,
    Test "bearerToken rejects a malformed scheme" $
      bearerToken [(hAuthorization, "Bearer2 token123")] `shouldBe` Nothing
  ]

-- ---------------------------------------------------------------------------
-- Auth: base64url padding
-- ---------------------------------------------------------------------------

base64Tests :: [Test]
base64Tests =
  [ Test "padBase64Url leaves a whole group alone" $
      padBase64Url "YWJjZA==" `shouldBe` "YWJjZA==",
    Test "padBase64Url pads a two-character remainder" $
      padBase64Url "YWJjZA" `shouldBe` "YWJjZA==",
    Test "padBase64Url pads a three-character remainder" $
      padBase64Url "YWJjZGU" `shouldBe` "YWJjZGU=",
    Test "padBase64Url leaves empty input alone" $
      padBase64Url "" `shouldBe` "",
    Test "padBase64Url always yields a whole number of groups" $
      shouldHold "every padded length divisible by 4" $
        all (\s -> BS.length (padBase64Url s) `rem` 4 == 0) ["", "a", "ab", "abc", "abcd", "abcde"]
  ]

-- ---------------------------------------------------------------------------
-- Firestore: Value JSON roundtrips
-- ---------------------------------------------------------------------------

valueRoundtripTests :: [Test]
valueRoundtripTests =
  [ Test "FirestoreValue NullValue roundtrip" (roundtrip NullValue),
    Test "FirestoreValue BoolValue roundtrip" (roundtrip (BoolValue True) >> roundtrip (BoolValue False)),
    Test "FirestoreValue IntegerValue roundtrip" $
      roundtrip (IntegerValue 0) >> roundtrip (IntegerValue 42) >> roundtrip (IntegerValue (-100)),
    Test "FirestoreValue DoubleValue roundtrip" (roundtrip (DoubleValue 3.14)),
    Test "FirestoreValue StringValue roundtrip" (roundtrip (StringValue "hello world")),
    Test "FirestoreValue TimestampValue roundtrip" $
      roundtrip (TimestampValue (parseUTC "2024-01-15T10:30:00Z")),
    Test "FirestoreValue ArrayValue roundtrip" $
      roundtrip (ArrayValue [StringValue "a", IntegerValue 1, BoolValue True]),
    Test "FirestoreValue MapValue roundtrip" $
      roundtrip (MapValue (Map.fromList [("name", StringValue "Alice"), ("age", IntegerValue 30)])),
    Test "FirestoreValue BytesValue roundtrip" $
      roundtrip (BytesValue "\0\1\2binary\255"),
    Test "FirestoreValue ReferenceValue roundtrip" $
      roundtrip (ReferenceValue "projects/p/databases/(default)/documents/users/alice"),
    Test "FirestoreValue GeoPointValue roundtrip" $
      roundtrip (GeoPointValue (GeoPoint 51.5 (-0.12))),
    Test "BytesValue encodes as base64" $
      Aeson.encode (BytesValue "hello")
        `shouldBe` Aeson.encode (Aeson.object ["bytesValue" Aeson..= ("aGVsbG8=" :: Text)]),
    Test "BytesValue rejects invalid base64" $
      (Aeson.decode "{\"bytesValue\":\"not!base64\"}" :: Maybe FirestoreValue) `shouldBe` Nothing,
    Test "GeoPoint defaults an omitted coordinate to zero" $
      (Aeson.decode "{\"geoPointValue\":{\"latitude\":10.5}}" :: Maybe FirestoreValue)
        `shouldBe` Just (GeoPointValue (GeoPoint 10.5 0)),
    Test "a document carrying a geo point decodes" $
      fmap
        docFields
        ( Aeson.decode
            "{\"name\":\"projects/p/databases/(default)/documents/c/d\"\
            \,\"fields\":{\"where\":{\"geoPointValue\":{\"latitude\":1,\"longitude\":2}}}}"
        )
        `shouldBe` Just (Map.fromList [("where", GeoPointValue (GeoPoint 1 2))]),
    Test "an unknown value tag is named in the error" $
      (Aeson.decode "{\"nonsenseValue\":1}" :: Maybe FirestoreValue) `shouldBe` Nothing,
    Test "IntegerValue encodes as a JSON string" $
      Aeson.encode (IntegerValue 42)
        `shouldBe` Aeson.encode (Aeson.object ["integerValue" Aeson..= ("42" :: Text)]),
    Test "IntegerValue rejects a hexadecimal literal" $
      (Aeson.decode "{\"integerValue\":\"0x2A\"}" :: Maybe FirestoreValue) `shouldBe` Nothing,
    Test "IntegerValue rejects trailing characters" $
      (Aeson.decode "{\"integerValue\":\"42abc\"}" :: Maybe FirestoreValue) `shouldBe` Nothing,
    Test "DoubleValue roundtrips positive infinity" $
      roundtrip (DoubleValue (1 / 0)),
    Test "DoubleValue encodes NaN as the proto3 string" $
      Aeson.encode (DoubleValue (0 / 0))
        `shouldBe` Aeson.encode (Aeson.object ["doubleValue" Aeson..= ("NaN" :: Text)]),
    Test "DoubleValue encodes negative infinity as the proto3 string" $
      Aeson.encode (DoubleValue (negate (1 / 0)))
        `shouldBe` Aeson.encode (Aeson.object ["doubleValue" Aeson..= ("-Infinity" :: Text)]),
    Test "DoubleValue decodes the proto3 NaN string" $
      case Aeson.decode "{\"doubleValue\":\"NaN\"}" of
        Just (DoubleValue d) -> shouldHold "NaN decodes to NaN" (isNaN d)
        other -> Left ("expected a DoubleValue, got " <> show other),
    Test "DoubleValue rejects an unknown string spelling" $
      (Aeson.decode "{\"doubleValue\":\"fast\"}" :: Maybe FirestoreValue) `shouldBe` Nothing,
    Test "DoubleValue rejects JSON null" $
      (Aeson.decode "{\"doubleValue\":null}" :: Maybe FirestoreValue) `shouldBe` Nothing,
    Test "IntegerValue parses the Int64 bounds" $
      Aeson.decode "{\"integerValue\":\"9223372036854775807\"}"
        `shouldBe` Just (IntegerValue maxBound)
        >> Aeson.decode "{\"integerValue\":\"-9223372036854775808\"}"
          `shouldBe` Just (IntegerValue minBound),
    Test "IntegerValue rejects a value past the Int64 bounds" $
      (Aeson.decode "{\"integerValue\":\"9223372036854775808\"}" :: Maybe FirestoreValue)
        `shouldBe` Nothing
        >> (Aeson.decode "{\"integerValue\":\"-9223372036854775809\"}" :: Maybe FirestoreValue)
          `shouldBe` Nothing,
    Test "TimestampValue caps encoded precision at nine fractional digits" $
      Aeson.encode (TimestampValue (parseUTCSubsecond "2024-01-15T10:30:00.123456789012Z"))
        `shouldBe` Aeson.encode
          (Aeson.object ["timestampValue" Aeson..= ("2024-01-15T10:30:00.123456789Z" :: Text)])
  ]

-- | Encode then decode, and confirm nothing was lost.
roundtrip :: FirestoreValue -> Either String ()
roundtrip val = Aeson.decode (Aeson.encode val) `shouldBe` Just val

-- ---------------------------------------------------------------------------
-- Firestore: Document decoding
-- ---------------------------------------------------------------------------

documentTests :: [Test]
documentTests =
  [ Test "Document decodes from Firestore JSON" $
      Aeson.decode
        "{ \"name\": \"projects/p/databases/(default)/documents/users/alice\"\
        \, \"fields\": { \"name\": { \"stringValue\": \"Alice\" }\
        \              , \"age\": { \"integerValue\": \"30\" } }\
        \, \"createTime\": \"2024-01-15T10:30:00Z\"\
        \, \"updateTime\": \"2024-06-20T14:45:00Z\" }"
        `shouldBe` Just
          Document
            { docName = "projects/p/databases/(default)/documents/users/alice",
              docFields = Map.fromList [("name", StringValue "Alice"), ("age", IntegerValue 30)],
              docCreateTime = Just (parseUTC "2024-01-15T10:30:00Z"),
              docUpdateTime = Just (parseUTC "2024-06-20T14:45:00Z")
            },
    Test "Document decodes with empty fields" $
      Aeson.decode "{ \"name\": \"projects/p/databases/(default)/documents/col/doc\" }"
        `shouldBe` Just
          Document
            { docName = "projects/p/databases/(default)/documents/col/doc",
              docFields = Map.empty,
              docCreateTime = Nothing,
              docUpdateTime = Nothing
            },
    Test "FirestoreError constructors" $
      length
        [ DocumentNotFound,
          PermissionDenied "no access",
          NetworkError "timeout",
          InvalidResponse "bad json",
          FirestoreApiError 500 "INTERNAL" "oops",
          TransactionAborted "contention"
        ]
        `shouldBe` 6
  ]

-- ---------------------------------------------------------------------------
-- Firestore: URL construction
-- ---------------------------------------------------------------------------

-- | Project used by the URL tests.
testProject :: ProjectId
testProject = ProjectId "myproj"

-- | Prefix every document URL under 'testProject' shares.
testDatabaseUrl :: String
testDatabaseUrl = "https://firestore.googleapis.com/v1/projects/myproj/databases/(default)/documents"

alicePath :: DocumentPath
alicePath = DocumentPath (CollectionPath "users") (DocumentId "alice")

urlTests :: [Test]
urlTests =
  [ Test "databaseUrl builds correct URL" $
      databaseUrl testProject `shouldBe` testDatabaseUrl,
    Test "documentUrl builds correct URL" $
      documentUrl testProject alicePath `shouldBe` testDatabaseUrl <> "/users/alice",
    Test "collectionUrl builds correct URL" $
      collectionUrl testProject (CollectionPath "users") `shouldBe` testDatabaseUrl <> "/users",
    Test "createDocUrl builds correct URL" $
      createDocUrl testProject (CollectionPath "users") (DocumentId "alice")
        `shouldBe` testDatabaseUrl
        <> "/users?documentId=alice",
    Test "updateDocUrl with fields builds correct URL" $
      updateDocUrl testProject alicePath ["name", "age"]
        `shouldBe` testDatabaseUrl
        <> "/users/alice?updateMask.fieldPaths=name&updateMask.fieldPaths=age",
    Test "updateDocUrl without fields builds correct URL" $
      updateDocUrl testProject alicePath [] `shouldBe` testDatabaseUrl <> "/users/alice",
    Test "runQueryUrl posts to the database root for a top-level collection" $
      runQueryUrl testProject (CollectionPath "users") `shouldBe` testDatabaseUrl <> ":runQuery",
    Test "runQueryUrl posts to the parent document for a subcollection" $
      runQueryUrl testProject (CollectionPath "users/abc/posts")
        `shouldBe` testDatabaseUrl
        <> "/users/abc:runQuery",
    Test "beginTransactionUrl builds correct URL" $
      beginTransactionUrl testProject `shouldBe` testDatabaseUrl <> ":beginTransaction",
    Test "commitUrl builds correct URL" $
      commitUrl testProject `shouldBe` testDatabaseUrl <> ":commit",
    Test "rollbackUrl builds correct URL" $
      rollbackUrl testProject `shouldBe` testDatabaseUrl <> ":rollback",
    Test "collectionUrl keeps subcollection separators" $
      collectionUrl testProject (CollectionPath "users/abc/posts")
        `shouldBe` testDatabaseUrl
        <> "/users/abc/posts",
    Test "splitCollectionPath keeps a top-level collection whole" $
      splitCollectionPath (CollectionPath "users") `shouldBe` (Nothing, "users"),
    Test "splitCollectionPath splits a subcollection from its parent" $
      splitCollectionPath (CollectionPath "users/abc/posts") `shouldBe` (Just "users/abc", "posts"),
    Test "splitCollectionPath splits a deep subcollection" $
      splitCollectionPath (CollectionPath "a/b/c/d/e") `shouldBe` (Just "a/b/c/d", "e")
  ]

-- ---------------------------------------------------------------------------
-- Firestore: Percent encoding
--
-- Caller-supplied components reach the URL encoded, so a value carrying a
-- URL delimiter names a document instead of altering the request.
-- ---------------------------------------------------------------------------

urlEncodingTests :: [Test]
urlEncodingTests =
  [ Test "encodePathSegment escapes a path separator" $
      encodePathSegment "a/b" `shouldBe` "a%2Fb",
    Test "encodePathSegment escapes a query introducer" $
      encodePathSegment "a?b" `shouldBe` "a%3Fb",
    Test "encodePathSegment escapes a fragment introducer" $
      encodePathSegment "a#b" `shouldBe` "a%23b",
    Test "encodePathSegment escapes a space" $
      encodePathSegment "a b" `shouldBe` "a%20b",
    Test "encodePathSegment escapes a percent" $
      encodePathSegment "a%b" `shouldBe` "a%25b",
    Test "encodePathSegment encodes non-ASCII as UTF-8" $
      encodePathSegment (T.pack "caf\233") `shouldBe` "caf%C3%A9",
    Test "encodePathSegment leaves unreserved characters alone" $
      encodePathSegment "aZ0.~-_" `shouldBe` "aZ0.~-_",
    Test "encodeQueryValue escapes a parameter separator" $
      encodeQueryValue "a&b" `shouldBe` "a%26b",
    Test "encodeQueryValue escapes an assignment" $
      encodeQueryValue "a=b" `shouldBe` "a%3Db",
    Test "documentUrl escapes a document ID" $
      documentUrl testProject (DocumentPath (CollectionPath "users") (DocumentId "a?b#c"))
        `shouldBe` testDatabaseUrl
        <> "/users/a%3Fb%23c",
    Test "documentUrl escapes a project ID" $
      documentUrl (ProjectId "a b") alicePath
        `shouldBe` "https://firestore.googleapis.com/v1/projects/a%20b/databases/(default)/documents/users/alice",
    Test "createDocUrl escapes a document ID in the query" $
      createDocUrl testProject (CollectionPath "users") (DocumentId "a&b=c")
        `shouldBe` testDatabaseUrl
        <> "/users?documentId=a%26b%3Dc",
    Test "updateDocUrl escapes field paths" $
      updateDocUrl testProject alicePath ["a&b"]
        `shouldBe` testDatabaseUrl
        <> "/users/alice?updateMask.fieldPaths=a%26b",
    Test "collectionUrl escapes within a subcollection segment" $
      collectionUrl testProject (CollectionPath "users/a b/posts")
        `shouldBe` testDatabaseUrl
        <> "/users/a%20b/posts",
    Test "runQueryUrl escapes within the parent path" $
      runQueryUrl testProject (CollectionPath "users/a b/posts")
        `shouldBe` testDatabaseUrl
        <> "/users/a%20b:runQuery",
    Test "documentInTransactionUrl escapes the transaction ID" $
      documentInTransactionUrl testProject (TransactionId "ab+/=") alicePath
        `shouldBe` testDatabaseUrl
        <> "/users/alice?transaction=ab%2B%2F%3D"
  ]

-- ---------------------------------------------------------------------------
-- Firestore: Resource names
-- ---------------------------------------------------------------------------

resourceNameTests :: [Test]
resourceNameTests =
  [ Test "documentResourceName builds the full path" $
      documentResourceName testProject alicePath
        `shouldBe` "projects/myproj/databases/(default)/documents/users/alice",
    Test "documentResourceName carries components verbatim" $
      documentResourceName testProject (DocumentPath (CollectionPath "users") (DocumentId "a b"))
        `shouldBe` "projects/myproj/databases/(default)/documents/users/a b",
    Test "documentResourceName matches a decoded document's name" $
      documentResourceName (ProjectId "p") (DocumentPath (CollectionPath "users") (DocumentId "alice"))
        `shouldBe` "projects/p/databases/(default)/documents/users/alice"
  ]

-- ---------------------------------------------------------------------------
-- Firestore: Transaction writes
-- ---------------------------------------------------------------------------

writeTests :: [Test]
writeTests =
  [ Test "mkUpdateWrite encodes name and fields" $
      Aeson.toJSON (mkUpdateWrite testProject alicePath (Map.fromList [("age", IntegerValue 31)]))
        `shouldBe` Aeson.object
          [ "update"
              Aeson..= Aeson.object
                [ "name" Aeson..= (T.pack (testResourcePrefix <> "/users/alice") :: Text),
                  "fields"
                    Aeson..= Aeson.object
                      ["age" Aeson..= Aeson.object ["integerValue" Aeson..= ("31" :: Text)]]
                ]
          ],
    Test "mkDeleteWrite encodes the document name" $
      Aeson.toJSON (mkDeleteWrite testProject alicePath)
        `shouldBe` Aeson.object
          ["delete" Aeson..= (T.pack (testResourcePrefix <> "/users/alice") :: Text)],
    Test "mkUpdateWrite accepts an empty field map" $
      Aeson.toJSON (mkUpdateWrite testProject alicePath Map.empty)
        `shouldBe` Aeson.object
          [ "update"
              Aeson..= Aeson.object
                [ "name" Aeson..= (T.pack (testResourcePrefix <> "/users/alice") :: Text),
                  "fields" Aeson..= Aeson.object []
                ]
          ]
  ]
  where
    testResourcePrefix = "projects/myproj/databases/(default)/documents"

-- ---------------------------------------------------------------------------
-- Firestore: Query DSL
-- ---------------------------------------------------------------------------

-- | Wrap a structured-query body in the envelope 'encodeQuery' produces.
structuredQuery :: [(Aeson.Key, Aeson.Value)] -> Aeson.Value
structuredQuery body = Aeson.object ["structuredQuery" Aeson..= Aeson.object body]

-- | The @from@ clause naming a single collection.
fromCollection :: Text -> (Aeson.Key, Aeson.Value)
fromCollection name = "from" Aeson..= [Aeson.object ["collectionId" Aeson..= name]]

-- | A @fieldFilter@ as it appears on the wire.
wireFieldFilter :: Text -> Text -> Aeson.Value -> Aeson.Value
wireFieldFilter field op value =
  Aeson.object
    [ "fieldFilter"
        Aeson..= Aeson.object
          [ "field" Aeson..= Aeson.object ["fieldPath" Aeson..= field],
            "op" Aeson..= op,
            "value" Aeson..= value
          ]
    ]

-- | An @integerValue@ as it appears on the wire.
wireInteger :: Text -> Aeson.Value
wireInteger n = Aeson.object ["integerValue" Aeson..= n]

-- | An @orderBy@ entry as it appears on the wire.
wireOrderBy :: Text -> Text -> Aeson.Value
wireOrderBy field direction =
  Aeson.object
    [ "field" Aeson..= Aeson.object ["fieldPath" Aeson..= field],
      "direction" Aeson..= direction
    ]

queryTests :: [Test]
queryTests =
  [ Test "query encodes basic collection" $
      encodeQuery (query (CollectionPath "users")) `shouldBe` structuredQuery [fromCollection "users"],
    Test "query on a subcollection names only its collection ID" $
      encodeQuery (query (CollectionPath "users/abc/posts"))
        `shouldBe` structuredQuery [fromCollection "posts"],
    Test "encodeQueryInTransaction carries the transaction alongside the query" $
      encodeQueryInTransaction (TransactionId "txn123") (query (CollectionPath "users"))
        `shouldBe` Aeson.object
          [ "structuredQuery" Aeson..= Aeson.object [fromCollection "users"],
            "transaction" Aeson..= ("txn123" :: Text)
          ],
    Test "query encodes with field filter" $
      encodeQuery (query (CollectionPath "users") & where_ (fieldFilter "age" OpGreaterThan (IntegerValue 18)))
        `shouldBe` structuredQuery
          [ fromCollection "users",
            "where" Aeson..= wireFieldFilter "age" "GREATER_THAN" (wireInteger "18")
          ],
    Test "query encodes with orderBy and limit" $
      encodeQuery (query (CollectionPath "users") & orderBy "name" Ascending & limit 25)
        `shouldBe` structuredQuery
          [ fromCollection "users",
            "orderBy" Aeson..= [wireOrderBy "name" "ASCENDING"],
            "limit" Aeson..= (25 :: Int)
          ],
    Test "query encodes composite AND filter" $
      encodeQuery
        ( query (CollectionPath "users")
            & where_
              ( compositeAnd
                  [ fieldFilter "age" OpGreaterThan (IntegerValue 18),
                    fieldFilter "active" OpEqual (BoolValue True)
                  ]
              )
        )
        `shouldBe` structuredQuery
          [ fromCollection "users",
            "where"
              Aeson..= Aeson.object
                [ "compositeFilter"
                    Aeson..= Aeson.object
                      [ "op" Aeson..= ("AND" :: Text),
                        "filters"
                          Aeson..= [ wireFieldFilter "age" "GREATER_THAN" (wireInteger "18"),
                                     wireFieldFilter "active" "EQUAL" (Aeson.object ["booleanValue" Aeson..= True])
                                   ]
                      ]
                ]
          ],
    Test "query encodes composite OR filter" $
      encodeQuery
        ( query (CollectionPath "users")
            & where_
              ( compositeOr
                  [ fieldFilter "role" OpEqual (StringValue "admin"),
                    fieldFilter "role" OpEqual (StringValue "moderator")
                  ]
              )
        )
        `shouldBe` structuredQuery
          [ fromCollection "users",
            "where"
              Aeson..= Aeson.object
                [ "compositeFilter"
                    Aeson..= Aeson.object
                      [ "op" Aeson..= ("OR" :: Text),
                        "filters"
                          Aeson..= [ wireFieldFilter "role" "EQUAL" (Aeson.object ["stringValue" Aeson..= ("admin" :: Text)]),
                                     wireFieldFilter "role" "EQUAL" (Aeson.object ["stringValue" Aeson..= ("moderator" :: Text)])
                                   ]
                      ]
                ]
          ],
    Test "query encodes with offset" $
      encodeQuery (query (CollectionPath "items") & offset 50 & limit 25)
        `shouldBe` structuredQuery
          [ fromCollection "items",
            "limit" Aeson..= (25 :: Int),
            "offset" Aeson..= (50 :: Int)
          ],
    Test "query encodes Descending order" $
      encodeQuery (query (CollectionPath "posts") & orderBy "createdAt" Descending)
        `shouldBe` structuredQuery
          [ fromCollection "posts",
            "orderBy" Aeson..= [wireOrderBy "createdAt" "DESCENDING"]
          ],
    Test "orderBy clauses accumulate in order" $
      encodeQuery (query (CollectionPath "posts") & orderBy "a" Ascending & orderBy "b" Descending)
        `shouldBe` structuredQuery
          [ fromCollection "posts",
            "orderBy" Aeson..= [wireOrderBy "a" "ASCENDING", wireOrderBy "b" "DESCENDING"]
          ],
    Test "all FilterOp values encode distinctly" $
      shouldHold "10 distinct encodings" (length encodings == length (nub encodings))
        >> length allOps `shouldBe` 10
  ]
  where
    encodings = map encodeOp allOps
    encodeOp op = Aeson.encode (encodeQuery (query (CollectionPath "c") & where_ (fieldFilter "f" op (StringValue "v"))))
    allOps =
      [ OpEqual,
        OpNotEqual,
        OpLessThan,
        OpLessThanOrEqual,
        OpGreaterThan,
        OpGreaterThanOrEqual,
        OpArrayContains,
        OpIn,
        OpArrayContainsAny,
        OpNotIn
      ]

-- ---------------------------------------------------------------------------
-- Firestore: Transaction options
-- ---------------------------------------------------------------------------

transactionOptionTests :: [Test]
transactionOptionTests =
  [ Test "ReadWrite transaction options encode" $
      encodeTransactionOptions ReadWrite
        `shouldBe` transactionOptions ["readWrite" Aeson..= Aeson.object []],
    Test "RetryWith transaction options encode" $
      encodeTransactionOptions (RetryWith (TransactionId "abc123"))
        `shouldBe` transactionOptions
          ["readWrite" Aeson..= Aeson.object ["retryTransaction" Aeson..= ("abc123" :: Text)]],
    Test "ReadOnly transaction options encode" $
      encodeTransactionOptions ReadOnly
        `shouldBe` transactionOptions ["readOnly" Aeson..= Aeson.object []],
    Test "TransactionId Show does not leak the token" $
      shouldHold "redacted" (show (TransactionId "super-secret") == "TransactionId <redacted>"),
    Test "AccessToken Show does not leak the token" $
      shouldHold "redacted" (show (AccessToken "ya29.super-secret") == "AccessToken <redacted>")
  ]
  where
    transactionOptions body = Aeson.object ["options" Aeson..= Aeson.object body]

-- ---------------------------------------------------------------------------
-- Firestore: Request helpers
-- ---------------------------------------------------------------------------

requestTests :: [Test]
requestTests =
  [ Test "authorizeRequest adds Bearer header" $
      case parseRequest "https://example.com" :: Maybe Request of
        Nothing -> Left "parseRequest rejected a valid URL"
        Just req ->
          lookup hAuthorization (requestHeaders (authorizeRequest (AccessToken "test-token-123") req))
            `shouldBe` Just "Bearer test-token-123"
  ]

-- ---------------------------------------------------------------------------
-- Firestore: Error parsing
-- ---------------------------------------------------------------------------

errorParsingTests :: [Test]
errorParsingTests =
  [ Test "parseFirestoreError 404" $
      parseFirestoreError 404 "{\"error\":{\"code\":404,\"message\":\"not found\",\"status\":\"NOT_FOUND\"}}"
        `shouldBe` DocumentNotFound,
    Test "parseFirestoreError 403" $
      parseFirestoreError 403 "{\"error\":{\"code\":403,\"message\":\"denied\",\"status\":\"PERMISSION_DENIED\"}}"
        `shouldBe` PermissionDenied "denied",
    Test "parseFirestoreError 409 ABORTED" $
      parseFirestoreError 409 "{\"error\":{\"code\":409,\"message\":\"contention\",\"status\":\"ABORTED\"}}"
        `shouldBe` TransactionAborted "contention",
    Test "parseFirestoreError unparseable" $
      parseFirestoreError 500 "not json at all" `shouldBe` NetworkError "HTTP 500",
    Test "parseFirestoreError JSON without an error object" $
      parseFirestoreError 500 "{\"unexpected\":true}" `shouldBe` NetworkError "HTTP 500",
    Test "parseFirestoreError keeps an unclassified status" $
      parseFirestoreError 500 "{\"error\":{\"code\":500,\"message\":\"boom\",\"status\":\"INTERNAL\"}}"
        `shouldBe` FirestoreApiError 500 "INTERNAL" "boom",
    Test "parseFirestoreError tolerates a missing message" $
      parseFirestoreError 403 "{\"error\":{\"code\":403,\"status\":\"PERMISSION_DENIED\"}}"
        `shouldBe` PermissionDenied "",
    Test "parseFirestoreError unwraps a streaming array-framed error" $
      parseFirestoreError 403 "[{\"error\":{\"code\":403,\"message\":\"denied\",\"status\":\"PERMISSION_DENIED\"}}]"
        `shouldBe` PermissionDenied "denied",
    Test "parseFirestoreError classifies an array-framed ABORTED" $
      parseFirestoreError 409 "[{\"error\":{\"code\":409,\"message\":\"contention\",\"status\":\"ABORTED\"}}]"
        `shouldBe` TransactionAborted "contention"
  ]

-- ---------------------------------------------------------------------------
-- Firestore: Response decoding
-- ---------------------------------------------------------------------------

decoderTests :: [Test]
decoderTests =
  [ Test "decodeQueryResults keeps documents and skips the readTime entry" $
      decodeQueryResults
        "[{\"document\":{\"name\":\"projects/p/databases/(default)/documents/c/d\"}}\
        \,{\"readTime\":\"2024-01-15T10:30:00Z\"}]"
        `shouldBe` Right
          [ Document
              { docName = "projects/p/databases/(default)/documents/c/d",
                docFields = Map.empty,
                docCreateTime = Nothing,
                docUpdateTime = Nothing
              }
          ],
    Test "decodeQueryResults reports a malformed document instead of dropping it" $
      shouldHold "InvalidResponse" $
        isInvalidResponse (decodeQueryResults "[{\"document\":{\"fields\":{}}}]"),
    Test "decodeQueryResults reports a non-object result entry" $
      shouldHold "InvalidResponse" (isInvalidResponse (decodeQueryResults "[42]")),
    Test "decodeDocumentList reads an empty response as no documents" $
      decodeDocumentList "{}" `shouldBe` Right [],
    Test "decodeDocumentList reads the documents field" $
      fmap
        (map docName)
        ( decodeDocumentList
            "{\"documents\":[{\"name\":\"projects/p/databases/(default)/documents/c/d\"}]}"
        )
        `shouldBe` Right ["projects/p/databases/(default)/documents/c/d"],
    Test "decodeTransactionId extracts the ID" $
      decodeTransactionId "{\"transaction\":\"txn-bytes\"}"
        `shouldBe` Right (TransactionId "txn-bytes"),
    Test "decodeTransactionId reports a missing ID" $
      shouldHold "InvalidResponse" (isInvalidResponse (decodeTransactionId "{}")),
    Test "decodeQueryResults surfaces a streamed error element" $
      decodeQueryResults "[{\"error\":{\"code\":403,\"message\":\"denied\",\"status\":\"PERMISSION_DENIED\"}}]"
        `shouldBe` Left (PermissionDenied "denied"),
    Test "decodeQueryResults reports an error after partial results" $
      decodeQueryResults
        "[{\"document\":{\"name\":\"projects/p/databases/(default)/documents/c/d\"}}\
        \,{\"error\":{\"code\":409,\"message\":\"contention\",\"status\":\"ABORTED\"}}]"
        `shouldBe` Left (TransactionAborted "contention")
  ]
  where
    isInvalidResponse :: Either FirestoreError a -> Bool
    isInvalidResponse (Left (InvalidResponse _)) = True
    isInvalidResponse _ = False

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Parse a UTC time string for test data.
parseUTC :: String -> UTCTime
parseUTC = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

-- | Parse a UTC time with sub-second precision, for the timestamp-precision test.
parseUTCSubsecond :: String -> UTCTime
parseUTCSubsecond = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ"
