<div align="center">
<h1>firebase-hs</h1>
<p><strong>Firebase for Haskell</strong></p>
<p>Auth verification, Firestore CRUD, structured queries, atomic transactions, and a Servant auth combinator.</p>
<p><a href="#quick-start">Quick Start</a> · <a href="#firestore">Firestore</a> · <a href="#servant">Servant</a> · <a href="#api-reference">API Reference</a></p>
<p>

[![CI](https://github.com/Gondola-Bros-Entertainment/firebase-hs/actions/workflows/ci.yml/badge.svg)](https://github.com/Gondola-Bros-Entertainment/firebase-hs/actions/workflows/ci.yml)
[![Hackage](https://img.shields.io/hackage/v/firebase-hs.svg)](https://hackage.haskell.org/package/firebase-hs)
![Haskell](https://img.shields.io/badge/haskell-GHC%209.6-purple)
![License](https://img.shields.io/badge/license-MIT-blue)

</p>
</div>

---

## What is firebase-hs?

A pure Haskell library for Firebase services:

- **Auth** — JWT verification against Google's public JWKs, with automatic key caching
- **Firestore** — CRUD operations, structured queries, and atomic transactions via the REST API
- **Servant** — One-liner auth combinator for Servant servers (optional flag)

---

## Quick Start

Add to your `.cabal` file:

```cabal
build-depends:
    firebase-hs
  , http-client-tls
```

### Verify a Token

```haskell
import Firebase.Auth
import Network.HTTP.Client.TLS (newTlsManager)

main :: IO ()
main = do
  cache <- newTlsKeyCache
  let cfg = defaultFirebaseConfig "my-project-id"
  result <- verifyIdTokenCached cache cfg tokenBytes
  case result of
    Left err   -> putStrLn ("Auth failed: " ++ show err)
    Right user -> putStrLn ("UID: " ++ show (fuUid user))
```

---

## Auth

### Verification Rules

| Check | Rule |
|-------|------|
| **Algorithm** | RS256 only |
| **Signature** | Must match a Google public key |
| **Issuer** | `https://securetoken.google.com/<projectId>` |
| **Audience** | Must equal your Firebase project ID |
| **Expiry** | `exp` must be in the future (within clock skew) |
| **Issued at** | `iat` must be in the past (within clock skew) |
| **Subject** | `sub` must be non-empty (becomes the Firebase UID) |

### Key Caching

Keys are fetched lazily on first verification, cached per Google's `Cache-Control: max-age`, and refreshed automatically. Thread-safe via STM.

### Error Handling

```haskell
case result of
  Left (KeyFetchError msg) -> logError "Network issue" msg
  Left InvalidSignature    -> respond 401 "Invalid token"
  Left TokenExpired        -> respond 401 "Token expired"
  Left (InvalidClaims msg) -> respond 401 ("Bad claims: " <> msg)
  Left (MalformedToken _)  -> respond 400 "Malformed token"
  Right user               -> handleAuthenticated user
```

---

## Firestore

### CRUD Operations

```haskell
import Firebase.Firestore
import Network.HTTP.Client.TLS (newTlsManager)

main :: IO ()
main = do
  mgr <- newTlsManager
  let pid = ProjectId "my-project"
      tok = AccessToken "ya29..."

  -- Create
  let fields = Map.fromList [("name", StringValue "Alice"), ("age", IntegerValue 30)]
  _ <- createDocument mgr tok pid (CollectionPath "users") (DocumentId "alice") fields

  -- Read
  let path = DocumentPath (CollectionPath "users") (DocumentId "alice")
  doc <- getDocument mgr tok pid path

  -- Update specific fields
  let updates = Map.fromList [("age", IntegerValue 31)]
  _ <- updateDocument mgr tok pid path ["age"] updates

  -- Delete
  _ <- deleteDocument mgr tok pid path
  pure ()
```

### Structured Queries

Build queries with a pure DSL and `(&)` composition:

```haskell
import Data.Function ((&))

let q = query (CollectionPath "users")
      & where_ (fieldFilter "age" OpGreaterThan (IntegerValue 18))
      & orderBy "age" Ascending
      & limit 10

result <- runQuery mgr tok pid q
```

Composite filters for complex conditions:

```haskell
let q = query (CollectionPath "users")
      & where_ (compositeAnd
          [ fieldFilter "age" OpGreaterThan (IntegerValue 18)
          , fieldFilter "active" OpEqual (BoolValue True)
          ])
```

### Atomic Transactions

Read-then-write operations that succeed or fail atomically:

```haskell
result <- runTransaction mgr tok pid ReadWrite $ \txnId -> do
  -- Reads within the transaction see a consistent snapshot
  doc <- getDocument mgr tok pid userPath
  case doc of
    Left err -> pure (Left err)
    Right d  -> do
      let newBalance = computeNewBalance (docFields d)
      pure (Right [mkUpdateWrite userPath newBalance])
```

Retry aborted transactions:

```haskell
-- First attempt
result <- beginTransaction mgr tok pid ReadWrite
case result of
  Left (TransactionAborted _) ->
    -- Retry with the failed transaction ID for priority
    beginTransaction mgr tok pid (RetryWith txnId)
```

### Firestore Value Types

Values mirror Firestore's tagged wire format:

```haskell
data FirestoreValue
  = NullValue | BoolValue !Bool | IntegerValue !Int64
  | DoubleValue !Double | StringValue !Text | TimestampValue !UTCTime
  | ArrayValue ![FirestoreValue] | MapValue !(Map Text FirestoreValue)
```

Note: integers are encoded as JSON strings (`{"integerValue":"42"}`), not numbers. The JSON instances handle this transparently.

---

## Servant

Enable with the `servant` cabal flag:

```cabal
build-depends: firebase-hs
flags: +servant
```

One-liner auth for any Servant server:

```haskell
import Firebase.Auth (newTlsKeyCache, defaultFirebaseConfig)
import Firebase.Servant (firebaseAuthHandler)
import Servant.Server (Context (..))

main :: IO ()
main = do
  cache <- newTlsKeyCache
  let cfg = defaultFirebaseConfig "my-project-id"
      ctx = firebaseAuthHandler cache cfg :. EmptyContext
  runSettings defaultSettings (serveWithContext api ctx server)
```

The handler extracts the Bearer token, verifies it against Google's keys, and injects a `FirebaseUser` into your endpoint — or returns 401 with a descriptive error.

---

## API Reference

### Auth

```haskell
verifyIdToken       :: Manager -> FirebaseConfig -> ByteString -> IO (Either AuthError FirebaseUser)
newKeyCache         :: Manager -> IO KeyCache
newTlsKeyCache      :: IO KeyCache
verifyIdTokenCached :: KeyCache -> FirebaseConfig -> ByteString -> IO (Either AuthError FirebaseUser)
parseCacheMaxAge    :: ResponseHeaders -> Maybe Int
```

### Firestore

```haskell
getDocument    :: Manager -> AccessToken -> ProjectId -> DocumentPath -> IO (Either FirestoreError Document)
createDocument :: Manager -> AccessToken -> ProjectId -> CollectionPath -> DocumentId -> Map Text FirestoreValue -> IO (Either FirestoreError Document)
updateDocument :: Manager -> AccessToken -> ProjectId -> DocumentPath -> [Text] -> Map Text FirestoreValue -> IO (Either FirestoreError Document)
deleteDocument :: Manager -> AccessToken -> ProjectId -> DocumentPath -> IO (Either FirestoreError ())
runQuery       :: Manager -> AccessToken -> ProjectId -> StructuredQuery -> IO (Either FirestoreError [Document])
```

### Transactions

```haskell
beginTransaction    :: Manager -> AccessToken -> ProjectId -> TransactionMode -> IO (Either FirestoreError TransactionId)
commitTransaction   :: Manager -> AccessToken -> ProjectId -> TransactionId -> [Value] -> IO (Either FirestoreError ())
rollbackTransaction :: Manager -> AccessToken -> ProjectId -> TransactionId -> IO (Either FirestoreError ())
runTransaction      :: Manager -> AccessToken -> ProjectId -> TransactionMode -> (TransactionId -> IO (Either FirestoreError [Value])) -> IO (Either FirestoreError ())
```

### Servant

```haskell
firebaseAuthHandler :: KeyCache -> FirebaseConfig -> AuthHandler Request FirebaseUser
extractBearerToken  :: Request -> Maybe ByteString
authErrorToBody     :: AuthError -> LBS.ByteString
```

Full Haddock documentation is available on [Hackage](https://hackage.haskell.org/package/firebase-hs).

---

## Build & Test

```bash
cabal build                              # Build library
cabal test                               # Run all tests (40 pure tests)
cabal build --ghc-options="-Werror"      # Warnings as errors
cabal build -f servant                   # Build with Servant combinator
cabal haddock                            # Generate docs
```

---

<p align="center">
  <sub>MIT License · <a href="https://github.com/Gondola-Bros-Entertainment">Gondola Bros Entertainment</a></sub>
</p>
