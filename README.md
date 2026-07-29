<div align="center">
<h1>firebase-hs</h1>
<p><strong>Firebase for Haskell</strong></p>
<p>Auth verification, Firestore CRUD, structured queries, atomic transactions, and a Servant auth combinator.</p>
<p><a href="#quick-start">Quick Start</a> | <a href="#firestore">Firestore</a> | <a href="#servant">Servant</a> | <a href="#api-reference">API Reference</a></p>
<p>

[![CI](https://github.com/Gondola-Bros-Entertainment/firebase-hs/actions/workflows/ci.yml/badge.svg)](https://github.com/Gondola-Bros-Entertainment/firebase-hs/actions/workflows/ci.yml)
[![Hackage](https://img.shields.io/hackage/v/firebase-hs.svg)](https://hackage.haskell.org/package/firebase-hs)
![Haskell](https://img.shields.io/badge/haskell-GHC%209.8-purple)
![License](https://img.shields.io/badge/license-BSD--3--Clause-blue)

</p>
</div>

---

## What is firebase-hs?

A pure Haskell library for Firebase services:

- **Auth**: JWT verification against Google's public JWKs using crypton for RS256, with automatic key caching
- **Firestore**: CRUD operations, structured queries, and atomic transactions via the REST API
- **WAI / Servant**: auth middleware and a one-line auth combinator, each behind an optional flag

---

## Quick Start

Add to your `.cabal` file:

```cabal
build-depends: firebase-hs
```

### Verify a Token

```haskell
import Firebase.Auth

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

### Authorization

A verified token carries more than an identity. Custom claims set by the
Firebase Admin SDK's `setCustomUserClaims` are where roles live:

```haskell
Right user <- verifyIdTokenCached cache cfg tokenBytes

if hasClaim "admin" user
  then handleAdmin user
  else refuse

-- Non-boolean claims come back as raw JSON
case lookupClaim "tier" user of
  Just (String tier) -> applyTier tier
  _                  -> applyDefaultTier
```

`FirebaseUser` also exposes `fuEmailVerified`, `fuSignInProvider`, and
`fuAuthTime` for requiring a recent login before a sensitive action.

### Key Caching

Keys are fetched lazily on first verification, cached for the lifetime named
by Google's `Cache-Control: max-age`, and refreshed automatically when that
expires. Build one `KeyCache` at startup and share it across threads.

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

The `Text` payloads describe internal specifics. To render an error for a
client instead, use `authErrorMessage`, which omits them.

---

## Firestore

### CRUD Operations

```haskell
import qualified Data.Map.Strict as Map
import Firebase.Firestore

main :: IO ()
main = do
  fs <- newFirestore (ProjectId "my-project") (AccessToken "ya29...")

  -- Create
  let fields = Map.fromList [("name", StringValue "Alice"), ("age", IntegerValue 30)]
  _ <- createDocument fs (CollectionPath "users") (DocumentId "alice") fields

  -- Read
  let path = DocumentPath (CollectionPath "users") (DocumentId "alice")
  doc <- getDocument fs path

  -- Update specific fields
  _ <- updateDocument fs path ["age"] (Map.fromList [("age", IntegerValue 31)])

  -- List a collection
  docs <- listDocuments fs (CollectionPath "users")

  -- Delete
  _ <- deleteDocument fs path
  pure ()
```

Build one `Firestore` and share it; it holds a pooled connection manager.
Access tokens expire, so swap in a fresh one with `withToken fs` rather than
rebuilding the handle.

### Structured Queries

Build queries with a pure DSL and `(&)` composition:

```haskell
import Data.Function ((&))

let q = query (CollectionPath "users")
      & where_ (fieldFilter "age" OpGreaterThan (IntegerValue 18))
      & orderBy "age" Ascending
      & limit 10

result <- runQuery fs q
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

Read-then-write operations that succeed or fail together. If the callback
fails, or the commit does, the transaction is rolled back for you:

```haskell
result <- runTransaction fs ReadWrite $ \_txnId -> runExceptT $ do
  -- Reads within the transaction see a consistent snapshot
  doc <- ExceptT $ getDocument fs userPath
  let updated = applyDebit 100 (docFields doc)
  pure [mkUpdateWrite (fsProject fs) userPath updated]
```

Use `mkUpdateWrite` and `mkDeleteWrite` to build the writes a commit expects.

Losing a contention race returns `TransactionAborted`. Retry with the aborted
transaction's ID to give the new attempt priority:

```haskell
begun <- beginTransaction fs ReadWrite
case begun of
  Left err -> pure (Left err)
  Right txnId -> do
    committed <- commitTransaction fs txnId writes
    case committed of
      Left (TransactionAborted _) -> do
        retried <- beginTransaction fs (RetryWith txnId)
        either (pure . Left) (\rid -> commitTransaction fs rid writes) retried
      other -> pure other
```

### Firestore Value Types

Values mirror Firestore's tagged wire format:

```haskell
data FirestoreValue
  = NullValue | BoolValue !Bool | IntegerValue !Int64
  | DoubleValue !Double | StringValue !Text | BytesValue !ByteString
  | ReferenceValue !Text | GeoPointValue !GeoPoint | TimestampValue !UTCTime
  | ArrayValue ![FirestoreValue] | MapValue !(Map Text FirestoreValue)
```

Every type Firestore stores is covered. Integers travel as JSON strings
(`{"integerValue":"42"}`) and bytes as base64; the JSON instances handle both
transparently.

---

## WAI Middleware

Protect any WAI-based server (Warp, Scotty, Yesod, Spock) with Firebase auth.
Enable with the `wai` cabal flag:

```bash
cabal build -f wai
```

### Simple Gate

Reject unauthenticated requests before they reach your app:

```haskell
import Firebase.Auth (newTlsKeyCache, defaultFirebaseConfig)
import Firebase.Auth.WAI (requireAuth)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  cache <- newTlsKeyCache
  let cfg = defaultFirebaseConfig "my-project-id"
  run 3000 $ requireAuth cache cfg myApp
```

### With User Propagation

Store the authenticated user in the WAI vault for downstream handlers:

```haskell
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Text as T
import Firebase.Auth.WAI (firebaseAuth, lookupFirebaseUser)
import Network.HTTP.Types (status200, status500)
import Network.Wai (responseLBS)

main :: IO ()
main = do
  cache <- newTlsKeyCache
  let cfg = defaultFirebaseConfig "my-project-id"
  run 3000 (firebaseAuth cache cfg myApp)

myApp req respond = respond $ case lookupFirebaseUser req of
  Just user -> responseLBS status200 [] (LBS8.pack ("Hello, " <> T.unpack (fuUid user)))
  Nothing   -> responseLBS status500 [] "unreachable: the middleware rejects first"
```

---

## Servant

Enable with the `servant` cabal flag:

```bash
cabal build -f servant
```

One-line auth for any Servant server:

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

The handler extracts the Bearer token, verifies it against Google's keys, and
injects a `FirebaseUser` into your endpoint, or returns 401.

---

## API Reference

Generated from the source and published with each release:
[hackage.haskell.org/package/firebase-hs](https://hackage.haskell.org/package/firebase-hs)

---

## Build & Test

```bash
cabal build                       # Library, default flags
cabal test                        # Test suite
cabal haddock -f wai -f servant   # Generate docs
```

The optional modules are off by default. To build everything the package
ships, with warnings as errors:

```bash
cabal build all -f wai -f servant --enable-tests --ghc-options="-Werror"
```

---

<p align="center">
  <sub>BSD-3-Clause License | <a href="https://github.com/Gondola-Bros-Entertainment">Gondola Bros Entertainment</a></sub>
</p>
