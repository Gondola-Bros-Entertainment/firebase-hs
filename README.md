# firebase-hs

[![CI](https://github.com/Gondola-Bros-Entertainment/firebase-hs/actions/workflows/ci.yml/badge.svg)](https://github.com/Gondola-Bros-Entertainment/firebase-hs/actions/workflows/ci.yml)
[![Hackage](https://img.shields.io/hackage/v/firebase-hs.svg)](https://hackage.haskell.org/package/firebase-hs)
[![License](https://img.shields.io/badge/license-BSD--3--Clause-blue)](LICENSE)

Firebase for Haskell:

- **Auth**: Firebase ID token (JWT) verification against Google's public
  keys, with RS256 via crypton and automatic key caching
- **Firestore**: CRUD, structured queries, and atomic transactions over the
  REST API
- **WAI / Servant**: auth middleware and an auth combinator, each behind an
  optional cabal flag

Full API documentation lives on
[Hackage](https://hackage.haskell.org/package/firebase-hs).

## Install

```cabal
build-depends: firebase-hs
```

The web integrations are off by default; enable the ones you use:

```bash
cabal build -f wai      # Firebase.Auth.WAI
cabal build -f servant  # Firebase.Servant
```

## Auth

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

Build one `KeyCache` at startup and share it across threads; keys refresh
automatically per Google's `Cache-Control` header.

A token is accepted only if every one of these holds:

| Check | Rule |
|-------|------|
| Algorithm | RS256 only |
| Signature | Must match a Google public key |
| Issuer | `https://securetoken.google.com/<projectId>` |
| Audience | Must equal your Firebase project ID |
| Expiry / issued-at | `exp` in the future, `iat` in the past, within clock skew |
| Subject | `sub` non-empty (becomes the Firebase UID) |

Roles live in custom claims, set by the Admin SDK's `setCustomUserClaims`:

```haskell
if hasClaim "admin" user then handleAdmin user else refuse
```

## Firestore

```haskell
import qualified Data.Map.Strict as Map
import Firebase.Firestore

main :: IO ()
main = do
  fs <- newFirestore (ProjectId "my-project") (AccessToken "ya29...")

  let path = DocumentPath (CollectionPath "users") (DocumentId "alice")
  _ <- createDocument fs (CollectionPath "users") (DocumentId "alice")
         (Map.fromList [("name", StringValue "Alice"), ("age", IntegerValue 30)])
  _ <- updateDocument fs path ["age"] (Map.fromList [("age", IntegerValue 31)])
  doc <- getDocument fs path
  print doc
```

Build one `Firestore` handle and share it; it holds a pooled connection
manager. Access tokens expire, so swap in a fresh one with `withToken`
rather than rebuilding the handle.

Queries compose with `(&)`; subcollections are addressed by path:

```haskell
import Data.Function ((&))

result <- runQuery fs $
  query (CollectionPath "users")
    & where_ (fieldFilter "age" OpGreaterThan (IntegerValue 18))
    & orderBy "age" Ascending
    & limit 10
```

Transactions read with the transaction ID and return the writes to commit.
On any failure the transaction is rolled back, and losing a contention race
surfaces as `TransactionAborted`:

```haskell
result <- runTransaction fs ReadWrite $ \txnId -> runExceptT $ do
  doc <- ExceptT (getDocumentInTransaction fs txnId path)
  pure [mkUpdateWrite (fsProject fs) path (applyDebit 100 (docFields doc))]
```

## WAI

```haskell
import Firebase.Auth (newTlsKeyCache, defaultFirebaseConfig)
import Firebase.Auth.WAI (requireAuth)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  cache <- newTlsKeyCache
  let cfg = defaultFirebaseConfig "my-project-id"
  run 3000 (requireAuth cache cfg myApp)
```

`firebaseAuth` additionally stores the verified `FirebaseUser` in the
request vault for `lookupFirebaseUser` to read downstream.

## Servant

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

## Build and Test

```bash
cabal build all -f wai -f servant --enable-tests --ghc-options="-Werror"
cabal test
```

BSD-3-Clause. Maintained by
[Gondola Bros Entertainment](https://github.com/Gondola-Bros-Entertainment).
