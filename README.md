<div align="center">
<h1>firebase-hs</h1>
<p><strong>Firebase Authentication for Haskell</strong></p>
<p>Verify Firebase ID tokens (JWTs) against Google's public keys. Cached key store with automatic refresh.</p>
<p><a href="#quick-start">Quick Start</a> · <a href="#verification">Verification</a> · <a href="#key-caching">Key Caching</a> · <a href="#error-handling">Error Handling</a> · <a href="#api-reference">API Reference</a></p>
<p>

[![CI](https://github.com/Gondola-Bros-Entertainment/firebase-hs/actions/workflows/ci.yml/badge.svg)](https://github.com/Gondola-Bros-Entertainment/firebase-hs/actions/workflows/ci.yml)
[![Hackage](https://img.shields.io/hackage/v/firebase-hs.svg)](https://hackage.haskell.org/package/firebase-hs)
![Haskell](https://img.shields.io/badge/haskell-GHC%209.6-purple)
![License](https://img.shields.io/badge/license-MIT-blue)

</p>
</div>

---

## What is firebase-hs?

A library for verifying Firebase Authentication ID tokens in Haskell applications:

- **JWT verification** — RS256 signature validation against Google's public JWKs via [jose](https://hackage.haskell.org/package/jose)
- **Automatic key caching** — Respects Google's `Cache-Control: max-age` header, refreshes transparently
- **Claims extraction** — Firebase UID, email, and display name in one type-directed pass
- **Strict validation** — Issuer, audience, expiry, and issued-at checks with configurable clock skew

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
  mgr <- newTlsManager
  let cfg = defaultFirebaseConfig "my-project-id"
  result <- verifyIdToken mgr cfg tokenBytes
  case result of
    Left err   -> putStrLn ("Auth failed: " ++ show err)
    Right user -> putStrLn ("Welcome, " ++ show (fuUid user))
```

### Production: Cached Verification

```haskell
import Firebase.Auth

main :: IO ()
main = do
  cache <- newTlsKeyCache
  let cfg = defaultFirebaseConfig "my-project-id"

  -- Keys are fetched once and refreshed automatically
  result <- verifyIdTokenCached cache cfg tokenBytes
  case result of
    Left err   -> putStrLn ("Auth failed: " ++ show err)
    Right user -> putStrLn ("UID: " ++ show (fuUid user))
```

---

## Verification

### What Gets Validated

Every call to `verifyIdToken` or `verifyIdTokenCached` performs the full Firebase verification:

| Check | Rule |
|-------|------|
| **Algorithm** | RS256 only |
| **Signature** | Must match a Google public key |
| **Issuer** | `https://securetoken.google.com/<projectId>` |
| **Audience** | Must equal your Firebase project ID |
| **Expiry** | `exp` must be in the future (within clock skew) |
| **Issued at** | `iat` must be in the past (within clock skew) |
| **Subject** | `sub` must be non-empty (becomes the Firebase UID) |

### Claims Extraction

Verified tokens yield a `FirebaseUser`:

```haskell
data FirebaseUser = FirebaseUser
  { fuUid   :: !Text         -- Firebase UID (from sub claim)
  , fuEmail :: !(Maybe Text) -- Email, if present
  , fuName  :: !(Maybe Text) -- Display name, if present
  }
```

Uses jose's polymorphic `verifyJWT` with a custom claims subtype — signature verification, standard claims validation, and Firebase-specific field extraction happen in a single type-directed pass.

---

## Key Caching

Google's public keys rotate periodically. The `KeyCache` handles this transparently:

```haskell
-- Create once at startup
cache <- newTlsKeyCache

-- Use for every request — keys refresh automatically
result <- verifyIdTokenCached cache cfg token
```

- Keys are fetched lazily on first verification
- Cache duration comes from Google's `Cache-Control: max-age` response header
- Falls back to 1 hour if the header is missing
- Thread-safe via `IORef` (reads are non-blocking, writes are rare)

For custom HTTP manager configuration:

```haskell
mgr <- newTlsManagerWith mySettings
cache <- newKeyCache mgr
```

---

## Error Handling

All errors are represented as `AuthError`:

```haskell
data AuthError
  = KeyFetchError !Text    -- Failed to fetch Google's public keys
  | InvalidSignature       -- JWT signature doesn't match any Google key
  | TokenExpired           -- Past exp claim (minus allowed skew)
  | InvalidClaims !Text    -- Wrong issuer, audience, empty subject, etc.
  | MalformedToken !Text   -- Not valid compact JWT serialization
```

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

## API Reference

### Configuration

```haskell
-- Default: 300-second clock skew allowance
defaultFirebaseConfig :: Text -> FirebaseConfig

data FirebaseConfig = FirebaseConfig
  { fcProjectId :: !Text            -- Firebase project ID
  , fcClockSkew :: !NominalDiffTime -- Max clock skew (default 300s)
  }
```

### One-Shot Verification

```haskell
-- Fetches keys fresh every call (use for low-traffic or testing)
verifyIdToken :: Manager -> FirebaseConfig -> ByteString -> IO (Either AuthError FirebaseUser)
```

### Cached Verification

```haskell
-- Create a key cache (lazy fetch, automatic refresh)
newKeyCache    :: Manager -> IO KeyCache
newTlsKeyCache :: IO KeyCache

-- Verify with cached keys
verifyIdTokenCached :: KeyCache -> FirebaseConfig -> ByteString -> IO (Either AuthError FirebaseUser)
```

### Utilities

```haskell
-- Parse max-age from Cache-Control headers
parseCacheMaxAge :: ResponseHeaders -> Maybe Int
```

Full Haddock documentation is available on [Hackage](https://hackage.haskell.org/package/firebase-hs).

---

## Build & Test

Requires [GHCup](https://www.haskell.org/ghcup/) with GHC >= 9.4.

```bash
cabal build                              # Build library
cabal test                               # Run all tests
cabal build --ghc-options="-Werror"      # Warnings as errors
cabal haddock                            # Generate docs
```

---

## Contributing

```bash
cabal test && cabal build --ghc-options="-Werror"
```

---

<p align="center">
  <sub>MIT License · <a href="https://github.com/Gondola-Bros-Entertainment">Gondola Bros Entertainment</a></sub>
</p>
