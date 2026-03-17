# Changelog

## 0.2.0.0

### Breaking Changes
- Replaced `jose` JWT backend with direct `crypton` RS256 verification
- Dropped `jose`, `lens`, `memory` dependencies — significantly lighter dependency tree
- `JWKSet` (from jose) replaced with internal `JwkSet`/`JwkKey` types in `Firebase.Auth.Types`
- License changed from MIT to BSD-3-Clause

### Added
- `JwkSet` and `JwkKey` types for Google's public key representation
- Manual JWT parsing with `base64-bytestring` (no jose overhead)
- RS256 signature verification via `crypton` (`Crypto.PubKey.RSA.PKCS15`)

### Changed
- Bumped GHC to 9.8.4, `base >= 4.19`
- Widened `stm` bounds to `< 2.7`
- Widened `text` bounds to `>= 1.2 && < 2.2`
- Compatible with modern Haskell crypto stack (`crypton`)

### Removed
- `jose` dependency (source of `memory`/`ram` incompatibility)
- `lens` dependency (only used for jose's API)
- `FirebaseClaims`, `HasClaimsSet` internal types (replaced with direct JSON parsing)

### Unchanged
- Public API for `verifyIdToken`, `verifyIdTokenCached`, `newKeyCache`, `newTlsKeyCache`
- `FirebaseConfig`, `FirebaseUser`, `AuthError`, `KeyCache` types
- WAI middleware (`Firebase.Auth.WAI`)
- Servant combinator (`Firebase.Servant`)
- Firestore client (all modules unchanged)
- All 46 tests pass

## 0.1.1.0

### Fixed
- Import `throwError` from `Control.Monad.Except` (mtl) instead of `Servant.Server` re-export for compatibility with newer servant versions
- Add explicit `mtl` dependency under `servant` flag

## 0.1.0.0

Initial release.

### Auth
- Firebase ID token (JWT) verification against Google's public keys
- RS256 signature validation via jose
- JWK-based key fetching with `Cache-Control: max-age` caching
- STM-backed `KeyCache` for thread-safe concurrent verification
- Full claims validation: issuer, audience, expiry, issued-at, subject
- Configurable clock skew (default 300s)

### Firestore REST API Client
- CRUD operations: `getDocument`, `createDocument`, `updateDocument`, `deleteDocument`
- Structured query DSL with composable builder pattern
- Composite filters for complex query conditions
- `FirestoreValue` ADT with custom JSON instances matching Firestore's tagged wire format

### Atomic Transactions
- `beginTransaction`, `commitTransaction`, `rollbackTransaction` for manual control
- `runTransaction` for automatic begin/commit/rollback with callback
- `TransactionMode` sum type: `ReadWrite`, `RetryWith`, `ReadOnly`

### WAI Auth Middleware (optional, `wai` flag)
- `requireAuth`, `firebaseAuth`, `lookupFirebaseUser`

### Servant Auth Combinator (optional, `servant` flag)
- `firebaseAuthHandler`, `extractBearerToken`, `authErrorToBody`

### Internal
- Pure URL builders and error parsers in `Firebase.Firestore.Internal`
- 41 pure tests
