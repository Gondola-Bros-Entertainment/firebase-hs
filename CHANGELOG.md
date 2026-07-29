# Changelog

## 0.3.0.0

### Fixed
- Percent-encode every caller-supplied URL component. Project IDs, collection
  paths, document IDs, and `updateMask` field paths were interpolated raw, so
  a value containing `?`, `#`, `%`, `&`, a space, or any non-ASCII character
  produced a malformed request or injected query parameters. Subcollection
  `/` separators are preserved; the segments between them are encoded.
- `FirestoreValue` covers every type Firestore stores. `bytesValue`,
  `referenceValue`, and `geoPointValue` had no representation, so a document
  containing any of them failed to decode outright and `getDocument` returned
  `InvalidResponse` for data Firestore considers perfectly valid.
- A cache refresh no longer overwrites a newer key set installed by a
  concurrent verification.

### Breaking Changes
- Firestore operations take a single `Firestore` handle in place of the
  `Manager -> AccessToken -> ProjectId` prefix every one of them repeated.
  Build it with `newFirestore`, and refresh an expiring token with
  `withToken` rather than rebuilding the connection pool.
- `FirebaseUser` gained fields, so positional construction no longer
  compiles. Pattern matches on `fuUid`, `fuEmail`, and `fuName` are
  unaffected.
- `KeyCache` is now backed by `IORef` rather than `TVar`, and `Firebase.Auth`
  re-exports it as an abstract type. Construct it with `newKeyCache` or
  `newTlsKeyCache`.
- Dropped the `stm` dependency: the cache is a single reference with no
  composed transactions, which `atomicModifyIORef'` covers.
- Dropped the `mtl` dependency from the `servant` flag. `Firebase.Servant`
  now builds its 401 on the `Handler` newtype and `transformers`, which does
  not depend on which `MonadError` re-export a given servant version ships.
- `Firebase.Servant` no longer exports `extractBearerToken` or
  `authErrorToBody`. Both were duplicates of the `Firebase.Auth.WAI`
  versions; use `Firebase.Auth.Internal.bearerToken` and
  `Firebase.Auth.authErrorMessage`.
- A response body that is JSON but not shaped like a Firestore error now
  reports `NetworkError "HTTP <status>"` instead of
  `FirestoreApiError status "" "unknown error"`.

### Added
- Token claims are no longer discarded. `FirebaseUser` now carries
  `fuEmailVerified`, `fuPicture`, `fuAuthTime`, `fuSignInProvider`, and
  `fuCustomClaims`, with `hasClaim` and `lookupClaim` to read them. Custom
  claims are how Firebase expresses roles, so without them a verified token
  could establish identity but not authorize anything.
- `listDocuments` lists a collection. `collectionUrl` had described itself as
  "used for listing" since the first release with nothing to use it.
- `BytesValue`, `ReferenceValue`, `GeoPointValue`, and the `GeoPoint` type.
- `mkUpdateWrite` and `mkDeleteWrite` build the writes `commitTransaction`
  and `runTransaction` expect. `mkUpdateWrite` was referenced by the README
  and Haddock but never existed, leaving no supported way to construct a
  write.
- `Firebase.Auth.Internal`: base64url padding and bearer-token extraction,
  shared by the verifier and both web integrations.
- `authErrorMessage` renders an `AuthError` as a client-safe body.
- `documentResourceName` and the percent-encoding helpers are exported from
  `Firebase.Firestore.Internal`.

### Changed
- Bearer scheme names are matched case-insensitively, as RFC 7235 requires.
- Widened bounds: `http-client-tls < 0.5` (resolves the Stackage report in
  issue #1), `containers < 0.9` (`< 0.8` excluded the version GHC 9.10 and
  later ship, blocking those compilers outright), `aeson < 2.4`,
  `time < 1.17`, `crypton < 2`. Lowered `base` to `>= 4.18`.
- CI builds the `wai` and `servant` modules, which no job previously
  compiled, and adds a GHC compatibility matrix, an sdist build, and an
  ASCII-only source check.

## 0.2.0.0

### Breaking Changes
- Replaced `jose` JWT backend with direct `crypton` RS256 verification
- Dropped `jose`, `lens`, `memory` dependencies for a significantly lighter dependency tree
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
