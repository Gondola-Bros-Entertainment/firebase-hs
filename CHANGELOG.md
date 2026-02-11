# Changelog

## 0.2.0.0

### Firestore REST API Client
- CRUD operations: `getDocument`, `createDocument`, `updateDocument`, `deleteDocument`
- Structured query DSL with composable builder pattern (`query`, `where_`, `orderBy`, `limit`)
- Composite filters (`compositeAnd`, `compositeOr`) for complex query conditions
- `FirestoreValue` ADT with custom JSON instances matching Firestore's tagged wire format
- `Document` type with automatic JSON decoding from Firestore responses

### Atomic Transactions
- `beginTransaction`, `commitTransaction`, `rollbackTransaction` for manual control
- `runTransaction` for automatic begin/commit/rollback with callback
- `TransactionMode` sum type: `ReadWrite`, `RetryWith`, `ReadOnly`
- `TransactionAborted` error variant for contention detection

### Servant Auth Combinator (optional)
- `firebaseAuthHandler` — one-liner Firebase auth for Servant servers
- Gated behind `servant` cabal flag (default off, zero extra deps)
- Pure helpers: `extractBearerToken`, `authErrorToBody`

### Internal
- Pure URL builders and error parsers in `Firebase.Firestore.Internal` (fully testable)
- 40 pure tests (up from 9): value roundtrips, document decoding, URL construction, query DSL, transaction encoding, error parsing

## 0.1.0.0

Initial release.

- Firebase ID token (JWT) verification against Google's public keys
- RS256 signature validation via jose's polymorphic `verifyJWT` with custom claims subtype
- JWK-based key fetching with `Cache-Control: max-age` caching
- STM-backed `KeyCache` for thread-safe concurrent verification
- Strict JWKSet evaluation on fetch (zero deferred parse cost on hot path)
- Convenience `newTlsKeyCache` constructor
- Full claims validation: issuer, audience, expiry, issued-at, subject
- Configurable clock skew (default 300s)
