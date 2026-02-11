# Changelog

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
