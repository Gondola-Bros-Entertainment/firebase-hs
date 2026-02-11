# Changelog

## 0.1.0.0

Initial release.

- Firebase ID token (JWT) verification against Google's public keys
- JWK-based key fetching with `Cache-Control: max-age` caching
- `KeyCache` for production servers (automatic key refresh)
- Convenience `newTlsKeyCache` constructor
