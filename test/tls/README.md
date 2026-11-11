Certificates for the TLS tests (EC P-256, valid until 2125). Test use only.

- `ca.pem`: the test CA. The test suite trusts it, and nothing else, by
  setting `SSL_CERT_FILE` to it. Its key was thrown away after signing.
- `server.pem`, `server-key.pem`: signed by the CA for `localhost` and `127.0.0.1`.
- `other.pem`, `other-key.pem`: signed by the CA for `other.test`, to check that
  a trusted certificate for the wrong name is refused.
- `untrusted.pem`, `untrusted-key.pem`: self-signed for `localhost` and
  `127.0.0.1`, not trusted.
