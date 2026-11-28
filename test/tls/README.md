Certificates for the TLS tests (EC P-256, valid until 2125). Test use only.

- `ca.pem`: the test CA. The test suite trusts it, and nothing else, by
  setting `SSL_CERT_FILE` to it. Its key was thrown away after signing.
- `server.pem`, `server-key.pem`: signed by the CA for `localhost` and `127.0.0.1`.
- `other.pem`, `other-key.pem`: signed by the CA for `other.test`, to check that
  a trusted certificate for the wrong name is refused.
- `untrusted.pem`, `untrusted-key.pem`: self-signed for `localhost` and
  `127.0.0.1`, not trusted.

## `kube/`

A second CA, whose key is kept so the files can be regenerated with
`kube/make.sh` (EC P-256, valid for ten years). It is for `Net.trusting` and
`Net.presenting`. It must never be added to `SSL_CERT_FILE`: the suite trusts
only `ca.pem` above, so the tests that hand this CA to a connection prove it
is used.

- `ca.pem`, `ca-key.pem`: the CA.
- `server.pem`, `server-key.pem`: for `localhost` and `127.0.0.1`
  (serverAuth).
- `client.pem`, `client-key.pem`: a client certificate (clientAuth).
- `client-key-encrypted.pem`: the same key, encrypted (passphrase
  `limen-test`), which `presenting` refuses.
- `other-ca.pem`, `other-ca-key.pem`: an unrelated CA.
- `other-server.pem`, `other-server-key.pem`: signed by `other-ca.pem`, same
  names as `server.pem`, for checking a bundle with several CAs.
