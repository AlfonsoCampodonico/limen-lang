#!/bin/sh
# Regenerates every file in this directory. Test use only; the keys are public.
# Works with OpenSSL 1.1.1+, 3.x and LibreSSL. Valid for ten years.
set -eu
cd "$(dirname "$0")"
days=3650
tmp=$(mktemp -d "${TMPDIR:-/tmp}/kube-tls.XXXXXX")
trap 'rm -rf "$tmp"' EXIT

key() { openssl ecparam -name prime256v1 -genkey -noout -out "$1.tmp" && openssl pkcs8 -topk8 -nocrypt -in "$1.tmp" -out "$1" && rm "$1.tmp"; }

cat > "$tmp/ca.ext" <<X
basicConstraints = critical, CA:TRUE
keyUsage = critical, keyCertSign, cRLSign
subjectKeyIdentifier = hash
X
cat > "$tmp/server.ext" <<X
basicConstraints = CA:FALSE
keyUsage = critical, digitalSignature
extendedKeyUsage = serverAuth
subjectAltName = DNS:localhost, IP:127.0.0.1
X
cat > "$tmp/client.ext" <<X
basicConstraints = CA:FALSE
keyUsage = critical, digitalSignature
extendedKeyUsage = clientAuth
X

# ca NAME CN
ca() {
  key "$1-key.pem"
  openssl req -new -key "$1-key.pem" -subj "/CN=$2" -out "$tmp/$1.csr"
  openssl x509 -req -in "$tmp/$1.csr" -signkey "$1-key.pem" -days $days -extfile "$tmp/ca.ext" -out "$1.pem"
}
# leaf NAME CN CA EXT
leaf() {
  key "$1-key.pem"
  openssl req -new -key "$1-key.pem" -subj "/CN=$2" -out "$tmp/$1.csr"
  openssl x509 -req -in "$tmp/$1.csr" -CA "$3.pem" -CAkey "$3-key.pem" -set_serial "0x$(openssl rand -hex 8)" \
    -days $days -extfile "$tmp/$4.ext" -out "$1.pem"
}

ca ca "Limen kube test CA"
leaf server localhost ca server
leaf client limen-test-client ca client
ca other-ca "Limen other test CA"
leaf other-server localhost other-ca server
