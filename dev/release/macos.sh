#!/bin/sh
# Build the macOS release (Apple silicon) into dist-release/.
#   dev/release/macos.sh 0.3.1
# OpenSSL is linked statically, from Homebrew's openssl@3, so the binary
# runs on a Mac without Homebrew; it then trusts the system's certificate
# bundle. Native programs that use TLS still need OpenSSL installed.
set -e
cd "$(dirname "$0")/../.."
v="${1:?version}"
ssl="${LIMEN_OPENSSL:-$(brew --prefix openssl@3)}"
LIMEN_OPENSSL="$ssl" LIMEN_OPENSSL_STATIC=1 dune build --root . --profile release bin/main.exe
if otool -L _build/default/bin/main.exe | grep -q -i ssl; then
  echo "macos.sh: the binary still links OpenSSL dynamically" >&2; exit 1
fi
d=$(mktemp -d)
mkdir -p "$d/limen-$v/bin" dist-release
install -m 0755 _build/default/bin/main.exe "$d/limen-$v/bin/limen"
strip "$d/limen-$v/bin/limen"
cp LICENSE-MIT LICENSE-APACHE "$d/limen-$v/"
tar -C "$d" -czf "dist-release/limen-$v-macos-arm64.tar.gz" "limen-$v"
rm -rf "$d"
# Leave _build as a normal development build.
dune build --root . bin/main.exe
ls -la "dist-release/limen-$v-macos-arm64.tar.gz"
