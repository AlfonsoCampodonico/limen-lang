#!/bin/sh
# Build the playground's files for a release into dist-release/:
#   sh dev/release/wasm.sh 0.9.0
# writes limen-0.9.0.wasm.gz (the self-hosted compiler built to
# WebAssembly by dev/wasm/build.sh, gzipped) and wasi-0.9.0.mjs (the WASI
# shim it runs with, dev/wasm/wasi.mjs), and puts their SHA-256 sums in
# dist-release/SHA256SUMS, replacing older lines for the same names. The
# book (limen-book, `make play`) embeds the two in its server.
set -e
cd "$(dirname "$0")/../.."
v="${1:?version}"
sh dev/wasm/build.sh
mkdir -p dist-release
gzip -9 -n -c _build/wasm/limen.wasm > "dist-release/limen-$v.wasm.gz"
cp dev/wasm/wasi.mjs "dist-release/wasi-$v.mjs"
sums=dist-release/SHA256SUMS
touch "$sums"
for f in "limen-$v.wasm.gz" "wasi-$v.mjs"; do
  grep -v "  $f\$" "$sums" > "$sums.new" || true
  (cd dist-release && shasum -a 256 "$f") >> "$sums.new"
  mv "$sums.new" "$sums"
done
ls -la "dist-release/limen-$v.wasm.gz" "dist-release/wasi-$v.mjs"
