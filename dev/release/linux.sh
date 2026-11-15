#!/bin/sh
# Build the Linux release for amd64 and arm64 into dist-release/.
#   dev/release/linux.sh 0.2.0 [arm64|amd64]
set -e
cd "$(dirname "$0")/../.."
v="${1:?version}"
archs="${2:-arm64 amd64}"
mkdir -p dist-release
for a in $archs; do
  docker build --platform "linux/$a" -f dev/release/Dockerfile -t "limen-release-$a" .
  docker run --rm --platform "linux/$a" -e VERSION="$v" -v "$PWD/dist-release:/out" "limen-release-$a"
done
