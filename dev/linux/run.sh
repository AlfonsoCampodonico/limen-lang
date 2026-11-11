#!/bin/sh
# Build Vela in a Linux container and run the full test suite there.
# Usage: dev/linux/run.sh [arm64|amd64] [command...]
set -e
cd "$(dirname "$0")/../.."
arch="${1:-$(uname -m | sed 's/x86_64/amd64/; s/aarch64/arm64/')}"
[ $# -gt 0 ] && shift
tag="vela-linux-$arch"
docker build --platform "linux/$arch" -f dev/linux/Dockerfile -t "$tag" .
# ptrace is needed by the sanitizers; the container stays otherwise unprivileged
exec docker run --rm --platform "linux/$arch" --cap-add SYS_PTRACE "$tag" "$@"
