#!/bin/sh
# Check grammar/limen.gbnf with llama.cpp's real GBNF parser and matcher (not
# part of `dune test`; it needs git, cmake, a C++ compiler and a network
# connection the first time).
#
# Fetches llama.cpp at a pinned release into a cache outside the repository
# ($LIMEN_CACHE, default ~/.cache/limen), builds only the llama library, then
# runs every program through grammar/llama_check.cpp: the hand-written accept
# and reject cases, the examples, the benchmarks, and every program in the
# test suite (programs that parse must match, programs the parser rejects
# must not).
#
# Homebrew's llama.cpp is not used: it ships neither a grammar validator nor
# the internal header (src/llama-grammar.h) one needs, and the public API can
# only apply a grammar to a model's tokens.
set -e
TAG=b12250
root=$(cd "$(dirname "$0")/.." && pwd)
cache=${LIMEN_CACHE:-$HOME/.cache/limen}
src=$cache/llama.cpp-$TAG
build=$src/build-grammar
mkdir -p "$cache"

if [ ! -d "$src/.git" ]; then
  echo "fetching llama.cpp $TAG into $src"
  rm -rf "$src"
  git clone --quiet --depth 1 --branch "$TAG" https://github.com/ggml-org/llama.cpp "$src"
fi

if [ ! -f "$build/src/libllama.a" ]; then
  echo "building libllama (once)"
  cmake -S "$src" -B "$build" -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=OFF \
    -DLLAMA_BUILD_TESTS=OFF -DLLAMA_BUILD_EXAMPLES=OFF -DLLAMA_BUILD_TOOLS=OFF -DLLAMA_BUILD_SERVER=OFF \
    -DLLAMA_BUILD_COMMON=OFF -DLLAMA_CURL=OFF -DLLAMA_OPENSSL=OFF -DGGML_METAL=OFF -DGGML_BLAS=OFF -DGGML_NATIVE=OFF \
    -DGGML_OPENMP=OFF >/dev/null
  cmake --build "$build" --target llama -j "$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 4)" >/dev/null
fi

exe=$build/limen-llama-check
if [ ! -x "$exe" ] || [ "$root/grammar/llama_check.cpp" -nt "$exe" ]; then
  libs=$(find "$build" -name 'libllama.a' -o -name 'libggml*.a' | sort -r)
  case $(uname) in Darwin) libs="$libs -framework Accelerate -framework Foundation" ;; esac
  ${CXX:-c++} -std=c++17 -O1 -I"$src/include" -I"$src/ggml/include" -I"$src/src" \
    "$root/grammar/llama_check.cpp" $libs -lpthread -o "$exe"
fi

# every program in the test suite, sorted by whether the parser accepts it
dump=$(mktemp -d)
trap 'rm -rf "$dump"' EXIT
cd "$root"
dune build --root . 2>/dev/null
LIMEN_SKIP_NATIVE=1 LIMEN_DUMP_DIR="$dump" ./_build/default/test/test_limen.exe >/dev/null || true

"$exe" grammar/limen.gbnf grammar/tests/accept/*.lmn examples/*.lmn bench/*.lmn "$dump"/accept/*.lmn \
  --reject grammar/tests/reject/*.lmn "$dump"/reject/*.lmn
