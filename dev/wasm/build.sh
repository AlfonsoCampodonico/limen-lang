#!/bin/sh
# Build the self-hosted compiler (selfhost/limen.lmn) to WebAssembly:
# the OCaml compiler's C for it, compiled for wasm32-wasip1 with the
# runtime's LIMEN_WASM mode, to _build/wasm/limen.wasm. It runs through
# the WASI shim in dev/wasm/wasi.mjs (node: dev/wasm/run.mjs).
#   sh dev/wasm/build.sh          build, and print the size raw and gzipped
#   sh dev/wasm/build.sh --probe  exit 0 when the toolchain is here, 1 if not
#
# Toolchain: clang with the WebAssembly target, wasm-ld, a wasm32-wasip1
# sysroot (wasi-libc) and compiler-rt's builtins for it; wasm-opt is used
# when installed. Built with Homebrew's llvm, lld, wasi-libc,
# wasi-runtimes and binaryen (LLVM 23, wasi-libc 34, binaryen 133). A
# wasi-sdk works too: set WASI_SDK_PATH. Otherwise CLANG and WASI_SYSROOT
# name them, and wasm-ld must be on PATH or next to clang.
set -e
cd "$(dirname "$0")/../.."

if [ -n "$WASI_SDK_PATH" ]; then
  : "${CLANG:=$WASI_SDK_PATH/bin/clang}"
  : "${WASI_SYSROOT:=$WASI_SDK_PATH/share/wasi-sysroot}"
fi
if [ -z "$CLANG" ] && [ -x /opt/homebrew/opt/llvm/bin/clang ]; then CLANG=/opt/homebrew/opt/llvm/bin/clang; fi
if [ -z "$CLANG" ] && [ -x /usr/local/opt/llvm/bin/clang ]; then CLANG=/usr/local/opt/llvm/bin/clang; fi
: "${CLANG:=clang}"
for s in /opt/homebrew/opt/wasi-libc/share/wasi-sysroot /usr/local/opt/wasi-libc/share/wasi-sysroot /usr/share/wasi-sysroot; do
  if [ -z "$WASI_SYSROOT" ] && [ -d "$s" ]; then WASI_SYSROOT=$s; fi
done
# Homebrew keeps wasm-ld in lld, apart from clang
for d in /opt/homebrew/opt/lld/bin /usr/local/opt/lld/bin; do
  if [ -x "$d/wasm-ld" ]; then PATH=$d:$PATH; fi
done
export PATH

# whether CLANG links a C program for wasm32-wasip1
probe() {
  [ -n "$WASI_SYSROOT" ] && [ -d "$WASI_SYSROOT" ] || return 1
  command -v "$CLANG" > /dev/null 2>&1 || return 1
  t=$(mktemp -d "${TMPDIR:-/tmp}/limen-wasm.XXXXXX")
  printf 'int main(void) { return 0; }\n' > "$t/p.c"
  if "$CLANG" --target=wasm32-wasip1 --sysroot="$WASI_SYSROOT" -o "$t/p.wasm" "$t/p.c" > /dev/null 2>&1; then r=0; else r=1; fi
  rm -rf "$t"
  return $r
}

if [ "$1" = --probe ]; then
  probe
  exit $?
fi
if ! probe; then
  echo "no wasm32-wasip1 toolchain: install clang, wasm-ld and a WASI sysroot (Homebrew: llvm lld wasi-libc wasi-runtimes), or set WASI_SDK_PATH" >&2
  exit 1
fi

dune build ./bin/main.exe
mkdir -p _build/wasm
_build/default/bin/main.exe build selfhost/limen.lmn --emit-c -o _build/wasm/limen > /dev/null
# The flags: the native build's (-fwrapv -ffp-contract=off), wasm
# exception handling for the runtime's setjmp/longjmp panics, WASI's
# emulated signal, process clocks and getpid (the hash seed's fallback
# mixes in the pid), a 64 MB stack (the compiler recurses as deep as the
# program nests) and memory up to 4 GB. --stack-first puts the stack below
# the data, so running past it traps instead of overwriting globals.
"$CLANG" --target=wasm32-wasip1 --sysroot="$WASI_SYSROOT" -O2 -DLIMEN_WASM \
  -fwrapv -ffp-contract=off -w \
  -mexception-handling -mllvm -wasm-enable-sjlj \
  -D_WASI_EMULATED_SIGNAL -D_WASI_EMULATED_PROCESS_CLOCKS -D_WASI_EMULATED_GETPID \
  -Wl,-z,stack-size=67108864 -Wl,--stack-first -Wl,--max-memory=4294967296 \
  -o _build/wasm/limen.wasm _build/wasm/limen.c \
  -lsetjmp -lwasi-emulated-signal -lwasi-emulated-process-clocks -lwasi-emulated-getpid -lm
if command -v wasm-opt > /dev/null 2>&1; then
  wasm-opt -O2 --enable-exception-handling --enable-bulk-memory --enable-sign-ext --enable-mutable-globals --enable-nontrapping-float-to-int \
    -o _build/wasm/limen.opt.wasm _build/wasm/limen.wasm
  mv _build/wasm/limen.opt.wasm _build/wasm/limen.wasm
fi
raw=$(wc -c < _build/wasm/limen.wasm)
gz=$(gzip -9 -c _build/wasm/limen.wasm | wc -c)
echo "_build/wasm/limen.wasm: $((raw / 1024)) KB, $((gz / 1024)) KB gzipped"
