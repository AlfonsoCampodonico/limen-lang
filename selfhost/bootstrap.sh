#!/bin/sh
# The bootstrap: the Limen compiler, written in Limen, compiling itself.
#   stage 1: the OCaml compiler builds selfhost/limen.lmn
#   stage 2: stage 1 builds selfhost/limen.lmn
#   stage 3: stage 2 builds selfhost/limen.lmn
# Limen is self-hosting when stage 2 and stage 3 emit the same C: the
# compiler then reproduces itself exactly. The binaries can still differ in
# their bytes, since the linker stamps each with a unique id.
#   sh selfhost/bootstrap.sh
set -e
cd "$(dirname "$0")/.."
dune build ./bin/main.exe
out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT
# the self-hosted compiler and the only authority it gets: files, the
# settings it reads, and the programs a build runs
tsc() {
  bin=$1
  shift
  "$bin" --grant fs=/ \
    --grant env=PWD --grant env=CC --grant env=PATH --grant env=HOME \
    --grant env=LIMEN_CACHE --grant env=XDG_CACHE_HOME --grant env=DEVELOPER_DIR --grant env=LIMEN_OPENSSL \
    --grant env=LIBRARY_PATH --grant env=CPATH --grant env=C_INCLUDE_PATH --grant env=SDKROOT --grant env=MACOSX_DEPLOYMENT_TARGET \
    --grant "exec=${CC:-cc}" --grant exec=readlink --grant exec=cp --grant exec=touch --grant exec=pkg-config \
    -- "$@"
}
step() { printf '%-44s' "$1"; start=$(date +%s); }
done_() { echo "$(( $(date +%s) - start )) s"; }

step "stage 1: OCaml compiler builds limen.lmn"
LIMEN_CACHE=off _build/default/bin/main.exe build selfhost/limen.lmn -o "$out/stage1" > /dev/null
done_
step "stage 2: stage 1 builds limen.lmn"
LIMEN_CACHE=off tsc "$out/stage1" build selfhost/limen.lmn -o "$out/stage2" > /dev/null
done_
step "stage 3: stage 2 builds limen.lmn"
LIMEN_CACHE=off tsc "$out/stage2" build selfhost/limen.lmn -o "$out/stage3" > /dev/null
done_
tsc "$out/stage2" build --emit-c selfhost/limen.lmn -o "$out/c2" > /dev/null
tsc "$out/stage3" build --emit-c selfhost/limen.lmn -o "$out/c3" > /dev/null
_build/default/bin/main.exe build --emit-c selfhost/limen.lmn -o "$out/c0" > /dev/null
lines=$(wc -l < "$out/c2.c" | tr -d ' ')
if cmp -s "$out/c2.c" "$out/c3.c" && cmp -s "$out/c0.c" "$out/c2.c"; then
  echo "fixed point: stages 2 and 3 emit the same C as the OCaml compiler ($lines lines)"
else
  echo "NOT a fixed point"
  cmp "$out/c2.c" "$out/c3.c" || true
  cmp "$out/c0.c" "$out/c2.c" || true
  exit 1
fi
# and stage 3 works: it checks and runs a program
tsc "$out/stage3" build examples/hello.lmn -o "$out/hello" > /dev/null
"$out/hello"
