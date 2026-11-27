#!/bin/sh
# Compare the compiler built to WebAssembly (_build/wasm/limen.wasm, from
# dev/wasm/build.sh, run in node through dev/wasm/run.mjs) with the native
# self-hosted one: `check FILE --json` and `caps FILE --json` on every file
# selfhost/check.sh gives the JSON comparison (the repository's programs,
# the C generation edge cases, selfhost/tests/*.lmn and the book's
# examples), each run in the file's directory: same stdout, same exit
# status.
#   sh dev/wasm/check.sh
# LIMEN_TSC names a native self-hosted compiler already built (selfhost/
# check.sh passes its own); without it one is built into _build/wasm.
#
# The wasm compiler's file system is the repository (for the book's
# examples, the examples directory), so it reads a package's files outside
# the file's directory as the native one does, and works in the file's
# directory through PWD. All its runs go through one node process, which
# compiles the module once (run.mjs --batch).
set -e
cd "$(dirname "$0")/../.."
root=$(pwd -P)
wasm=$root/_build/wasm/limen.wasm
if [ ! -f "$wasm" ]; then
  echo "no $wasm: run sh dev/wasm/build.sh first" >&2
  exit 1
fi
if [ -z "$LIMEN_TSC" ]; then
  dune build ./bin/main.exe
  mkdir -p _build/wasm
  _build/default/bin/main.exe build selfhost/limen.lmn -o _build/wasm/limen-native > /dev/null
  LIMEN_TSC=$root/_build/wasm/limen-native
fi
mkdir -p _build
out=$(mktemp -d "$root/_build/wasmcheck.XXXXXX")
trap 'rm -rf "$out"' EXIT

book=${LIMEN_BOOK:-../limen-book}/examples
files=$({ git ls-files '*.lmn'; ls selfhost/*.lmn; find selfhost/tests/load selfhost/tests/check selfhost/tests/ir selfhost/tests/c -name '*.lmn'; } | sort -u)
programs=$(echo "$files" | grep -v '^selfhost/tests/load/' | grep -v '^selfhost/tests/check/' | grep -v '^selfhost/tests/ir/' | grep -v '^selfhost/tests/c/')
c_cases=$(echo "$files" | grep '^selfhost/tests/c/')
cases=$(printf '%s\n' $programs $c_cases $(ls selfhost/tests/*.lmn) | sort -u)
if [ -d "$book" ]; then
  bookroot=$(cd "$book" && pwd -P)
  cases="$cases $(find "$bookroot" -name '*.lmn' | sort)"
fi

# Known limits: programs the native compiler handles that nest deeper than
# the engine's stack allows (V8's, about 1 MB, which wasm frames use too,
# and the playground's browser has no more), so the wasm run ends in a trap
# (run.mjs: `trap: Maximum call stack size exceeded`, exit 134). Each is
# expected to fail that way; if one stops failing, take it off the list.
limits="selfhost/tests/deep-binary.lmn selfhost/tests/deep-chain.lmn selfhost/tests/deep-else-if.lmn"

# The native compiler is granted the directory that holds this repository
# and the book, and finds the working directory from PWD.
grant=$(cd .. && pwd -P)
if [ -d "$book" ]; then
  while [ "$grant" != / ] && case "$bookroot/" in "$grant/"*) false ;; *) true ;; esac; do
    grant=$(dirname "$grant")
  done
fi

# the native runs, and the wasm runs to make (ROOT DIR OUT ARGS..., by tabs)
tab=$(printf '\t')
i=0
for f in $cases; do
  i=$((i + 1))
  dir=$(cd "$(dirname "$f")" && pwd -P)
  file=$(basename "$f")
  case "$dir/" in
    "$root/"*) fsroot=$root ;;
    *) fsroot=$bookroot ;;
  esac
  echo "$f" > "$out/$i.name"
  for cmd in check caps; do
    (cd "$dir" && "$LIMEN_TSC" --grant "fs=$grant" --grant env=PWD -- $cmd "$file" --json) > "$out/$i.$cmd.want" 2> /dev/null && echo "exit 0" >> "$out/$i.$cmd.want" || echo "exit $?" >> "$out/$i.$cmd.want"
    echo "$fsroot$tab$dir$tab$out/$i.$cmd.got$tab$cmd$tab$file$tab--json" >> "$out/jobs"
  done
done
node dev/wasm/run.mjs --batch "$out/jobs"

n=0 bad=0 known=0
j=0
while [ "$j" -lt "$i" ]; do
  j=$((j + 1))
  f=$(cat "$out/$j.name")
  n=$((n + 1))
  limit=0
  for l in $limits; do [ "$f" = "$l" ] && limit=1; done
  ok=1
  for cmd in check caps; do
    if [ "$limit" = 1 ]; then
      if tail -1 "$out/$j.$cmd.got" | grep -qx 'exit 134' && grep -q '^trap: ' "$out/$j.$cmd.got.err"; then continue; fi
      echo "known limit no longer fails as expected: $f ($cmd --json); take it off the list"
      ok=0
    elif ! cmp -s "$out/$j.$cmd.want" "$out/$j.$cmd.got"; then
      echo "differs: $f ($cmd --json)"
      diff "$out/$j.$cmd.want" "$out/$j.$cmd.got" | head -6
      head -3 "$out/$j.$cmd.got.err"
      ok=0
    fi
  done
  if [ "$ok" = 0 ]; then bad=$((bad + 1)); elif [ "$limit" = 1 ]; then known=$((known + 1)); fi
done
echo "$((n - bad - known))/$n files give the same JSON from the wasm compiler ($known known limits: too deep for the engine's stack)"

# eval in the wasm build against the engine's stack: recursion 1000 deep
# runs as natively, and recursion with no end stops at eval's own limit
# with its message (selfhost/tests/eval/too-deep.expected), never a trap
ev=$root/selfhost/tests/eval
for c in deep too-deep; do
  (cd "$ev" && node "$root/dev/wasm/run.mjs" -- eval $c.lmn) > "$out/$c.got" 2> "$out/$c.got.err" && echo "exit 0" >> "$out/$c.got.err" || echo "exit $?" >> "$out/$c.got.err"
done
(cd "$ev" && "$LIMEN_TSC" --grant "fs=$ev" -- eval deep.lmn) > "$out/deep.want" 2> "$out/deep.want.err" && echo "exit 0" >> "$out/deep.want.err" || echo "exit $?" >> "$out/deep.want.err"
if cmp -s "$out/deep.want" "$out/deep.got" && cmp -s "$out/deep.want.err" "$out/deep.got.err"; then
  echo "wasm eval: deep.lmn runs as natively"
else
  echo "wasm eval: deep.lmn differs"
  head -3 "$out/deep.got.err"
  bad=$((bad + 1))
fi
if [ ! -s "$out/too-deep.got" ] && cmp -s "$ev/too-deep.expected" "$out/too-deep.got.err"; then
  echo "wasm eval: too-deep.lmn stops at eval's limit"
else
  echo "wasm eval: too-deep.lmn does not stop at eval's limit"
  head -3 "$out/too-deep.got.err"
  bad=$((bad + 1))
fi
# Rand with no seed draws from the host's secure generator (WASI random_get)
(cd "$ev" && node "$root/dev/wasm/run.mjs" -- eval rand.lmn) > "$out/rand.got" 2> "$out/rand.got.err" && rs=0 || rs=$?
if [ "$rs" -eq 0 ] && [ ! -s "$out/rand.got.err" ]; then
  echo "wasm eval: rand.lmn draws without a seed"
else
  echo "wasm eval: rand.lmn fails without a seed (exit $rs)"
  head -3 "$out/rand.got.err"
  bad=$((bad + 1))
fi
[ "$bad" -eq 0 ]
