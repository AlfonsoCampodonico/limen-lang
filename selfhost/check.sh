#!/bin/sh
# Compare the Tessera lexer and parser (selfhost/*.tsr) with the OCaml ones
# (src/lexer.ml and src/parser.ml, through selfhost/tokdump and
# selfhost/astdump): same tokens, same trees, same positions, same errors.
#   sh selfhost/check.sh
# The parsers are compared on every .tsr file in the repository, on the
# edge cases in selfhost/tests, and on the book's examples when the book is
# checked out next to this repository (or at $TESSERA_BOOK).
set -e
cd "$(dirname "$0")/.."
dune build ./bin/main.exe ./selfhost/tokdump/tokdump.exe ./selfhost/astdump/astdump.exe
tessera=_build/default/bin/main.exe
tokdump=$PWD/_build/default/selfhost/tokdump/tokdump.exe
astdump=$PWD/_build/default/selfhost/astdump/astdump.exe
book=${TESSERA_BOOK:-../tessera-book}/examples
out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT
"$tessera" build selfhost/tokens.tsr -o "$out/tokens"
"$tessera" build selfhost/parse.tsr -o "$out/parse"

# The repository's files (the new ones in selfhost/ too, before they are
# committed).
files=$({ git ls-files '*.tsr'; ls selfhost/*.tsr; } | sort -u)
failed=0

# compare WANT_CMD GOT_PROGRAM DIR FILE: run both on DIR/FILE, the Tessera
# one with read access to DIR only
compare() {
  (cd "$3" && "$1" "$4") > "$out/want"
  "$out/$2" --grant "fs=$(cd "$3" && pwd):ro" -- "$4" > "$out/got" || true
  if cmp -s "$out/want" "$out/got"; then
    return 0
  fi
  echo "differs: $3/$4"
  diff "$out/want" "$out/got" | head -6
  return 1
}

# check LABEL WANT_CMD GOT_PROGRAM FILE...: compare on each file and count
check() {
  label=$1 want=$2 got=$3
  shift 3
  n=0 bad=0
  for f in "$@"; do
    n=$((n + 1))
    compare "$want" "$got" "$(dirname "$f")" "$(basename "$f")" || bad=$((bad + 1))
  done
  echo "$((n - bad))/$n $label"
  [ "$bad" -eq 0 ] || failed=1
}

check "files lex the same" "$tokdump" tokens $files
check "files parse the same" "$astdump" parse $files
check "edge cases parse the same" "$astdump" parse selfhost/tests/*.tsr
if [ -d "$book" ]; then
  check "book examples parse the same" "$astdump" parse $(find "$book" -name '*.tsr' | sort)
else
  echo "skipped the book's examples: no $book"
fi
[ "$failed" -eq 0 ]
