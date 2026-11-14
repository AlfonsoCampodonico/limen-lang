#!/bin/sh
# Compare the Tessera lexer, parser, module loader, type checker and
# lowering (selfhost/*.tsr) with the OCaml ones (src/lexer.ml,
# src/parser.ml, src/modules.ml, src/check.ml with the capability checks
# of src/package.ml, and src/lower.ml with the IR passes Native.c_source
# runs, through selfhost/tokdump, selfhost/astdump, selfhost/loaddump,
# selfhost/checkdump and selfhost/irdump): same tokens, same trees, same
# loaded programs, same types and rewrites, same capabilities per
# function, same IR, same positions, same errors.
#   sh selfhost/check.sh
# They are compared on every .tsr file in the repository, on the edge cases
# in selfhost/tests (the loader's in selfhost/tests/load, one directory
# each, the checker's in selfhost/tests/check, the lowering's in
# selfhost/tests/ir), and on the book's examples
# when the book is checked out next to this repository (or at
# $TESSERA_BOOK).
set -e
cd "$(dirname "$0")/.."
dune build ./bin/main.exe ./selfhost/tokdump/tokdump.exe ./selfhost/astdump/astdump.exe ./selfhost/loaddump/loaddump.exe ./selfhost/checkdump/checkdump.exe ./selfhost/irdump/irdump.exe
tessera=_build/default/bin/main.exe
tokdump=$PWD/_build/default/selfhost/tokdump/tokdump.exe
astdump=$PWD/_build/default/selfhost/astdump/astdump.exe
loaddump=$PWD/_build/default/selfhost/loaddump/loaddump.exe
checkdump=$PWD/_build/default/selfhost/checkdump/checkdump.exe
irdump=$PWD/_build/default/selfhost/irdump/irdump.exe
book=${TESSERA_BOOK:-../tessera-book}/examples
out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT
"$tessera" build selfhost/tokens.tsr -o "$out/tokens"
"$tessera" build selfhost/parse.tsr -o "$out/parse"
"$tessera" build selfhost/load.tsr -o "$out/load"
"$tessera" build selfhost/check.tsr -o "$out/check"
"$tessera" build selfhost/lowered.tsr -o "$out/lowered"

# The repository's files (the new ones in selfhost/ too, before they are
# committed), and the loader's edge cases among them: each file of
# selfhost/tests/load is loaded as a program.
files=$({ git ls-files '*.tsr'; ls selfhost/*.tsr; find selfhost/tests/load selfhost/tests/check selfhost/tests/ir -name '*.tsr'; } | sort -u)
load_cases=$(echo "$files" | grep '^selfhost/tests/load/')
check_cases=$(echo "$files" | grep '^selfhost/tests/check/')
ir_cases=$(echo "$files" | grep '^selfhost/tests/ir/')
programs=$(echo "$files" | grep -v '^selfhost/tests/load/' | grep -v '^selfhost/tests/check/' | grep -v '^selfhost/tests/ir/')
failed=0

# compare WANT_CMD GOT_PROGRAM DIR FILE: run both on DIR/FILE, the Tessera
# one with read access to DIR only
compare() {
  (cd "$3" && "$1" "$4") > "$out/want"
  "$out/$2" --grant "fs=$(cd "$3" && pwd):ro" -- "$4" > "$out/got" || true
  same "$3/$4"
}

# The loaders read more than the file's directory: the package around it,
# its dependencies, the standard library. The Tessera one is granted the
# directory that holds this repository and the book, finds the standard
# library in this repository's src/std, and runs in DIR as the OCaml one
# does.
grant=$(cd .. && pwd)
if [ -d "$book" ]; then
  bookdir=$(cd "$book" && pwd)
  while [ "$grant" != / ] && case "$bookdir/" in "$grant/"*) false ;; *) true ;; esac; do
    grant=$(dirname "$grant")
  done
fi
# inside DIR: a path in the granted directory
inside() {
  if [ "$grant" = / ]; then echo "${1#/}"; else echo "${1#"$grant"/}"; fi
}
std=$(inside "$PWD/src/std")

# compare_load DIR FILE: load DIR/FILE with both loaders
compare_load() {
  (cd "$1" && "$loaddump" "$2") > "$out/want" 2>&1 || true
  "$out/load" --grant "fs=$grant:ro" -- "$std" "$(inside "$(cd "$1" && pwd)")" "$2" > "$out/got" 2>&1 || true
  same "$1/$2"
}

# compare_check DIR FILE: check DIR/FILE with both checkers, as
# compare_load loads it
compare_check() {
  (cd "$1" && "$checkdump" "$2") > "$out/want" 2>&1 || true
  "$out/check" --grant "fs=$grant:ro" -- "$std" "$(inside "$(cd "$1" && pwd)")" "$2" > "$out/got" 2>&1 || true
  same "$1/$2"
}

# compare_ir DIR FILE: lower DIR/FILE with both backends, as compare_load
# loads it
compare_ir() {
  (cd "$1" && "$irdump" "$2") > "$out/want" 2>&1 || true
  "$out/lowered" --grant "fs=$grant:ro" -- "$std" "$(inside "$(cd "$1" && pwd)")" "$2" > "$out/got" 2>&1 || true
  same "$1/$2"
}

same() {
  if cmp -s "$out/want" "$out/got"; then
    return 0
  fi
  echo "differs: $1"
  diff "$out/want" "$out/got" | head -6
  return 1
}

# check LABEL COMPARE [WANT_CMD GOT_PROGRAM] -- FILE...: compare on each
# file and count
check() {
  label=$1
  shift
  how=
  while [ "$1" != -- ]; do
    how="$how $1"
    shift
  done
  shift
  n=0 bad=0
  for f in "$@"; do
    n=$((n + 1))
    $how "$(dirname "$f")" "$(basename "$f")" || bad=$((bad + 1))
  done
  echo "$((n - bad))/$n $label"
  [ "$bad" -eq 0 ] || failed=1
}

check "files lex the same" compare "$tokdump" tokens -- $files
check "files parse the same" compare "$astdump" parse -- $files
check "edge cases parse the same" compare "$astdump" parse -- selfhost/tests/*.tsr
check "files load the same" compare_load -- $programs
check "edge cases load the same" compare_load -- $load_cases
check "files check the same" compare_check -- $programs
check "edge cases check the same" compare_check -- $check_cases
check "files lower the same" compare_ir -- $programs
check "edge cases lower the same" compare_ir -- $ir_cases
if [ -d "$book" ]; then
  examples=$(find "$book" -name '*.tsr' | sort)
  check "book examples parse the same" compare "$astdump" parse -- $examples
  check "book examples load the same" compare_load -- $examples
  check "book examples check the same" compare_check -- $examples
  check "book examples lower the same" compare_ir -- $examples
else
  echo "skipped the book's examples: no $book"
fi
[ "$failed" -eq 0 ]
