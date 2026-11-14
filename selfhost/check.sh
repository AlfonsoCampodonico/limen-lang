#!/bin/sh
# Compare the Tessera lexer, parser, module loader and type checker
# (selfhost/*.tsr) with the OCaml ones (src/lexer.ml, src/parser.ml,
# src/modules.ml and src/check.ml, through selfhost/tokdump,
# selfhost/astdump, selfhost/loaddump and selfhost/checkdump): same tokens,
# same trees, same loaded programs, same types and rewrites, same
# positions, same errors.
#   sh selfhost/check.sh
# They are compared on every .tsr file in the repository, on the edge cases
# in selfhost/tests (the loader's in selfhost/tests/load, one directory
# each, the checker's in selfhost/tests/check), and on the book's examples
# when the book is checked out next to this repository (or at
# $TESSERA_BOOK).
set -e
cd "$(dirname "$0")/.."
dune build ./bin/main.exe ./selfhost/tokdump/tokdump.exe ./selfhost/astdump/astdump.exe ./selfhost/loaddump/loaddump.exe ./selfhost/checkdump/checkdump.exe
tessera=_build/default/bin/main.exe
tokdump=$PWD/_build/default/selfhost/tokdump/tokdump.exe
astdump=$PWD/_build/default/selfhost/astdump/astdump.exe
loaddump=$PWD/_build/default/selfhost/loaddump/loaddump.exe
checkdump=$PWD/_build/default/selfhost/checkdump/checkdump.exe
book=${TESSERA_BOOK:-../tessera-book}/examples
out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT
"$tessera" build selfhost/tokens.tsr -o "$out/tokens"
"$tessera" build selfhost/parse.tsr -o "$out/parse"
"$tessera" build selfhost/load.tsr -o "$out/load"
"$tessera" build selfhost/check.tsr -o "$out/check"

# The repository's files (the new ones in selfhost/ too, before they are
# committed), and the loader's edge cases among them: each file of
# selfhost/tests/load is loaded as a program.
files=$({ git ls-files '*.tsr'; ls selfhost/*.tsr; find selfhost/tests/load selfhost/tests/check -name '*.tsr'; } | sort -u)
load_cases=$(echo "$files" | grep '^selfhost/tests/load/')
check_cases=$(echo "$files" | grep '^selfhost/tests/check/')
programs=$(echo "$files" | grep -v '^selfhost/tests/load/' | grep -v '^selfhost/tests/check/')
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

# The checker is ported in two parts. These files use what the second part
# will port, so they are not compared yet: interfaces and bounds on them,
# `dyn` values, the `Error` interface on a program's own types, bounds on
# structs and enums, foreign functions (C). Each is named with what it
# uses; a book example as book/PATH. The list is to shrink to nothing: a
# file on it must still need part 2 (the Tessera checker says so), and one
# that compares the same must come off it.
part2="
examples/dyn.tsr: interfaces, dyn values, interface bounds
examples/ffi.tsr: foreign functions
examples/generics.tsr: interfaces, bounds on structs and enums, generic interface bounds, dyn values
examples/interfaces.tsr: interfaces, interface bounds
examples/notes.tsr: foreign functions (std sql)
examples/todo_api.tsr: foreign functions (std sql)
examples/tour.tsr: interfaces, dyn values, the Error interface
grammar/tests/accept/foreign-c.tsr: foreign functions
grammar/tests/accept/interfaces.tsr: interfaces
selfhost/tests/ok-foreign.tsr: foreign functions
src/std/errors.tsr: dyn values
src/std/sql.tsr: foreign functions
book/ch08-03-context/main.tsr: dyn values (std errors)
book/ch08-04-own-error/main.tsr: the Error interface
book/ch10-02-interfaces/main.tsr: interfaces, interface bounds
book/ch10-03-unsatisfied/main.tsr: interfaces, interface bounds
book/ch10-04-generic-interfaces/main.tsr: interfaces, generic interface bounds
book/ch10-05-dyn/main.tsr: interfaces, dyn values
book/ch11-04-fakes/main.tsr: interfaces, interface bounds
book/ch12-07-sandbox/main.tsr: foreign functions
book/ch14-05-sql/main.tsr: foreign functions (std sql)
book/ch15-01-strtol/main.tsr: foreign functions
book/ch15-02-more-c/main.tsr: foreign functions
book/ch15-03-closure-callback/main.tsr: foreign functions
book/ch16-links/api.tsr: foreign functions
book/ch16-links/main.tsr: foreign functions, dyn values
book/ch16-links/store.tsr: foreign functions
"
skipped=0

# compare_check DIR FILE: check DIR/FILE with both checkers, as
# compare_load loads it
compare_check() {
  (cd "$1" && "$checkdump" "$2") > "$out/want" 2>&1 || true
  "$out/check" --grant "fs=$grant:ro" -- "$std" "$(inside "$(cd "$1" && pwd)")" "$2" > "$out/got" 2>&1 || true
  key=$1/$2
  if [ -d "$book" ]; then
    key=$(echo "$key" | sed "s|^$book/|book/|")
  fi
  if echo "$part2" | grep -q "^$key: "; then
    if cmp -s "$out/want" "$out/got"; then
      echo "compares the same, so it comes off the part-2 list: $key"
      return 1
    fi
    if grep -q '^UNPORTED' "$out/got"; then
      skipped=$((skipped + 1))
      return 0
    fi
  fi
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
  if [ "$skipped" -gt 0 ]; then
    echo "$((n - bad - skipped))/$((n - skipped)) $label ($skipped on the part-2 list)"
  else
    echo "$((n - bad))/$n $label"
  fi
  skipped=0
  [ "$bad" -eq 0 ] || failed=1
}

check "files lex the same" compare "$tokdump" tokens -- $files
check "files parse the same" compare "$astdump" parse -- $files
check "edge cases parse the same" compare "$astdump" parse -- selfhost/tests/*.tsr
check "files load the same" compare_load -- $programs
check "edge cases load the same" compare_load -- $load_cases
check "files check the same" compare_check -- $programs
check "edge cases check the same" compare_check -- $check_cases
if [ -d "$book" ]; then
  examples=$(find "$book" -name '*.tsr' | sort)
  check "book examples parse the same" compare "$astdump" parse -- $examples
  check "book examples load the same" compare_load -- $examples
  check "book examples check the same" compare_check -- $examples
else
  echo "skipped the book's examples: no $book"
fi
[ "$failed" -eq 0 ]
