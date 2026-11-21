#!/bin/sh
# Compare the Limen lexer, parser, module loader, type checker and
# lowering (selfhost/*.lmn) with the OCaml ones (src/lexer.ml,
# src/parser.ml, src/modules.ml, src/check.ml with the capability checks
# of src/package.ml, and src/lower.ml with the IR passes Native.c_source
# runs, through selfhost/tokdump, selfhost/astdump, selfhost/loaddump,
# selfhost/checkdump and selfhost/irdump): same tokens, same trees, same
# loaded programs, same types and rewrites, same capabilities per
# function, same IR, same positions, same errors. Then the compilers
# themselves: selfhost/limen.lmn and bin/main.ml give the same C for
# `build --emit-c` (and for the test binary, against selfhost/cdump), with
# the same output, the same diagnostics on stderr and the same exit status
# when there is no C; and programs built
# by the Limen compiler run as those the OCaml compiler builds.
#   sh selfhost/check.sh
# They are compared on every .lmn file in the repository, on the edge cases
# in selfhost/tests (the loader's in selfhost/tests/load, one directory
# each, the checker's in selfhost/tests/check, the lowering's in
# selfhost/tests/ir, C generation's in selfhost/tests/c), and on the
# book's examples when the book is checked out next to this repository
# (or at $LIMEN_BOOK).
set -e
cd "$(dirname "$0")/.."
dune build ./bin/main.exe ./selfhost/tokdump/tokdump.exe ./selfhost/astdump/astdump.exe ./selfhost/loaddump/loaddump.exe ./selfhost/checkdump/checkdump.exe ./selfhost/irdump/irdump.exe ./selfhost/cdump/cdump.exe
limen=_build/default/bin/main.exe
tokdump=$PWD/_build/default/selfhost/tokdump/tokdump.exe
astdump=$PWD/_build/default/selfhost/astdump/astdump.exe
loaddump=$PWD/_build/default/selfhost/loaddump/loaddump.exe
checkdump=$PWD/_build/default/selfhost/checkdump/checkdump.exe
irdump=$PWD/_build/default/selfhost/irdump/irdump.exe
cdump=$PWD/_build/default/selfhost/cdump/cdump.exe
limen_abs=$PWD/$limen
book=${LIMEN_BOOK:-../limen-book}/examples
# in the repository, so the Limen compiler's grant (see tsc) holds it
mkdir -p _build
out=$(mktemp -d "$PWD/_build/check.XXXXXX")
trap 'rm -rf "$out"' EXIT
"$limen" build selfhost/tokens.lmn -o "$out/tokens"
"$limen" build selfhost/parse.lmn -o "$out/parse"
"$limen" build selfhost/load.lmn -o "$out/load"
"$limen" build selfhost/check.lmn -o "$out/check"
"$limen" build selfhost/lowered.lmn -o "$out/lowered"
"$limen" build selfhost/limen.lmn -o "$out/tsc"

# The repository's files (the new ones in selfhost/ too, before they are
# committed), and the loader's edge cases among them: each file of
# selfhost/tests/load is loaded as a program.
files=$({ git ls-files '*.lmn'; ls selfhost/*.lmn; find selfhost/tests/load selfhost/tests/check selfhost/tests/ir selfhost/tests/c -name '*.lmn'; } | sort -u)
load_cases=$(echo "$files" | grep '^selfhost/tests/load/')
check_cases=$(echo "$files" | grep '^selfhost/tests/check/')
ir_cases=$(echo "$files" | grep '^selfhost/tests/ir/')
c_cases=$(echo "$files" | grep '^selfhost/tests/c/')
programs=$(echo "$files" | grep -v '^selfhost/tests/load/' | grep -v '^selfhost/tests/check/' | grep -v '^selfhost/tests/ir/' | grep -v '^selfhost/tests/c/')
failed=0

# compare WANT_CMD GOT_PROGRAM DIR FILE: run both on DIR/FILE, the Limen
# one with read access to DIR only
compare() {
  (cd "$3" && "$1" "$4") > "$out/want"
  "$out/$2" --grant "fs=$(cd "$3" && pwd):ro" -- "$4" > "$out/got" || true
  same "$3/$4"
}

# The loaders read more than the file's directory: the package around it,
# its dependencies, the files it embeds. The Limen one is granted the
# directory that holds this repository and the book, and runs in DIR as the
# OCaml one does; both have the standard library built in.
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

# compare_load DIR FILE: load DIR/FILE with both loaders
compare_load() {
  (cd "$1" && "$loaddump" "$2") > "$out/want" 2>&1 || true
  "$out/load" --grant "fs=$grant:ro" -- "$(inside "$(cd "$1" && pwd)")" "$2" > "$out/got" 2>&1 || true
  same "$1/$2"
}

# compare_check DIR FILE: check DIR/FILE with both checkers, as
# compare_load loads it
compare_check() {
  (cd "$1" && "$checkdump" "$2") > "$out/want" 2>&1 || true
  "$out/check" --grant "fs=$grant:ro" -- "$(inside "$(cd "$1" && pwd)")" "$2" > "$out/got" 2>&1 || true
  same "$1/$2"
}

# compare_ir DIR FILE: lower DIR/FILE with both backends, as compare_load
# loads it
compare_ir() {
  (cd "$1" && "$irdump" "$2") > "$out/want" 2>&1 || true
  "$out/lowered" --grant "fs=$grant:ro" -- "$(inside "$(cd "$1" && pwd)")" "$2" > "$out/got" 2>&1 || true
  same "$1/$2"
}

# tsc ARGS...: the Limen compiler, in the current directory, granted
# what it reads and writes: this repository (where $out is), or with the
# book the directory that holds both. Never a system directory: on Linux a
# writable grant over /usr or /lib leaves the C compiler unable to start.
# Then the programs a build runs and the variables it reads.
tsc_grant=$PWD
if [ -d "$book" ]; then
  while [ "$tsc_grant" != / ] && case "$bookdir/" in "$tsc_grant/"*) false ;; *) true ;; esac; do
    tsc_grant=$(dirname "$tsc_grant")
  done
  if [ "$tsc_grant" = / ]; then
    echo "the book ($bookdir) and this repository share no directory but /: put them side by side, or set LIMEN_BOOK=none" >&2
    exit 1
  fi
fi
tsc() {
  "$out/tsc" --grant "fs=$tsc_grant" \
    --grant env=PWD --grant env=CC --grant env=PATH --grant env=HOME \
    --grant env=LIMEN_CACHE --grant env=XDG_CACHE_HOME --grant env=DEVELOPER_DIR --grant env=LIMEN_OPENSSL \
    --grant env=LIBRARY_PATH --grant env=CPATH --grant env=C_INCLUDE_PATH --grant env=SDKROOT --grant env=MACOSX_DEPLOYMENT_TARGET \
    --grant "exec=${CC:-cc}" --grant exec=readlink --grant exec=cp --grant exec=touch --grant exec=pkg-config \
    -- "$@"
}

# compare_c DIR FILE: `build --emit-c` with both compilers: the output, the
# diagnostics on stderr and the exit status, then the C
compare_c() {
  rm -f "$out/cw.c" "$out/cg.c"
  (cd "$1" && "$limen_abs" build "$2" --emit-c -o "$out/cw") > "$out/want" 2> "$out/want.err" && echo "exit 0" >> "$out/want" || echo "exit $?" >> "$out/want"
  (cd "$1" && tsc build "$2" --emit-c -o "$out/cg") > "$out/got" 2> "$out/got.err" && echo "exit 0" >> "$out/got" || echo "exit $?" >> "$out/got"
  if [ -f "$out/cw.c" ]; then cat "$out/cw.c" >> "$out/want"; fi
  if [ -f "$out/cg.c" ]; then cat "$out/cg.c" >> "$out/got"; fi
  same "$1/$2"
}

# compare_dev_c DIR FILE: the C of a development build, with its `#line`
# directives
compare_dev_c() {
  rm -f "$out/cw.c" "$out/cg.c"
  (cd "$1" && "$limen_abs" build "$2" --emit-c --dev -o "$out/cw") > "$out/want" 2> "$out/want.err" && echo "exit 0" >> "$out/want" || echo "exit $?" >> "$out/want"
  (cd "$1" && tsc build "$2" --emit-c --dev -o "$out/cg") > "$out/got" 2> "$out/got.err" && echo "exit 0" >> "$out/got" || echo "exit $?" >> "$out/got"
  if [ -f "$out/cw.c" ]; then cat "$out/cw.c" >> "$out/want"; fi
  if [ -f "$out/cg.c" ]; then cat "$out/cg.c" >> "$out/got"; fi
  same "$1/$2"
}

# compare_profile_c DIR FILE: the C of a build with --profile
compare_profile_c() {
  rm -f "$out/cw.c" "$out/cg.c"
  (cd "$1" && "$limen_abs" build "$2" --emit-c --profile -o "$out/cw") > "$out/want" 2> "$out/want.err" && echo "exit 0" >> "$out/want" || echo "exit $?" >> "$out/want"
  (cd "$1" && tsc build "$2" --emit-c --profile -o "$out/cg") > "$out/got" 2> "$out/got.err" && echo "exit 0" >> "$out/got" || echo "exit $?" >> "$out/got"
  if [ -f "$out/cw.c" ]; then cat "$out/cw.c" >> "$out/want"; fi
  if [ -f "$out/cg.c" ]; then cat "$out/cg.c" >> "$out/got"; fi
  same "$1/$2"
}

# compare_test_c DIR FILE: the test binary's C, from selfhost/cdump and
# from `build --emit-c --tests`
compare_test_c() {
  rm -f "$out/cw.c" "$out/cg.c"
  (cd "$1" && "$cdump" "$2" "$out/cw") > "$out/want" 2> "$out/want.err" && echo "exit 0" >> "$out/want" || echo "exit $?" >> "$out/want"
  (cd "$1" && tsc build "$2" --emit-c --tests -o "$out/cg") > "$out/got" 2> "$out/got.err" && echo "exit 0" >> "$out/got" || echo "exit $?" >> "$out/got"
  if [ -f "$out/cw.c" ]; then cat "$out/cw.c" >> "$out/want"; fi
  if [ -f "$out/cg.c" ]; then cat "$out/cg.c" >> "$out/got"; fi
  same "$1/$2"
}

# compare_run DIR FILE [ARGS...]: build FILE with both compilers (the
# Limen one through a cache of its own, so it runs the C compiler) and
# run both binaries in DIR with ARGS; a fs=work grant gets a fresh
# directory each. Same output, same exit status.
compare_run() {
  dir=$1 file=$2
  shift 2
  rm -rf "$out/rw" "$out/rg" "$out/work"
  (cd "$dir" && "$limen_abs" build "$file" -o "$out/rw") > "$out/want" 2> "$out/want.err"
  (cd "$dir" && export LIMEN_CACHE="$out/cache" && tsc build "$file" -o "$out/rg") > "$out/got" 2> "$out/got.err"
  for bin in rw rg; do
    mkdir -p "$out/work"
    if [ "$bin" = rw ]; then into=want; else into=got; fi
    (cd "$dir" && "$out/$bin" $(echo "$*" | sed "s|fs=work|fs=$out/work|")) >> "$out/$into" 2>&1 && echo "exit 0" >> "$out/$into" || echo "exit $?" >> "$out/$into"
    (cd "$out/work" && find . -type f | sort) >> "$out/$into"
    rm -rf "$out/work"
  done
  same "$dir/$file"
}

# compare_tests DIR FILE: the test binary from both compilers (the OCaml
# one's C compiled as Native.command does), each test run on its own:
# same output, same exit status
compare_tests() {
  rm -f "$out/tw" "$out/tg"
  (cd "$1" && "$cdump" "$2" "$out/tw" && ${CC:-cc} -O3 -fwrapv -ffp-contract=off -fstack-clash-protection -fexceptions -w -o "$out/tw" "$out/tw.c" -lm) > "$out/want" 2> "$out/want.err"
  (cd "$1" && export LIMEN_CACHE="$out/cache" && tsc build "$2" --tests -o "$out/tg") > "$out/got" 2> "$out/got.err"
  n=$(grep -c '^test ' "$1/$2")
  i=0
  while [ "$i" -lt "$n" ]; do
    (cd "$1" && "$out/tw" "$i") >> "$out/want" 2>&1 && echo "exit 0" >> "$out/want" || echo "exit $?" >> "$out/want"
    (cd "$1" && "$out/tg" "$i") >> "$out/got" 2>&1 && echo "exit 0" >> "$out/got" || echo "exit $?" >> "$out/got"
    i=$((i + 1))
  done
  same "$1/$2"
}

# same FILE: the outputs compared, and the compilers' stderr where a
# comparison keeps it apart (both compilers write their diagnostics there);
# then it is cleared for the next comparison
same() {
  touch "$out/want.err" "$out/got.err"
  if cmp -s "$out/want" "$out/got" && cmp -s "$out/want.err" "$out/got.err"; then
    rm -f "$out/want.err" "$out/got.err"
    return 0
  fi
  echo "differs: $1"
  diff "$out/want" "$out/got" | head -6
  diff "$out/want.err" "$out/got.err" | head -6
  rm -f "$out/want.err" "$out/got.err"
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
check "edge cases parse the same" compare "$astdump" parse -- selfhost/tests/*.lmn
check "files load the same" compare_load -- $programs
check "edge cases load the same" compare_load -- $load_cases
check "files check the same" compare_check -- $programs
check "edge cases check the same" compare_check -- $check_cases
check "files lower the same" compare_ir -- $programs
check "edge cases lower the same" compare_ir -- $ir_cases
check "files compile to the same C" compare_c -- $programs
check "edge cases compile to the same C" compare_c -- $c_cases
check "edge cases compile to the same development C" compare_dev_c -- $c_cases
check "examples compile to the same development C" compare_dev_c -- $(ls examples/*.lmn)
check "edge cases compile to the same profiled C" compare_profile_c -- $c_cases
with_tests=$(grep -l '^test \|^bench ' $programs $c_cases)
check "test binaries compile to the same C" compare_test_c -- $with_tests
if [ -d "$book" ]; then
  examples=$(find "$book" -name '*.lmn' | sort)
  check "book examples parse the same" compare "$astdump" parse -- $examples
  check "book examples load the same" compare_load -- $examples
  check "book examples check the same" compare_check -- $examples
  check "book examples lower the same" compare_ir -- $examples
  check "book examples compile to the same C" compare_c -- $examples
  book_tests=$(grep -l '^test \|^bench ' $examples)
  check "book test binaries compile to the same C" compare_test_c -- $book_tests
  # programs built by both compilers, run: concurrency, HTTP, TLS, JSON,
  # SQL, C calls, embedded files, files, running programs, defer, catch,
  # dyn, tests
  runs=0 bad_runs=0
  run() {
    runs=$((runs + 1))
    compare_run "$book/$1" "$2" $3 || bad_runs=$((bad_runs + 1))
  }
  run ch13-01-group main.lmn
  run ch13-03-channels main.lmn
  run ch13-04-select main.lmn
  run ch13-05-deadline main.lmn
  run ch14-01-files main.lmn "--grant fs=work"
  run ch14-02-http main.lmn "--grant listen=127.0.0.1:0 --grant net=127.0.0.1"
  run ch14-03-tls main.lmn
  run ch14-04-json main.lmn
  run ch14-05-sql main.lmn
  run ch15-01-strtol main.lmn "--grant foreign=c"
  run ch15-02-more-c main.lmn "--grant foreign=c"
  run ch12-08-embed main.lmn
  run ch12-06-exec main.lmn "--grant exec=echo --grant exec=ls"
  run ch08-07-defer main.lmn
  run ch08-06-catch main.lmn
  run ch10-05-dyn main.lmn
  for t in ch11-01-tests/slug.lmn ch11-02-failing/slug.lmn; do
    runs=$((runs + 1))
    compare_tests "$book/$(dirname "$t")" "$(basename "$t")" || bad_runs=$((bad_runs + 1))
  done
  echo "$((runs - bad_runs))/$runs programs run the same built by either compiler"
  [ "$bad_runs" -eq 0 ] || failed=1
else
  echo "skipped the book's examples: no $book"
fi
[ "$failed" -eq 0 ]
