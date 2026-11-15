#!/bin/sh
# Check the grammar against every program in the test suite: programs that
# parse must be accepted, programs the parser rejects must be rejected.
set -e
cd "$(dirname "$0")/.."
dir=$(mktemp -d)
dune build --root .
LIMEN_SKIP_NATIVE=1 LIMEN_DUMP_DIR="$dir" ./_build/default/test/test_limen.exe > /dev/null
python3 grammar/gbnf_check.py grammar/limen.gbnf "$dir"/accept/*.lmn --reject "$dir"/reject/*.lmn
rm -rf "$dir"
