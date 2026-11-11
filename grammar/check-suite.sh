#!/bin/sh
# Check the grammar against every program in the test suite: programs that
# parse must be accepted, programs the parser rejects must be rejected.
set -e
cd "$(dirname "$0")/.."
dir=$(mktemp -d)
dune build
WRIT_SKIP_NATIVE=1 WRIT_DUMP_DIR="$dir" ./_build/default/test/test_writ.exe > /dev/null
python3 grammar/gbnf_check.py grammar/writ.gbnf "$dir"/accept/*.wr --reject "$dir"/reject/*.wr
rm -rf "$dir"
