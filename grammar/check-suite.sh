#!/bin/sh
# Check the grammar against every program in the test suite: programs that
# parse must be accepted, programs the parser rejects must be rejected.
set -e
cd "$(dirname "$0")/.."
dir=$(mktemp -d)
dune build --root .
TESSERA_SKIP_NATIVE=1 TESSERA_DUMP_DIR="$dir" ./_build/default/test/test_tessera.exe > /dev/null
python3 grammar/gbnf_check.py grammar/tessera.gbnf "$dir"/accept/*.tsr --reject "$dir"/reject/*.tsr
rm -rf "$dir"
