#!/bin/sh
# Compare the Tessera lexer (selfhost/lexer.tsr) with the OCaml one
# (src/lexer.ml, through selfhost/tokdump) on every .tsr file in the
# repository: same tokens, same positions, same errors.
#   sh selfhost/check.sh
set -e
cd "$(dirname "$0")/.."
dune build ./bin/main.exe ./selfhost/tokdump/tokdump.exe
tessera=_build/default/bin/main.exe
tokdump=_build/default/selfhost/tokdump/tokdump.exe
out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT
"$tessera" build selfhost/tokens.tsr -o "$out/tokens"
n=0
bad=0
for f in $(git ls-files '*.tsr'); do
  n=$((n + 1))
  "$tokdump" "$f" > "$out/want"
  "$out/tokens" --grant "fs=$PWD:ro" -- "$f" > "$out/got" || true
  if ! cmp -s "$out/want" "$out/got"; then
    bad=$((bad + 1))
    echo "differs: $f"
    diff "$out/want" "$out/got" | head -6
  fi
done
echo "$((n - bad))/$n files lex the same"
[ "$bad" -eq 0 ]
