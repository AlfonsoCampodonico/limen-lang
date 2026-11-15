#!/bin/sh
# Build the Limen extension and install it into VS Code.
# Usage: editors/vscode/install.sh   (needs Node and the `code` command)
set -e
cd "$(dirname "$0")"
npm install --no-audit --no-fund --silent
npm test --silent
npx --no-install vsce package --allow-missing-repository --skip-license -o limen.vsix
code --install-extension limen.vsix --force
echo "Installed. Reload VS Code windows that have .lmn files open (Developer: Reload Window)."
