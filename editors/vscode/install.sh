#!/bin/sh
# Build the Tessera extension and install it into VS Code.
# Usage: editors/vscode/install.sh   (needs Node and the `code` command)
set -e
cd "$(dirname "$0")"
npm install --no-audit --no-fund --silent
npm test --silent
npx --no-install vsce package --allow-missing-repository --skip-license -o tessera.vsix
code --install-extension tessera.vsix --force
echo "Installed. Reload VS Code windows that have .tsr files open (Developer: Reload Window)."
