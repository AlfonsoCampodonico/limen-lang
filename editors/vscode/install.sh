#!/bin/sh
# Build the Vela extension and install it into VS Code.
# Usage: editors/vscode/install.sh   (needs Node and the `code` command)
set -e
cd "$(dirname "$0")"
npm install --no-audit --no-fund --silent
npm test --silent
npx --no-install vsce package --allow-missing-repository --skip-license -o vela.vsix
code --install-extension vela.vsix --force
echo "Installed. Reload VS Code windows that have .vela files open (Developer: Reload Window)."
