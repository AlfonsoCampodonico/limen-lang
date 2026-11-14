# Tessera for VS Code

A local extension, not published to the marketplace.

- **Highlighting** for `.tessera` files and for ```` ```tessera ```` blocks in Markdown. Capability types (`Net`, `Fs`, `Clock` and the rest) get their own scope, `support.type.capability.tessera`, so authority entering a function stands out.
- **The language server** (`tessera lsp`): errors as you type, hover, go to definition, references, rename, completion and quick fixes.
- Comment toggling, bracket matching, auto-closing quotes and indentation.

## Install

```sh
editors/vscode/install.sh
```

It installs the npm dependencies, runs the grammar test, packages `tessera.vsix` and installs it with `code --install-extension`. Run it again after changing the extension, then reload the VS Code window.

## Finding `tessera`

The extension runs the first of:

1. the `tessera.path` setting, from your user settings only (a workspace's `.vscode/settings.json` cannot set it);
2. `tessera` on the PATH;
3. in a folder you have trusted (VS Code's Workspace Trust), `_build/default/bin/main.exe` in that folder itself (not a folder above it, and not through a symlink leading out of it): this repository, after `dune build`.

So opening a folder you haven't trusted never runs a program that folder supplies. Highlighting works without any of them. Turn the language server off with `tessera.languageServer.enabled`, and restart it with **Tessera: Restart Language Server**.

## Coloring capabilities

Themes don't know the capability scope, so capabilities get the theme's color for built-in types. To give them their own, add this to your settings:

```json
"editor.tokenColorCustomizations": {
  "textMateRules": [
    { "scope": "support.type.capability.tessera", "settings": { "foreground": "#E5A50A", "fontStyle": "bold" } }
  ]
}
```

## Changing the grammar

Edit `tools/build-grammar.py`, not the JSON, then:

```sh
python3 tools/build-grammar.py && npm test
```

The test tokenizes a sample with VS Code's own TextMate engine and checks each kind of token's scope, then checks that every program in `examples/`, `bench/` and `src/std/` ends outside any string.
