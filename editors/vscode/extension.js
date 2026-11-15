// Starts `tessera lsp` for .tsr files. Highlighting needs none of this: it
// comes from the grammar in syntaxes/.
const vscode = require("vscode");
const fs = require("fs");
const path = require("path");
const { LanguageClient } = require("vscode-languageclient/node");

let client;
let output;

// The `tessera` to run: the user setting, else `tessera` on the PATH, else (in a
// trusted folder only) the build in an open folder, which is this repository
// while working on Tessera itself. A workspace can never choose the executable:
// opening a folder must not run a program that folder supplies.
function findTessera() {
  const set = vscode.workspace.getConfiguration("tessera").inspect("path");
  const userPath = set && (set.globalValue || set.defaultValue);
  if (userPath) return userPath;
  const dirs = (process.env.PATH || "").split(path.delimiter);
  for (const d of dirs) {
    if (!path.isAbsolute(d)) continue; // a relative PATH entry would resolve inside the workspace
    const p = path.join(d, "tessera");
    if (fs.existsSync(p)) return p;
  }
  if (!vscode.workspace.isTrusted) return null;
  // only the trusted folder itself: not its parents, which were never
  // trusted, and not through a symlink that leads out of it
  for (const folder of vscode.workspace.workspaceFolders || []) {
    if (folder.uri.scheme !== "file") continue;
    try {
      const root = fs.realpathSync(folder.uri.fsPath);
      const p = fs.realpathSync(path.join(root, "_build", "default", "bin", "main.exe"));
      if (p.startsWith(root + path.sep) && fs.statSync(p).isFile()) return p;
    } catch (_) {
      // no build in this folder
    }
  }
  return null;
}

async function start() {
  if (!vscode.workspace.getConfiguration("tessera").get("languageServer.enabled")) return;
  const command = findTessera();
  if (!command) {
    vscode.window.showWarningMessage(
      vscode.workspace.isTrusted
        ? "Tessera: `tessera` was not found, so there are no errors, hover or rename (highlighting still works). Put it on your PATH or set `tessera.path` in your user settings."
        : "Tessera: `tessera` was not found on your PATH or in your user settings, and this folder is not trusted, so its own build is not used. Highlighting still works."
    );
    return;
  }
  output.appendLine(`starting ${command} lsp`);
  client = new LanguageClient(
    "tessera",
    "Tessera",
    { command, args: ["lsp"] },
    { documentSelector: [{ scheme: "file", language: "tessera" }], outputChannel: output }
  );
  try {
    await client.start();
  } catch (e) {
    client = undefined;
    vscode.window.showErrorMessage(`Tessera: could not start \`${command} lsp\`: ${e.message}`);
  }
}

async function stop() {
  if (client) {
    const c = client;
    client = undefined;
    await c.stop();
  }
}

async function activate(context) {
  output = vscode.window.createOutputChannel("Tessera");
  context.subscriptions.push(
    output,
    vscode.commands.registerCommand("tessera.restartLanguageServer", async () => {
      await stop();
      await start();
    }),
    vscode.workspace.onDidGrantWorkspaceTrust(async () => {
      if (!client) await start();
    }),
    vscode.workspace.onDidChangeConfiguration(async (e) => {
      if (e.affectsConfiguration("tessera")) {
        await stop();
        await start();
      }
    })
  );
  await start();
}

function deactivate() {
  return stop();
}

module.exports = { activate, deactivate };
