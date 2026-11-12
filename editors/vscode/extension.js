// Starts `vela lsp` for .vela files. Highlighting needs none of this: it
// comes from the grammar in syntaxes/.
const vscode = require("vscode");
const fs = require("fs");
const path = require("path");
const { LanguageClient } = require("vscode-languageclient/node");

let client;
let output;

// The `vela` to run: the user setting, else `vela` on the PATH, else (in a
// trusted folder only) the build in an open folder, which is this repository
// while working on Vela itself. A workspace can never choose the executable:
// opening a folder must not run a program that folder supplies.
function findVela() {
  const set = vscode.workspace.getConfiguration("vela").inspect("path");
  const userPath = set && (set.globalValue || set.defaultValue);
  if (userPath) return userPath;
  const dirs = (process.env.PATH || "").split(path.delimiter);
  for (const d of dirs) {
    if (!path.isAbsolute(d)) continue; // a relative PATH entry would resolve inside the workspace
    const p = path.join(d, "vela");
    if (fs.existsSync(p)) return p;
  }
  if (!vscode.workspace.isTrusted) return null;
  for (const folder of vscode.workspace.workspaceFolders || []) {
    let dir = folder.uri.fsPath;
    for (;;) {
      const p = path.join(dir, "_build", "default", "bin", "main.exe");
      if (fs.existsSync(p)) return p;
      const up = path.dirname(dir);
      if (up === dir) break;
      dir = up;
    }
  }
  return null;
}

async function start() {
  if (!vscode.workspace.getConfiguration("vela").get("languageServer.enabled")) return;
  const command = findVela();
  if (!command) {
    vscode.window.showWarningMessage(
      vscode.workspace.isTrusted
        ? "Vela: `vela` was not found, so there are no errors, hover or rename (highlighting still works). Put it on your PATH or set `vela.path` in your user settings."
        : "Vela: `vela` was not found on your PATH or in your user settings, and this folder is not trusted, so its own build is not used. Highlighting still works."
    );
    return;
  }
  output.appendLine(`starting ${command} lsp`);
  client = new LanguageClient(
    "vela",
    "Vela",
    { command, args: ["lsp"] },
    { documentSelector: [{ scheme: "file", language: "vela" }], outputChannel: output }
  );
  try {
    await client.start();
  } catch (e) {
    client = undefined;
    vscode.window.showErrorMessage(`Vela: could not start \`${command} lsp\`: ${e.message}`);
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
  output = vscode.window.createOutputChannel("Vela");
  context.subscriptions.push(
    output,
    vscode.commands.registerCommand("vela.restartLanguageServer", async () => {
      await stop();
      await start();
    }),
    vscode.workspace.onDidGrantWorkspaceTrust(async () => {
      if (!client) await start();
    }),
    vscode.workspace.onDidChangeConfiguration(async (e) => {
      if (e.affectsConfiguration("vela")) {
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
