#!/usr/bin/env python3
"""End-to-end check of `writ lsp` over stdio: framing, diagnostics, hover,
completion, shutdown and exit.  Usage: python3 test/lsp_stdio.py WRIT_EXE"""
import json, subprocess, sys

exe = sys.argv[1]
p = subprocess.Popen([exe, "lsp"], stdin=subprocess.PIPE, stdout=subprocess.PIPE)


def send(msg):
    body = json.dumps(msg).encode()
    p.stdin.write(b"Content-Length: %d\r\n\r\n" % len(body) + body)
    p.stdin.flush()


def recv():
    length = None
    while True:
        line = p.stdout.readline().decode().strip()
        if not line:
            break
        k, v = line.split(":", 1)
        if k.lower() == "content-length":
            length = int(v)
    return json.loads(p.stdout.read(length))


uri = "file:///tmp/e2e.wr"
text = 'fn main(root: Root) {\n    let n = 41 + 1\n    print("x")\n    root.stdout.print("{n}")\n}\n'
send({"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {}})
init = recv()
assert init["result"]["capabilities"]["completionProvider"]["triggerCharacters"] == ["."], init
send({"jsonrpc": "2.0", "method": "initialized", "params": {}})
send({"jsonrpc": "2.0", "method": "textDocument/didOpen", "params": {"textDocument": {"uri": uri, "languageId": "writ", "version": 1, "text": text}}})
diags = recv()
assert diags["method"] == "textDocument/publishDiagnostics", diags
assert [d["code"] for d in diags["params"]["diagnostics"]] == ["E0302"], diags
assert diags["params"]["diagnostics"][0]["range"]["start"] == {"line": 2, "character": 4}, diags
send({"jsonrpc": "2.0", "id": 2, "method": "textDocument/hover", "params": {"textDocument": {"uri": uri}, "position": {"line": 1, "character": 8}}})
hover = recv()
assert hover["id"] == 2 and "let n: Int" in hover["result"]["contents"]["value"], hover
def complete(line, ch, rid):
    send({"jsonrpc": "2.0", "id": rid, "method": "textDocument/completion", "params": {"textDocument": {"uri": uri}, "position": {"line": line, "character": ch}}})
    return [i["label"] for i in recv()["result"]]


labels = complete(3, 9, 3)   # root.|stdout
assert "stdout" in labels and "net" in labels, labels
labels = complete(3, 16, 5)  # root.stdout.|print
assert labels == ["print", "write"], labels
send({"jsonrpc": "2.0", "id": 4, "method": "shutdown"})
assert recv()["id"] == 4
send({"jsonrpc": "2.0", "method": "exit"})
assert p.wait(timeout=5) == 0
print("lsp stdio: ok")
