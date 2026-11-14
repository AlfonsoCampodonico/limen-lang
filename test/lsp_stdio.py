#!/usr/bin/env python3
"""End-to-end check of `tessera lsp` over stdio: framing, diagnostics, hover,
completion, definitions, references and rename across modules, shutdown and
exit.  Usage: python3 test/lsp_stdio.py TESSERA_EXE"""
import json, os, subprocess, sys, tempfile

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


uri = "file:///tmp/e2e.tsr"
text = 'fn main(root: Root) {\n    let n = 41 + 1\n    print("x")\n    root.stdout.print("{n}")\n}\n'
send({"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {}})
init = recv()
assert init["result"]["capabilities"]["completionProvider"]["triggerCharacters"] == ["."], init
send({"jsonrpc": "2.0", "method": "initialized", "params": {}})
send({"jsonrpc": "2.0", "method": "textDocument/didOpen", "params": {"textDocument": {"uri": uri, "languageId": "tessera", "version": 1, "text": text}}})
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

# ---------- two modules on disk: definitions, hover, references, rename ----------
d = tempfile.mkdtemp(prefix="tessera-lsp-")
geom = """// Geometry.

// A point.
pub struct Point {
    x: Int,
    y: Int,
}

// Sum of the coordinates.
pub fn total(self: Point) -> Int {
    self.x + self.y
}

pub fn origin() -> Point {
    Point { x: 0, y: 0 }
}
"""
main = """use geom
use json

fn helper(n: Int) -> Int {
    let x = n + 1
    x * 2
}

fn main(root: Root) {
    let p = geom.origin()
    let t = p.total()
    let y = 3
    let s = geom.Point { x: helper(1), y }
    root.stdout.print("{t} {s.x} {helper(2)}")
}
"""
with open(os.path.join(d, "geom.tsr"), "w") as f:
    f.write(geom)
main_path = os.path.join(d, "main.tsr")
with open(main_path, "w") as f:
    f.write(main)
muri = "file://" + main_path
guri = "file://" + os.path.join(d, "geom.tsr")
send({"jsonrpc": "2.0", "method": "textDocument/didOpen", "params": {"textDocument": {"uri": muri, "languageId": "tessera", "version": 1, "text": main}}})
assert recv()["params"]["diagnostics"] == []
rid = [100]


def req(method, line, ch, **extra):
    rid[0] += 1
    params = {"textDocument": {"uri": muri}, "position": {"line": line, "character": ch}}
    params.update(extra)
    send({"jsonrpc": "2.0", "id": rid[0], "method": method, "params": params})
    r = recv()
    assert r["id"] == rid[0], r
    return r


def at(loc):
    return (loc["uri"], loc["range"]["start"]["line"], loc["range"]["start"]["character"])


# go to definition in another module, for a function and a method
assert at(req("textDocument/definition", 9, 18)["result"]) == (guri, 13, 7), "origin"
assert at(req("textDocument/definition", 10, 15)["result"]) == (guri, 9, 7), "total"
assert at(req("textDocument/definition", 12, 19)["result"]) == (guri, 3, 11), "Point"
assert at(req("textDocument/definition", 13, 30)["result"]) == (guri, 4, 4), "field x"
# hover shows the declaration and its doc comment
h = req("textDocument/hover", 10, 15)["result"]["contents"]["value"]
assert "fn total(self: Point) -> Int" in h and "Sum of the coordinates." in h, h
# references: definition and both calls
refs = req("textDocument/references", 3, 4, context={"includeDeclaration": True})["result"]
assert sorted(at(l)[1:] for l in refs) == [(3, 3), (12, 28), (13, 34)], refs
refs = req("textDocument/references", 3, 4, context={"includeDeclaration": False})["result"]
assert len(refs) == 2, refs


def rename(line, ch, name):
    r = req("textDocument/rename", line, ch, newName=name)
    if "error" in r:
        return r["error"]["message"]
    return {u: sorted((e["range"]["start"]["line"], e["range"]["start"]["character"], e["newText"]) for e in es)
            for u, es in r["result"]["changes"].items()}


assert rename(3, 4, "assist") == {muri: [(3, 3, "assist"), (12, 28, "assist"), (13, 34, "assist")]}
# a method renamed in both files
assert rename(10, 15, "sum") == {guri: [(9, 7, "sum")], muri: [(10, 14, "sum")]}, rename(10, 15, "sum")
# the shorthand `{ ..., y }` keeps its field
assert rename(11, 8, "yy") == {muri: [(11, 8, "yy"), (12, 39, "y: yy")]}, rename(11, 8, "yy")
# unsafe renames are refused, with a reason
assert "refer to" in rename(10, 8, "y"), rename(10, 8, "y")        # `t` -> `y`: the later `let y` would capture "{t}"
assert rename(4, 8, "n") == {muri: [(4, 8, "n"), (5, 4, "n")]}     # `let n = n + 1` shadows, which is fine
assert "E0" in rename(3, 4, "main"), rename(3, 4, "main")          # a second `main`
assert "uppercase" in rename(3, 10, "N"), rename(3, 10, "N")
assert "no name" in rename(13, 16, "say"), rename(13, 16, "say")  # print is built in
assert "standard library" not in rename(9, 18, "start") and isinstance(rename(9, 18, "start"), dict)
# members of a module after `json.`
send({"jsonrpc": "2.0", "method": "textDocument/didChange", "params": {"textDocument": {"uri": muri, "version": 2},
      "contentChanges": [{"text": main.replace("    let t = p.total()", "    let v = json.\n    let t = p.total()")}]}})
recv()
send({"jsonrpc": "2.0", "id": 300, "method": "textDocument/completion", "params": {"textDocument": {"uri": muri}, "position": {"line": 10, "character": 17}}})
labels = [i["label"] for i in recv()["result"]]
assert "parse" in labels and "Value" in labels and "Num" in labels and "get" not in labels, labels
# definition and rename of a standard library function
send({"jsonrpc": "2.0", "method": "textDocument/didChange", "params": {"textDocument": {"uri": muri, "version": 3},
      "contentChanges": [{"text": main.replace("    let t = p.total()", "    let v = json.parse(\"1\")\n    let t = p.total()")}]}})
assert recv()["params"]["diagnostics"] == []
loc = req("textDocument/definition", 10, 18)["result"]
assert loc["uri"].endswith("/tessera-std/json.tsr") and os.path.exists(loc["uri"][7:]), loc
assert "standard library" in rename(10, 18, "parse2")
send({"jsonrpc": "2.0", "id": 4, "method": "shutdown"})
assert recv()["id"] == 4
send({"jsonrpc": "2.0", "method": "exit"})
assert p.wait(timeout=5) == 0
print("lsp stdio: ok")
