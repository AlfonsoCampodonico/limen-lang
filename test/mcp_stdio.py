#!/usr/bin/env python3
"""End-to-end check of `vela mcp` over stdio, as a coding agent would use it.
Usage: python3 test/mcp_stdio.py VELA_EXE"""
import json, subprocess, sys

p = subprocess.Popen([sys.argv[1], "mcp"], stdin=subprocess.PIPE, stdout=subprocess.PIPE)
next_id = [0]


def rpc(method, params=None):
    next_id[0] += 1
    msg = {"jsonrpc": "2.0", "id": next_id[0], "method": method, "params": params or {}}
    p.stdin.write((json.dumps(msg) + "\n").encode())
    p.stdin.flush()
    reply = json.loads(p.stdout.readline())
    assert reply["id"] == next_id[0], reply
    return reply


def call(name, **args):
    r = rpc("tools/call", {"name": name, "arguments": args})["result"]
    return r["isError"], r["content"][0]["text"]


init = rpc("initialize", {"protocolVersion": "2025-06-18", "capabilities": {}, "clientInfo": {"name": "t", "version": "1"}})
assert init["result"]["protocolVersion"] == "2025-06-18", init
p.stdin.write(b'{"jsonrpc":"2.0","method":"notifications/initialized"}\n')
names = [t["name"] for t in rpc("tools/list")["result"]["tools"]]
assert names == ["vela_spec", "vela_check", "vela_run", "vela_test", "vela_format", "vela_caps", "vela_type_at", "vela_references", "vela_rename", "vela_explain"], names

hello = 'fn main(root: Root) {\n    let n = 6 * 7\n    root.stdout.print("{n}")\n}\n'
assert call("vela_check", source=hello) == (False, "ok")
err, diags = call("vela_check", source='fn main(root: Root) {\n    print("x")\n}\n')
assert not err and json.loads(diags)["diagnostics"][0]["code"] == "E0302", diags
assert call("vela_run", source=hello) == (False, "exit 0\nstdout:\n42\n\nstderr:\n")
err, out = call("vela_run", source=hello, native=True)
assert not err and "42" in out, out
# no grants: file access is denied, by the language, not by luck
err, out = call("vela_run", source='fn main(root: Root) {\n    root.stdout.print("{root.fs.read("/etc/passwd").is_err()} {root.net.get("https://example.com/")}")\n}\n')
assert "true Err(Denied" in out, out
err, out = call("vela_run", source="fn main(root: Root) {\n    while true {\n    }\n}\n", timeout_seconds=1)
assert err and out.startswith("timed out after 1 s"), out
err, out = call("vela_test", source='test "a" {\n    assert_eq(1 + 1, 2)\n}\ntest "b" {\n    assert(false)\n}\n')
assert err and "ok    a" in out and "FAIL  b" in out, out
assert call("vela_format", source="fn main(root: Root) { let x=1 }") == (False, "fn main(root: Root) {\n    let x = 1\n}\n")
assert call("vela_caps", source=hello) == (False, "main: Root")
assert call("vela_type_at", source=hello, line=1, character=8) == (False, "let n: Int")
prog = 'fn twice(n: Int) -> Int {\n    n * 2\n}\n\nfn main(root: Root) {\n    let a = twice(1)\n    let b = 2\n    root.stdout.print("{twice(a)} {b}")\n}\n'
err, out = call("vela_references", source=prog, line=0, character=4)
refs = json.loads(out)
assert not err and refs["name"] == "twice", out
assert [(r["line"], r["character"], r["end_character"], r["definition"]) for r in refs["references"]] == [(0, 3, 8, True), (5, 12, 17, False), (7, 24, 29, False)], out
err, out = call("vela_rename", source=prog, line=5, character=13, new_name="double")
assert not err and out == prog.replace("twice", "double"), out
err, out = call("vela_rename", source=prog, line=5, character=8, new_name="b")
assert err and "refer to" in out, out
err, out = call("vela_rename", source='use json\n\nfn main(root: Root) {\n    root.stdout.print("{json.parse("1")}")\n}\n', line=3, character=29, new_name="p")
assert err and "standard library" in out, out
assert call("vela_explain", code="e0302")[1].startswith("E0302: Vela has no ambient I/O")
spec = rpc("resources/read", {"uri": "vela://spec"})["result"]["contents"][0]["text"]
assert spec.startswith("# Vela language specification"), spec[:40]
assert call("vela_spec")[1] == spec
p.stdin.close()
assert p.wait(timeout=5) == 0
print("mcp stdio: ok")
