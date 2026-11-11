#!/usr/bin/env python3
"""Grade solutions for the agent benchmark.
Usage: python3 bench/agent/grade.py SOLUTIONS_DIR [--native]
SOLUTIONS_DIR holds NN-name.wr files. Each runs from a fresh copy of its
task directory, with that task's grants, and must print exactly the
expected output."""
import os, shutil, subprocess, sys, tempfile

ROOT = os.path.dirname(os.path.abspath(__file__))
WRIT = os.path.join(ROOT, "..", "..", "_build", "default", "bin", "main.exe")
sol_dir = sys.argv[1]
native = "--native" in sys.argv[2:]
tasks = sorted(os.listdir(os.path.join(ROOT, "tasks")))
passed, rows = 0, []
for t in tasks:
    sol = os.path.join(sol_dir, t + ".wr")
    expected = open(os.path.join(ROOT, "expected", t + ".txt")).read()
    if not os.path.exists(sol):
        rows.append((t, "missing", ""))
        continue
    src = open(sol).read()
    work = tempfile.mkdtemp(prefix="writ-grade-")
    tdir = os.path.join(work, t)
    shutil.copytree(os.path.join(ROOT, "tasks", t), tdir)
    shutil.copy(sol, os.path.join(tdir, "solution.wr"))
    grants = [g for g in open(os.path.join(tdir, "grants")).read().split("\n") if g]
    cmd = [os.path.abspath(WRIT), "run"] + (["--native"] if native else []) + ["solution.wr"]
    for g in grants:
        cmd += ["--grant", g]
    try:
        r = subprocess.run(cmd, cwd=tdir, capture_output=True, text=True, timeout=60)
        ok = r.returncode == 0 and r.stdout == expected
        note = "" if ok else (r.stderr.strip().split("\n")[0] if r.stderr.strip() else "wrong output")[:70]
    except subprocess.TimeoutExpired:
        ok, note = False, "timeout"
    # a solution that contains its whole expected output is suspicious
    if ok and len(expected.strip()) > 12 and expected.strip() in src:
        note = "hard-coded output?"
    shutil.rmtree(work, ignore_errors=True)
    passed += ok
    rows.append((t, "pass" if ok else "FAIL", note))
for t, s, n in rows:
    print(f"{t:28} {s:7} {n}")
print(f"\n{passed}/{len(tasks)} passed")
