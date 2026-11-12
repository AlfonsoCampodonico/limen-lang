#!/usr/bin/env python3
"""The same benchmarks in C, Rust, Go and Vela: build each with its usual
release settings, check the four outputs agree, and report the best of N
wall-clock runs. Writes _build/bench/compare.json for the comparison page.
Usage: python3 bench/compare.py [runs]"""
import json, os, platform, resource, subprocess, sys, time

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
BENCH = os.path.join(ROOT, "bench")
VELA = os.path.join(ROOT, "_build", "default", "bin", "main.exe")
OUT = os.path.join(ROOT, "_build", "bench")
RUNS = int(sys.argv[1]) if len(sys.argv) > 1 else 7
NAMES = ["fib", "sieve", "nbody", "pipeline", "mandel", "collatz", "lines"]
C_SOURCE = {"mandel": "mandel_pthreads", "collatz": "collatz_pthreads"}  # the threaded C versions

os.makedirs(OUT, exist_ok=True)
subprocess.run(["dune", "build"], cwd=ROOT, check=True)


def build(lang, name):
    exe = os.path.join(OUT, f"{name}-{lang}")
    if lang == "c":
        src = os.path.join(BENCH, C_SOURCE.get(name, name) + ".c")
        # no fused multiply-add, so C rounds every step as Rust and Vela do
        cmd = ["cc", "-O2", "-ffp-contract=off", "-o", exe, src, "-lm", "-lpthread"]
    elif lang == "rust":
        cmd = ["rustc", "--edition", "2021", "-C", "opt-level=3", "-o", exe, os.path.join(BENCH, "rust", name + ".rs")]
    elif lang == "go":
        cmd = ["go", "build", "-o", exe, os.path.join(BENCH, "go", name + ".go")]
    else:
        cmd = [VELA, "build", os.path.join(BENCH, name + ".vela"), "-o", exe]
    subprocess.run(cmd, check=True, capture_output=True)
    return exe


def cpu():
    r = resource.getrusage(resource.RUSAGE_CHILDREN)
    return r.ru_utime + r.ru_stime


def best(exe):
    """The best wall-clock time, the best CPU time (user + system, which other
    load on the machine disturbs much less), and the output."""
    walls, cpus, out = [], [], None
    for _ in range(RUNS):
        c, t = cpu(), time.perf_counter()
        out = subprocess.run([exe], capture_output=True, text=True, check=True).stdout
        walls.append(time.perf_counter() - t)
        cpus.append(cpu() - c)
    return min(walls), min(cpus), out


def numbers(out):
    """The output's numbers at 12 significant digits: languages print floats
    with different digit counts, and that is not a difference in the answer."""
    vals = []
    for tok in out.split():
        try:
            vals.append(float(f"{float(tok):.12g}"))
        except ValueError:
            vals.append(tok)
    return vals


langs = ["c", "rust", "go", "vela"]
results = {}
print(f"{'benchmark':<10}" + "".join(f"{l:>9}" for l in langs) + "   cpu: " + "".join(f"{l:>7}" for l in langs) + "  output")
for n in NAMES:
    row, cpus, outs = {}, {}, {}
    for l in langs:
        t, c, o = best(build(l, n))
        row[l], cpus[l], outs[l] = t, c, o
    ref = numbers(outs["vela"])
    differ = [l for l in langs if numbers(outs[l]) != ref]
    results[n] = {
        "seconds": row,
        "cpu_seconds": cpus,
        "differ": differ,
        "outputs": {l: outs[l].strip() for l in langs},
        "output": outs["vela"].strip().splitlines()[-1] if outs["vela"].strip() else "",
    }
    note = "same" if not differ else "differs: " + ", ".join(f"{l}={outs[l].strip()[:40]!r}" for l in differ)
    print(f"{n:<10}" + "".join(f"{row[l]:>9.3f}" for l in langs) + "        " + "".join(f"{cpus[l]:>7.2f}" for l in langs) + "  " + note)

info = {
    "machine": f"{platform.machine()} {platform.system()} {platform.release()}, {os.cpu_count()} cores",
    "c": subprocess.run(["cc", "--version"], capture_output=True, text=True).stdout.splitlines()[0],
    "rust": subprocess.run(["rustc", "--version"], capture_output=True, text=True).stdout.strip(),
    "go": subprocess.run(["go", "version"], capture_output=True, text=True).stdout.strip(),
    "runs": RUNS,
    "load": os.getloadavg()[0],
}
with open(os.path.join(OUT, "compare.json"), "w") as f:
    json.dump({"info": info, "results": results}, f, indent=2)
