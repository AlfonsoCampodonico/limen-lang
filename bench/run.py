#!/usr/bin/env python3
"""Build each benchmark with writ and with cc -O2, check outputs match, and
report the best of N wall-clock runs.  Usage: python3 bench/run.py [runs]"""
import os, subprocess, sys, time

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
WRIT = os.path.join(ROOT, "_build", "default", "bin", "main.exe")
OUT = os.path.join(ROOT, "_build", "bench")
RUNS = int(sys.argv[1]) if len(sys.argv) > 1 else 3
SEQUENTIAL = ["fib", "sieve", "nbody", "nbody_array", "pipeline"]
C_SOURCE = {"nbody_array": "nbody"}  # same C program
PARALLEL = ["mandel", "collatz"]
CFLAGS = ["-O2", "-fwrapv", "-ffp-contract=off"]

os.makedirs(OUT, exist_ok=True)
subprocess.run(["dune", "build"], cwd=ROOT, check=True)


def best(cmd, env=None):
    times, out = [], None
    for _ in range(RUNS):
        t = time.perf_counter()
        out = subprocess.run(cmd, capture_output=True, text=True, check=True, env=env).stdout
        times.append(time.perf_counter() - t)
    return min(times), out


def build_c(name):
    exe = os.path.join(OUT, name + "-c")
    subprocess.run(["cc", *CFLAGS, "-o", exe, os.path.join(ROOT, "bench", name + ".c"), "-lm", "-lpthread"], check=True)
    return exe


def build_writ(name):
    exe = os.path.join(OUT, name + "-writ")
    subprocess.run([WRIT, "build", os.path.join(ROOT, "bench", name + ".wr"), "-o", exe], check=True)
    return exe


print("single-threaded workloads")
print(f"{'benchmark':<10} {'C (s)':>8} {'Writ (s)':>9} {'Writ/C':>7}  same output")
for n in SEQUENTIAL:
    tc, oc = best([build_c(C_SOURCE.get(n, n))])
    tw, ow = best([build_writ(n)])
    print(f"{n:<10} {tc:>8.3f} {tw:>9.3f} {tw / tc:>6.2f}x  {'yes' if oc == ow else 'NO'}")

one = dict(os.environ, WRIT_THREADS="1")
print("\nparallel workloads (group/go)")
print(f"{'benchmark':<10} {'C seq':>8} {'C pthreads':>11} {'Writ 1 thr':>11} {'Writ all':>9} {'Writ/C seq':>11} {'Writ/C thr':>11}  same output")
for n in PARALLEL:
    ts, os_ = best([build_c(n)])
    tp, op = best([build_c(n + "_pthreads")])
    w = build_writ(n)
    t1, o1 = best([w], env=one)
    ta, oa = best([w])
    same = os_ == op == o1 == oa
    print(f"{n:<10} {ts:>8.3f} {tp:>11.3f} {t1:>11.3f} {ta:>9.3f} {ta / ts:>10.2f}x {ta / tp:>10.2f}x  {'yes' if same else 'NO ' + repr((os_, op, o1, oa))}")
