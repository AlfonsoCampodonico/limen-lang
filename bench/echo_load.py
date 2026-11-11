#!/usr/bin/env python3
"""Load test for examples/echo.vela: open N connections and keep them all open,
then echo one message on each and check every reply.
Usage: python3 bench/echo_load.py PORT [N]"""
import asyncio, resource, sys, time

port = int(sys.argv[1])
n = int(sys.argv[2]) if len(sys.argv) > 2 else 10000
soft, hard = resource.getrlimit(resource.RLIMIT_NOFILE)
want = min(hard if hard != resource.RLIM_INFINITY else 65536, 65536)
resource.setrlimit(resource.RLIMIT_NOFILE, (max(soft, want), hard))


async def connect(i):
    for attempt in range(20):
        try:
            return await asyncio.open_connection("127.0.0.1", port)
        except OSError:
            await asyncio.sleep(0.05 * (attempt + 1))
    raise RuntimeError(f"connection {i} failed")


async def main():
    t0 = time.perf_counter()
    conns = []
    for start in range(0, n, 500):   # batches, so a small listen backlog is not overrun
        conns += await asyncio.gather(*(connect(i) for i in range(start, min(n, start + 500))))
    t1 = time.perf_counter()
    print(f"{len(conns)} connections open at once ({t1 - t0:.2f}s)")

    async def echo(i, rw):
        r, w = rw
        msg = f"hello {i}\n".encode()
        w.write(msg)
        await w.drain()
        got = await r.readexactly(len(msg))
        return got == msg

    ok = await asyncio.gather(*(echo(i, rw) for i, rw in enumerate(conns)))
    t2 = time.perf_counter()
    for _, w in conns:
        w.close()
    await asyncio.gather(*(w.wait_closed() for _, w in conns), return_exceptions=True)
    print(f"{sum(ok)}/{n} echoes correct ({t2 - t1:.2f}s with all {n} connections open)")
    sys.exit(0 if sum(ok) == n else 1)


asyncio.run(main())
