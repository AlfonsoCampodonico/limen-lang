// The WASI shim on its own, with a fake instance whose _start calls the
// imports as a wasm program would.
//   node --test dev/wasm/*.test.mjs
import { test } from 'node:test';
import assert from 'node:assert/strict';
import { createWasi } from './wasi.mjs';

function options(over = {}) {
  return {
    args: ['limen'],
    files: new Map(),
    stdout: () => {},
    stderr: () => {},
    readStdin: () => null,
    now: () => 0n,
    random: (buf) => buf.fill(4),
    ...over,
  };
}

// an instance whose _start runs body(wasi functions, memory view)
function fake(wasi, body) {
  const memory = new WebAssembly.Memory({ initial: 1 });
  const fns = wasi.imports.wasi_snapshot_preview1;
  return { exports: { memory, _start: () => body(fns, new DataView(memory.buffer), new Uint8Array(memory.buffer)) } };
}

test('stdout bytes split across fd_write calls arrive intact', () => {
  const chunks = [];
  const wasi = createWasi(options({ stdout: (b) => chunks.push(Uint8Array.from(b)) }));
  const status = wasi.start(fake(wasi, (f, view, bytes) => {
    for (const [i, byte] of [0xc3, 0xa9].entries()) {
      bytes[100 + i] = byte;
      view.setUint32(0, 100 + i, true); // iovec: buf
      view.setUint32(4, 1, true);       // iovec: len
      assert.equal(f.fd_write(1, 0, 1, 16), 0);
      assert.equal(view.getUint32(16, true), 1);
    }
  }));
  assert.equal(status, 0);
  assert.deepEqual(Buffer.concat(chunks), Buffer.from('é'));
});

test('readStdin returning null makes fd_read return 0 bytes', () => {
  const wasi = createWasi(options({ readStdin: () => null }));
  let got = -1;
  wasi.start(fake(wasi, (f, view) => {
    view.setUint32(0, 100, true);
    view.setUint32(4, 32, true);
    assert.equal(f.fd_read(0, 0, 1, 16), 0);
    got = view.getUint32(16, true);
  }));
  assert.equal(got, 0);
});

test('stdin bytes beyond the buffer stay for the next read', () => {
  const input = [Uint8Array.from([1, 2, 3])];
  const wasi = createWasi(options({ readStdin: () => input.shift() ?? null }));
  const seen = [];
  wasi.start(fake(wasi, (f, view, bytes) => {
    for (let i = 0; i < 3; i++) {
      view.setUint32(0, 100, true);
      view.setUint32(4, 2, true);
      f.fd_read(0, 0, 1, 16);
      seen.push(Array.from(bytes.subarray(100, 100 + view.getUint32(16, true))));
    }
  }));
  assert.deepEqual(seen, [[1, 2], [3], []]);
});

test('proc_exit ends the run with its status', () => {
  const wasi = createWasi(options());
  assert.equal(wasi.start(fake(wasi, (f) => { f.proc_exit(3); throw new Error('not reached'); })), 3);
});

test('a file in the map opens, reads and reaches end of file', () => {
  const wasi = createWasi(options({ files: new Map([['dir/a.txt', new TextEncoder().encode('hi')]]) }));
  wasi.start(fake(wasi, (f, view, bytes) => {
    // fd 3 is the preopened `/`
    assert.equal(f.fd_prestat_get(3, 0), 0);
    assert.equal(view.getUint32(4, true), 1);
    bytes.set(new TextEncoder().encode('dir/a.txt'), 200);
    assert.equal(f.path_open(3, 1, 200, 9, 0, 0n, 0n, 0, 32), 0);
    const fd = view.getUint32(32, true);
    view.setUint32(0, 100, true);
    view.setUint32(4, 16, true);
    assert.equal(f.fd_read(fd, 0, 1, 16), 0);
    assert.equal(new TextDecoder().decode(bytes.subarray(100, 100 + view.getUint32(16, true))), 'hi');
    assert.equal(f.fd_read(fd, 0, 1, 16), 0);
    assert.equal(view.getUint32(16, true), 0);
    bytes.set(new TextEncoder().encode('dir/b.txt'), 200);
    assert.equal(f.path_open(3, 1, 200, 9, 0, 0n, 0n, 0, 32), 44); // ENOENT
  }));
});
