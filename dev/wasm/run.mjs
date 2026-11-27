// Run the compiler built to WebAssembly (_build/wasm/limen.wasm) in node,
// through the WASI shim the playground uses (dev/wasm/wasi.mjs):
//   node dev/wasm/run.mjs [--stdin-file F] [--root DIR] -- ARGS...
// as `limen ARGS...`. The current directory is the compiler's whole file
// system, read-only, granted to it as `/` (so `check main.lmn` reads
// ./main.lmn); with --root, DIR is (it must hold the current directory,
// which the compiler then works in, through PWD). stdin is F, or this
// process's stdin. Exits with the program's status; a trap, or the
// engine's stack running out, prints `trap: MESSAGE` and exits with 134.
//
//   node dev/wasm/run.mjs --batch JOBS
// runs many, compiling the module once (dev/wasm/check.sh): each line of
// JOBS is ROOT, DIR, OUT and the arguments, separated by tabs; stdout goes
// to OUT, stderr to OUT.err, and `exit N` is added to OUT.
import { appendFileSync, closeSync, openSync, readFileSync, readdirSync, readSync, statSync, writeSync } from 'node:fs';
import { join, dirname, relative, resolve, sep } from 'node:path';
import { fileURLToPath } from 'node:url';
import { randomFillSync } from 'node:crypto';
import { createWasi } from './wasi.mjs';

const usage = 'usage: node dev/wasm/run.mjs [--stdin-file F] [--root DIR] -- ARGS...\n       node dev/wasm/run.mjs --batch JOBS';
const here = dirname(fileURLToPath(import.meta.url));
const module = new WebAssembly.Module(readFileSync(join(here, '../../_build/wasm/limen.wasm')));

// DIR's files by path relative to it, without what a build or a checkout
// keeps beside the sources
const skip = new Set(['.git', '_build', 'node_modules', '_opam', 'target']);
function walk(dir, rel = '', files = new Map()) {
  for (const e of readdirSync(dir, { withFileTypes: true })) {
    if (skip.has(e.name)) continue;
    const path = join(dir, e.name), name = rel ? rel + '/' + e.name : e.name;
    let isDir = e.isDirectory(), isFile = e.isFile();
    if (e.isSymbolicLink()) {
      try {
        const st = statSync(path);
        isDir = st.isDirectory();
        isFile = st.isFile();
      } catch {
        continue;
      }
    }
    if (isDir) walk(path, name, files);
    else if (isFile) files.set(name, readFileSync(path));
  }
  return files;
}

// a pipe node left non-blocking can refuse a write: try again
function writeAll(fd, b) {
  let at = 0;
  while (at < b.length) {
    try {
      at += writeSync(fd, b, at, b.length - at);
    } catch (e) {
      if (e.code !== 'EAGAIN') throw e;
    }
  }
}

// run the compiler with args in dir, its file system root's files; the
// exit status
function run({ root, files, dir, args, stdinFd, outFd, errFd }) {
  const rel = relative(root, dir);
  if (rel.startsWith('..') || resolve(root, rel) !== resolve(dir)) throw new Error(`${dir} is not inside ${root}`);
  const pwd = rel === '' ? [] : ['--grant', 'env=PWD'];
  const wasi = createWasi({
    args: ['limen', '--grant', 'fs=/', ...pwd, '--', ...args],
    env: rel === '' ? [] : ['PWD=/' + rel.split(sep).join('/')],
    files,
    stdout: (b) => writeAll(outFd, b),
    stderr: (b) => writeAll(errFd, b),
    readStdin: (max) => {
      const buf = Buffer.alloc(max);
      for (;;) {
        try {
          const n = readSync(stdinFd, buf, 0, max, null);
          return n === 0 ? null : buf.subarray(0, n);
        } catch (e) {
          if (e.code === 'EAGAIN') continue;
          if (e.code === 'EOF') return null;
          throw e;
        }
      }
    },
    now: () => BigInt(Date.now()) * 1000000n,
    random: (buf) => randomFillSync(buf),
  });
  const instance = new WebAssembly.Instance(module, wasi.imports);
  try {
    return wasi.start(instance);
  } catch (e) {
    // a wasm trap, or V8's own stack (which the wasm frames use too) ran out
    if (!(e instanceof WebAssembly.RuntimeError) && !(e instanceof RangeError)) throw e;
    writeAll(errFd, Buffer.from(`trap: ${e.message}\n`));
    return 134;
  }
}

const argv = process.argv.slice(2);
if (argv[0] === '--batch' && argv.length === 2) {
  const roots = new Map();
  for (const line of readFileSync(argv[1], 'utf8').split('\n')) {
    if (line === '') continue;
    const [root, dir, out, ...args] = line.split('\t');
    if (!roots.has(root)) roots.set(root, walk(root));
    const outFd = openSync(out, 'w'), errFd = openSync(out + '.err', 'w'), stdinFd = openSync('/dev/null', 'r');
    const status = run({ root, files: roots.get(root), dir, args, stdinFd, outFd, errFd });
    [outFd, errFd, stdinFd].forEach(closeSync);
    appendFileSync(out, `exit ${status}\n`);
  }
} else {
  let stdinFile = null, root = process.cwd();
  while (argv.length > 0 && argv[0] !== '--') {
    if (argv[0] === '--stdin-file' && argv.length > 1) stdinFile = argv[1];
    else if (argv[0] === '--root' && argv.length > 1) root = resolve(argv[1]);
    else {
      console.error(usage);
      process.exit(2);
    }
    argv.splice(0, 2);
  }
  if (argv[0] !== '--') {
    console.error(usage);
    process.exit(2);
  }
  const stdinFd = stdinFile === null ? 0 : openSync(stdinFile, 'r');
  process.exitCode = run({ root, files: walk(root), dir: process.cwd(), args: argv.slice(1), stdinFd, outFd: 1, errFd: 2 });
}
