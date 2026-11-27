// A WASI (preview 1) shim for the compiler built to WebAssembly
// (_build/wasm/limen.wasm, from dev/wasm/build.sh). One file for node
// (dev/wasm/run.mjs) and for the playground's Worker: it touches nothing
// of either, only what createWasi is given.
//
//   const wasi = createWasi({ args, files, stdout, stderr, readStdin, now, random });
//   const { instance } = await WebAssembly.instantiate(module, wasi.imports);
//   const status = wasi.start(instance);
//
// args: the program's argv, its name first. files: the whole file system,
// read-only, as paths relative to its root ("main.lmn", "lib/a.lmn") and
// their bytes; the root is preopened as `/`, so the program reads it with
// `--grant fs=/`. stdout and stderr get each write's bytes as they come
// (a character may be split between two writes). readStdin(max) blocks
// until it has input and returns at most max bytes, or null at the end of
// input (never an empty array). now() is the time since the epoch in
// nanoseconds; random(buf) fills buf with secure random bytes. env, which
// may be left out, is the environment as NAME=value strings (none when
// absent: the playground passes none).
// start returns the exit status; a trap (a stack overflow, say) is thrown
// as the RuntimeError it is.

const SUCCESS = 0, BADF = 8, INVAL = 28, ISDIR = 31, NOENT = 44, NOSYS = 52, NOTDIR = 54, ROFS = 69, SPIPE = 70;
const CHARACTER_DEVICE = 2, DIRECTORY = 3, REGULAR_FILE = 4;
const OFLAGS_CREAT = 1, OFLAGS_DIRECTORY = 2, OFLAGS_TRUNC = 8;
const RIGHTS_FD_WRITE = 1n << 6n;
const ALL_RIGHTS = 0xffffffffffffffffn;
const ROOT_FD = 3;

class Exit {
  constructor(code) { this.code = code; }
}

export function createWasi({ args, files, stdout, stderr, readStdin, now, random, env = [] }) {
  const encoder = new TextEncoder();
  const decoder = new TextDecoder();
  // the file system: files by path, and every directory that holds one
  const tree = new Map();
  const dirs = new Set(['']);
  for (const [name, bytes] of files) {
    const path = normalize(name.replace(/^\/+/, ''));
    if (path === null || path === '') continue;
    tree.set(path, bytes);
    for (let i = path.indexOf('/'); i >= 0; i = path.indexOf('/', i + 1)) dirs.add(path.slice(0, i));
  }
  const argBytes = args.map((a) => encoder.encode(a + '\0'));
  const envBytes = env.map((e) => encoder.encode(e + '\0'));

  // open descriptors: 0-2 the standard streams, 3 the root
  const fds = new Map([
    [0, { kind: 'stdio' }],
    [1, { kind: 'stdio' }],
    [2, { kind: 'stdio' }],
    [ROOT_FD, { kind: 'dir', path: '' }],
  ]);
  let nextFd = ROOT_FD + 1;
  let pending = null;   // stdin bytes read but not yet taken
  let memory = null;
  let lastMono = 0n;

  const view = () => new DataView(memory.buffer);
  const bytes = () => new Uint8Array(memory.buffer);
  const str = (ptr, len) => decoder.decode(bytes().subarray(ptr, ptr + len));
  // the iovecs at ptr as [address, length] pairs
  const iovecs = (ptr, n) => {
    const v = view(), out = [];
    for (let i = 0; i < n; i++) out.push([v.getUint32(ptr + 8 * i, true), v.getUint32(ptr + 8 * i + 4, true)]);
    return out;
  };
  // `path` resolved against the directory `base`, or null if it climbs out
  const resolve = (base, path) => normalize(base === '' ? path : base + '/' + path);
  const kindOf = (path) => (tree.has(path) ? REGULAR_FILE : dirs.has(path) ? DIRECTORY : 0);
  const writeFilestat = (ptr, path) => {
    const v = view(), kind = kindOf(path);
    for (let i = 0; i < 64; i += 8) v.setBigUint64(ptr + i, 0n, true);
    v.setBigUint64(ptr + 8, BigInt(hash(path)), true);
    v.setUint8(ptr + 16, kind);
    v.setBigUint64(ptr + 24, 1n, true);
    v.setBigUint64(ptr + 32, BigInt(kind === REGULAR_FILE ? tree.get(path).length : 0), true);
  };
  const readStdinInto = (max) => {
    if (pending === null || pending.length === 0) {
      const got = readStdin(max);
      if (got === null || got.length === 0) return new Uint8Array(0);
      pending = got;
    }
    const take = pending.subarray(0, max);
    pending = pending.subarray(take.length);
    return take;
  };
  // the entries of directory `dir`, in order: `.`, `..`, then by name
  const entries = (dir) => {
    const names = new Map();
    const prefix = dir === '' ? '' : dir + '/';
    const add = (path, kind) => {
      if (!path.startsWith(prefix)) return;
      const rest = path.slice(prefix.length);
      if (rest !== '' && !rest.includes('/')) names.set(rest, kind);
    };
    for (const p of tree.keys()) add(p, REGULAR_FILE);
    for (const p of dirs) add(p, DIRECTORY);
    return [['.', DIRECTORY], ['..', DIRECTORY], ...[...names].sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0))];
  };

  // the sizes of a list of strings, and the list itself (argv, environ)
  const sizes = (list, count, size) => {
    const v = view();
    v.setUint32(count, list.length, true);
    v.setUint32(size, list.reduce((n, a) => n + a.length, 0), true);
    return SUCCESS;
  };
  const strings = (list, ptrs, buf) => {
    const v = view(), b = bytes();
    for (const [i, a] of list.entries()) {
      v.setUint32(ptrs + 4 * i, buf, true);
      b.set(a, buf);
      buf += a.length;
    }
    return SUCCESS;
  };

  const wasi = {
    args_sizes_get: (count, size) => sizes(argBytes, count, size),
    args_get: (ptrs, buf) => strings(argBytes, ptrs, buf),
    environ_sizes_get: (count, size) => sizes(envBytes, count, size),
    environ_get: (ptrs, buf) => strings(envBytes, ptrs, buf),
    clock_res_get(id, res) {
      view().setBigUint64(res, 1000n, true);
      return SUCCESS;
    },
    clock_time_get(id, precision, time) {
      let t = BigInt(now());
      if (id !== 0) {   // monotonic and the CPU clocks never go back
        if (t < lastMono) t = lastMono;
        lastMono = t;
      }
      view().setBigUint64(time, t, true);
      return SUCCESS;
    },
    random_get(buf, len) {
      random(bytes().subarray(buf, buf + len));
      return SUCCESS;
    },
    fd_write(fd, iovs, n, written) {
      const f = fds.get(fd);
      if (!f) return BADF;
      if (f.kind !== 'stdio' || fd === 0) return f.kind === 'dir' ? ISDIR : BADF;
      const parts = iovecs(iovs, n);
      const total = parts.reduce((k, [, len]) => k + len, 0);
      const out = new Uint8Array(total);
      let at = 0;
      for (const [ptr, len] of parts) {
        out.set(bytes().subarray(ptr, ptr + len), at);
        at += len;
      }
      if (total > 0) (fd === 1 ? stdout : stderr)(out);
      view().setUint32(written, total, true);
      return SUCCESS;
    },
    fd_read(fd, iovs, n, read) {
      const f = fds.get(fd);
      if (!f) return BADF;
      if (f.kind === 'dir') return ISDIR;
      let total = 0;
      for (const [ptr, len] of iovecs(iovs, n)) {
        if (len === 0) continue;
        let chunk;
        if (f.kind === 'stdio') {
          if (fd !== 0) return BADF;
          chunk = readStdinInto(len);
        } else {
          chunk = f.data.subarray(f.pos, f.pos + len);
          f.pos += chunk.length;
        }
        bytes().set(chunk, ptr);
        total += chunk.length;
        if (chunk.length < len || f.kind === 'stdio') break;   // stdin: give what came, do not wait for more
      }
      view().setUint32(read, total, true);
      return SUCCESS;
    },
    fd_pread(fd, iovs, n, offset, read) {
      const f = fds.get(fd);
      if (!f) return BADF;
      if (f.kind !== 'file') return f.kind === 'dir' ? ISDIR : SPIPE;
      let pos = Number(offset), total = 0;
      for (const [ptr, len] of iovecs(iovs, n)) {
        const chunk = f.data.subarray(pos, pos + len);
        bytes().set(chunk, ptr);
        pos += chunk.length;
        total += chunk.length;
        if (chunk.length < len) break;
      }
      view().setUint32(read, total, true);
      return SUCCESS;
    },
    fd_seek(fd, offset, whence, newOffset) {
      const f = fds.get(fd);
      if (!f) return BADF;
      if (f.kind !== 'file') return SPIPE;
      const base = whence === 0 ? 0n : whence === 1 ? BigInt(f.pos) : BigInt(f.data.length);
      const to = base + BigInt(offset);
      if (to < 0n) return INVAL;
      f.pos = Number(to);
      view().setBigUint64(newOffset, to, true);
      return SUCCESS;
    },
    fd_tell(fd, offset) {
      const f = fds.get(fd);
      if (!f) return BADF;
      if (f.kind !== 'file') return SPIPE;
      view().setBigUint64(offset, BigInt(f.pos), true);
      return SUCCESS;
    },
    fd_close(fd) {
      if (!fds.has(fd)) return BADF;
      if (fd > ROOT_FD) fds.delete(fd);
      return SUCCESS;
    },
    fd_fdstat_get(fd, stat) {
      const f = fds.get(fd);
      if (!f) return BADF;
      const v = view();
      // the standard streams are character devices that can seek, so
      // isatty says no, as for a pipe or a file
      v.setUint8(stat, f.kind === 'stdio' ? CHARACTER_DEVICE : f.kind === 'dir' ? DIRECTORY : REGULAR_FILE);
      v.setUint16(stat + 2, 0, true);
      v.setBigUint64(stat + 8, ALL_RIGHTS, true);
      v.setBigUint64(stat + 16, ALL_RIGHTS, true);
      return SUCCESS;
    },
    fd_fdstat_set_flags(fd) { return fds.has(fd) ? SUCCESS : BADF; },
    fd_filestat_get(fd, stat) {
      const f = fds.get(fd);
      if (!f) return BADF;
      if (f.kind === 'stdio') {
        const v = view();
        for (let i = 0; i < 64; i += 8) v.setBigUint64(stat + i, 0n, true);
        v.setUint8(stat + 16, CHARACTER_DEVICE);
        return SUCCESS;
      }
      writeFilestat(stat, f.path);
      return SUCCESS;
    },
    fd_prestat_get(fd, prestat) {
      if (fd !== ROOT_FD) return BADF;
      const v = view();
      v.setUint8(prestat, 0);
      v.setUint32(prestat + 4, 1, true);
      return SUCCESS;
    },
    fd_prestat_dir_name(fd, path, len) {
      if (fd !== ROOT_FD) return BADF;
      if (len < 1) return INVAL;
      bytes()[path] = 0x2f;   // "/"
      return SUCCESS;
    },
    fd_readdir(fd, buf, len, cookie, used) {
      const f = fds.get(fd);
      if (!f) return BADF;
      if (f.kind !== 'dir') return NOTDIR;
      const list = entries(f.path);
      const out = new Uint8Array(len);
      let at = 0;
      for (let i = Number(cookie); i < list.length && at < len; i++) {
        const [name, kind] = list[i];
        const nameBytes = encoder.encode(name);
        const entry = new Uint8Array(24 + nameBytes.length), ev = new DataView(entry.buffer);
        ev.setBigUint64(0, BigInt(i + 1), true);
        ev.setBigUint64(8, BigInt(hash(resolve(f.path, name) ?? '')), true);
        ev.setUint32(16, nameBytes.length, true);
        ev.setUint8(20, kind);
        entry.set(nameBytes, 24);
        const fit = Math.min(entry.length, len - at);
        out.set(entry.subarray(0, fit), at);
        at += fit;
      }
      bytes().set(out.subarray(0, at), buf);
      view().setUint32(used, at, true);
      return SUCCESS;
    },
    path_open(dirfd, dirflags, pathPtr, pathLen, oflags, rightsBase, rightsInheriting, fdflags, opened) {
      const d = fds.get(dirfd);
      if (!d) return BADF;
      if (d.kind !== 'dir') return NOTDIR;
      const path = resolve(d.path, str(pathPtr, pathLen));
      if (path === null) return NOENT;
      const kind = kindOf(path);
      if (kind === 0) return oflags & OFLAGS_CREAT ? ROFS : NOENT;
      if (oflags & (OFLAGS_CREAT | OFLAGS_TRUNC)) return ROFS;
      if (kind === DIRECTORY) {
        if (BigInt(rightsBase) & RIGHTS_FD_WRITE) return ISDIR;
        fds.set(nextFd, { kind: 'dir', path });
      } else {
        if (oflags & OFLAGS_DIRECTORY) return NOTDIR;
        if (BigInt(rightsBase) & RIGHTS_FD_WRITE) return ROFS;
        fds.set(nextFd, { kind: 'file', path, data: tree.get(path), pos: 0 });
      }
      view().setUint32(opened, nextFd, true);
      nextFd++;
      return SUCCESS;
    },
    path_filestat_get(dirfd, flags, pathPtr, pathLen, stat) {
      const d = fds.get(dirfd);
      if (!d) return BADF;
      if (d.kind !== 'dir') return NOTDIR;
      const path = resolve(d.path, str(pathPtr, pathLen));
      if (path === null || kindOf(path) === 0) return NOENT;
      writeFilestat(stat, path);
      return SUCCESS;
    },
    path_readlink(dirfd, pathPtr, pathLen) {
      const d = fds.get(dirfd);
      if (!d) return BADF;
      const path = resolve(d.path, str(pathPtr, pathLen));
      return path === null || kindOf(path) === 0 ? NOENT : INVAL;   // nothing is a link
    },
    // sleeping (nanosleep): a clock subscription blocks until its time;
    // anything else is ready at once
    poll_oneoff(inPtr, outPtr, n, nevents) {
      const v = view();
      let wait = null;
      for (let i = 0; i < n; i++) {
        const s = inPtr + 48 * i;
        if (v.getUint8(s + 8) !== 0) { wait = 0n; continue; }
        const timeout = v.getBigUint64(s + 24, true);
        const ns = v.getUint16(s + 40, true) & 1 ? timeout - BigInt(now()) : timeout;
        if (wait === null || ns < wait) wait = ns;
      }
      if (wait !== null && wait > 0n) sleep(Number(wait / 1000000n));
      const o = view();
      for (let i = 0; i < n; i++) {
        const s = inPtr + 48 * i, e = outPtr + 32 * i;
        for (let k = 0; k < 32; k += 8) o.setBigUint64(e + k, 0n, true);
        o.setBigUint64(e, o.getBigUint64(s, true), true);
        o.setUint8(e + 10, o.getUint8(s + 8));
      }
      o.setUint32(nevents, n, true);
      return SUCCESS;
    },
    proc_exit(code) { throw new Exit(code); },
    sched_yield() { return SUCCESS; },
    fd_sync() { return SUCCESS; },
    fd_datasync() { return SUCCESS; },
    fd_advise() { return SUCCESS; },
    path_create_directory() { return ROFS; },
    path_remove_directory() { return ROFS; },
    path_unlink_file() { return ROFS; },
    path_rename() { return ROFS; },
    path_symlink() { return ROFS; },
    path_link() { return ROFS; },
    path_filestat_set_times() { return ROFS; },
    fd_filestat_set_size() { return ROFS; },
    fd_filestat_set_times() { return ROFS; },
    fd_allocate() { return ROFS; },
  };
  // whatever else the module imports is not here
  const preview1 = new Proxy(wasi, { get: (t, name) => (name in t ? t[name] : () => NOSYS) });

  return {
    imports: { wasi_snapshot_preview1: preview1 },
    start(instance) {
      memory = instance.exports.memory;
      try {
        instance.exports._start();
        return 0;
      } catch (e) {
        if (e instanceof Exit) return e.code;
        throw e;
      }
    },
  };
}

// names separated by single slashes, `.` dropped and `..` taken back; null
// when it climbs out of the root
function normalize(path) {
  const out = [];
  for (const name of path.split('/')) {
    if (name === '' || name === '.') continue;
    if (name === '..') {
      if (out.length === 0) return null;
      out.pop();
    } else out.push(name);
  }
  return out.join('/');
}

// an inode number for a path
function hash(path) {
  let h = 2166136261;
  for (let i = 0; i < path.length; i++) h = Math.imul(h ^ path.charCodeAt(i), 16777619) >>> 0;
  return h + 1;
}

// block for ms milliseconds (node, or a Worker: Atomics.wait may block there)
function sleep(ms) {
  if (ms <= 0) return;
  Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, ms);
}
