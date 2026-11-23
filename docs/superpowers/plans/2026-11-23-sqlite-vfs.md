# SQLite files opened from the grant Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Every file SQLite opens for `sql.open(fs, name)` is opened with `openat(..., O_NOFOLLOW)` from a directory descriptor held inside the grant, through a Limen VFS, with WAL working as today.

**Architecture:** A new runtime helper, `limen_sql_open(root, name, flags)`, walks from the grant to the database's directory with the sandbox's existing walk (`w_fs_entry`), refuses symlinked side files, keeps a dup of the directory's descriptor in a slot table and opens the database through a VFS named `limen` under a filename that names the slot. The VFS (in `src/runtime/limen_sql.h`, shared by both backends) does its own opening, I/O, POSIX locking with a per-inode table and WAL shared memory, and delegates the rest to the default VFS. `sql.memory()` keeps the default VFS.

**Tech Stack:** C (runtime, no `sqlite3.h`), OCaml (`src/`), Limen (`selfhost/`, `src/std/sql.lmn`), SQLite's C API through the `w_sql_api` table.

**Spec:** `docs/superpowers/specs/2026-11-23-sqlite-vfs-design.md`

## Global Constraints

- Every change goes into both compilers (`src/` and `selfhost/`) and both backends (interpreter and C), and stays at the bootstrap fixed point.
- Before any commit, all four pass: `dune build`, `dune exec ./test/test_limen.exe`, `sh selfhost/check.sh`, `sh selfhost/bootstrap.sh`.
- Commit messages: no `Co-Authored-By`, no "Generated with", no Claude attribution of any kind. No emojis. British spelling, no em dashes, in messages, prose and comments.
- Comments and docs in the repo's plain style (see `src/runtime/limen_sql.h:1-44`, `src/std/sql.lmn:1-20`).
- RTK may rewrite shell commands; `rtk proxy <cmd>` runs one raw.
- SQLite's functions reach C only through the `w_sql_api` table (`W_SQL_*` enum, `Ffi.sql_syms`, `ffi.sql_syms()`), in one order everywhere. No `sqlite3.h`; the VFS structs are declared by hand: `sqlite3_vfs` version 3, `sqlite3_io_methods` version 3.
- SQLite 3.37 or later (the code already calls `sqlite3_changes64`, added in 3.37.0).
- macOS and Linux, x86-64 and arm64; page sizes up to 64 KB (arm64 Linux kernels), so `-shm` regions are mapped in page-sized groups.
- No new error codes or messages a program can see. The one visible change: a `-journal`, `-wal` or `-shm` that is a symlink is refused even when it points inside the grant, with the same `Denied` message as today.
- `sql.memory()` is unchanged (default VFS). Opening is not `blocking`: library `limen` helpers never are (`src/ffi.ml:375`); it runs on the calling task's thread, as an Fs operation does.
- SPEC.md is updated where it describes this behaviour. The book (`~/Github/Personal/limen-book`) does not describe side files or links (ch14 line 132 says only that the database stays in the grant), so it needs no change; never push it.

## Review Focus

- **A connection closing while another in the same process holds a lock.** Closing any descriptor drops every POSIX lock the process holds on that file, so a naive `xClose` silently releases the other connection's RESERVED lock to other processes. Expected: the lock survives. Test: `sql-locks-survive-close` (Task 1).
- **WAL index past its first 32 KB region, on any page size.** `mmap` of a region at a 32 KB offset fails with `EINVAL` on 64 KB-page kernels. Expected: a second region maps and a second connection reads every row. Test: `sql-wal` inserts 4,200 frames with checkpoints off (Task 1).
- **Read-only paths.** A database file the process cannot write (falls back to read-only), a read-only Fs on a WAL database while it is open, and a read-only Fs on a WAL database nobody has open. Expected: reads work, writes say "attempt to write a readonly database", as today. Tests: `sql-readonly-file`, `sql-wal` (Task 1).
- **The database's own name is a symlink.** Today it is refused (SQLITE_OPEN_NOFOLLOW). Expected: still refused, same message. Test: `sql-db-link` (Task 1).
- **Connections opened and closed from many pool threads at once.** The slot and inode tables are shared. Expected: no data race. Test: `check_tsan "sql-wal"` in its own directory (Task 1).

---

### Task 1: Pin today's file behaviour

Tests that pass on the current code and must still pass after Tasks 2 and 3. They are characterisation tests: if a first run prints something other than the expected string below, the current code's output is the expectation (record it, and say so in the commit message), because the goal is that nothing changes.

**Files:**
- Modify: `test/ffi_test.c` (append `vt_lock_held`)
- Modify: `test/test_limen.ml` (new tests after `sql-no-fs`, line 4085-4087)

**Interfaces:**
- Produces: `int32_t vt_lock_held(const char *path, int64_t offset)` in liblimentest: 1 if another process holds a lock on byte `offset` of `path`, 0 if not, -1 on error.

- [ ] **Step 1: Add the lock probe to `test/ffi_test.c`**

A process never sees its own POSIX locks, so the probe forks and asks from the child:

```c
/* For the sql tests: whether a lock is held on byte `offset` of `path`, as
   another process sees it (a process never sees its own): 1, 0, or -1. */
int32_t vt_lock_held(const char *path, int64_t offset) {
  pid_t pid = fork();
  if (pid == 0) {
    int fd = open(path, O_RDONLY);
    struct flock fl;
    memset(&fl, 0, sizeof fl);
    fl.l_type = F_WRLCK; fl.l_whence = SEEK_SET; fl.l_start = (off_t)offset; fl.l_len = 1;
    if (fd < 0 || fcntl(fd, F_GETLK, &fl) != 0) _exit(2);
    _exit(fl.l_type == F_UNLCK ? 0 : 1);
  }
  int st;
  if (pid < 0 || waitpid(pid, &st, 0) != pid || !WIFEXITED(st) || WEXITSTATUS(st) == 2) return -1;
  return WEXITSTATUS(st);
}
```

(includes: `<fcntl.h>`, `<sys/wait.h>`, `<unistd.h>`.)

- [ ] **Step 2: Write the tests in `test/test_limen.ml`, after `sql-no-fs`**

All use `sqlreal` (line 3972) as the grant. Each program uses file names no other test uses, since native runs overlap later interpreter tests.

`sql-locks-survive-close`, interpreter only (the probe forks; the native macOS sandbox denies `process-fork` without Exec), wrapped in `if selected "sql-locks-survive-close"` and checked with `run` and `ok`, grants `{ sqlg with fs = Some (sqlreal, false); foreign = [ "limentest" ] }`:

```limen
use sql

foreign "limentest" {
    fn vt_lock_held(path: Str, offset: Int) -> Int32
}

fn main(root: Root) -> Result[(), IoError] {
    let fs = root.fs
    let f = root.foreign
    fs.remove("held.db")
    let a = sql.open(fs, "held.db")?
    a.exec("CREATE TABLE t (x)")?
    let path = fs.full_path("held.db")?
    // SQLite's RESERVED byte, 0x40000001
    let reserved = 1073741825
    a.exec("BEGIN IMMEDIATE")?
    let before = vt_lock_held(f, path, reserved)
    let b = sql.open(fs, "held.db")?
    b.query("SELECT COUNT(*) AS n FROM t")?
    b.close()?
    let after = vt_lock_held(f, path, reserved)
    a.exec("COMMIT")?
    root.stdout.print("{before} {after} {vt_lock_held(f, path, reserved)}")
    a.close()?
    fs.remove("held.db")
    Ok(())
}
```

Expected output: `"1 1 0\n"`.

`sql-wal`, with `out` and `~grants:{ sqlg with fs = Some (sqlreal, false) }`, plus `check_tsan "sql-wal"` with the same source and want but a grant of its own directory (`limen-sql-tsan-<pid>` under the temp dir, made and realpath'd as `sqlreal` is), so the two native runs never share files:

```limen
use sql

fn count(db: sql.Db) -> Int {
    match db.query("SELECT COUNT(*) AS n FROM t") {
        Ok(rows) => rows[0].int("n") ?? -1,
        Err(_) => -1,
    }
}

fn main(root: Root) -> Result[(), IoError] {
    let out = root.stdout
    let fs = root.fs
    for f in ["wal.db", "wal.db-wal", "wal.db-shm"] {
        fs.remove(f)
    }
    let a = sql.open(fs, "wal.db")?
    a.exec("PRAGMA journal_mode = WAL")?
    a.exec("PRAGMA wal_autocheckpoint = 0")?
    a.exec("CREATE TABLE t (x)")?
    a.exec("CREATE TABLE big (b BLOB)")?
    // more frames than the -shm's first 32 KB region indexes (4062)
    a.exec("INSERT INTO big SELECT zeroblob(3000) FROM (WITH RECURSIVE c(x) AS (SELECT 1 UNION ALL SELECT x + 1 FROM c WHERE x < 4200) SELECT x FROM c)")?
    a.exec("INSERT INTO t VALUES (1)")?
    let mode = a.query("PRAGMA journal_mode")?[0].text("journal_mode") ?? "?"
    let b = sql.open(fs, "wal.db")?
    let blobs = b.query("SELECT COUNT(*) AS n FROM big")?[0].int("n") ?? -1
    // a reads from a snapshot while b writes: rollback mode would make b wait out the busy timeout
    a.exec("BEGIN")?
    let before = count(a)
    let wrote = b.exec("INSERT INTO t VALUES (?)", [sql.Int(2)])
    let during = count(a)
    a.exec("COMMIT")?
    let after = count(a)
    let ro = sql.open(fs.read_only(), "wal.db")?
    out.print("{mode} {fs.exists("wal.db-wal")} {fs.exists("wal.db-shm")} {blobs} {before} {wrote} {during} {after} {count(ro)}")
    ro.close()?
    b.close()?
    a.close()?
    out.print("{fs.exists("wal.db-wal")} {fs.exists("wal.db-shm")}")
    let cold = sql.open(fs.read_only(), "wal.db")?
    out.print("{count(cold)}")
    cold.close()?
    for f in ["wal.db", "wal.db-wal", "wal.db-shm"] {
        fs.remove(f)
    }
    Ok(())
}
```

Expected: `"wal true true 4200 1 Ok(1) 1 2 2\nfalse false\n2\n"`.

`sql-db-link`, with `out`: before it, OCaml replaces `sqlreal/link.db` with `Unix.symlink "real.db"` (no `real.db` needed). Program: `root.stdout.print("{sql.open(root.fs, "link.db")}")`. Expected: `sprintf "Err(Failed(\"sql: cannot open `%s/link.db`: unable to open database file\"))\n" sqlreal`.

`sql-readonly-file`, with `out`, only when `Unix.geteuid () <> 0` (root writes anyway): OCaml makes an empty `sqlreal/ro-file.db` with mode `0o444` (chmod back and unlink a leftover first). The program does not remove it (the native run comes later):

```limen
use sql

fn main(root: Root) -> Result[(), IoError] {
    let db = sql.open(root.fs, "ro-file.db")?
    root.stdout.print("{db.query("SELECT 1 AS one")?[0].int("one")} {db.exec("CREATE TABLE t (x)")}")
    db.close()
}
```

Expected: `"Some(1) Err(Failed(\"sql: exec: attempt to write a readonly database\"))\n"`.

- [ ] **Step 3: Run them on the current code**

Run: `LIMEN_NATIVE_ONLY=sql- dune exec ./test/test_limen.exe 2>&1 | grep -E 'sql-(locks|wal|db-link|readonly)'`
Expected: no FAIL lines for these names (they pin today's behaviour; see the note at the top of this task if one differs).

- [ ] **Step 4: Run the four checks**

Run: `dune build && dune exec ./test/test_limen.exe && sh selfhost/check.sh && sh selfhost/bootstrap.sh`
Expected: all pass.

- [ ] **Step 5: Commit**

```bash
git add test/ffi_test.c test/test_limen.ml
git commit -m "test: pin what sql does with files before it opens them itself"
```

---

### Task 2: Open through a runtime helper that walks from the grant

Moves opening into C, in both compilers and both backends, still through the default VFS by path. The side files are checked in C, and a link inside the grant is now refused too.

**Files:**
- Modify: `src/runtime/limen_sql.h:51-56` (enum), append `w_sql_open_core` after line 244
- Modify: `src/runtime/limen_rt.h:1486-1492`, `src/runtime/limen_rt.c:3706-3729` (add `w_sql_open` after `w_sql_run`)
- Modify: `src/ffi_stubs.c` (new stub after line 1007)
- Modify: `src/ffi.ml:190-226` (doc comment, `runtime_fns`, `sql_syms`)
- Modify: `selfhost/ffi.lmn:226-247` (`runtime_fns`, `sql_syms`)
- Modify: `src/interp.ml:1481-1484` (external), `1628-1647` (helper)
- Modify: `src/cgen.ml:602-614`
- Modify: `selfhost/cgen.lmn:1151-1160`
- Modify: `src/std/sql.lmn:31-74` (declarations), `111-143` (`fail`, `connect`, `open`)
- Test: `test/test_limen.ml:4024-4033` (`sql-sidecar-links`)

**Interfaces:**
- Consumes: `w_fs_entry`, `WFsAt`, `WFsErr`, `w_fs_at_free` (`src/runtime/limen_sandbox.h:672-686, 1051`); `W_FS_TOP` (`limen_rt.h:855`); `w_fs_top_path`, `w_fs_top_mu` (`limen_sandbox.h:731-733`).
- Produces:
  - `W_SQL_OPEN_V2, W_SQL_VFS_FIND, W_SQL_VFS_REGISTER` appended to the `W_SQL_*` enum before `W_SQL_NAPI`; `"sqlite3_open_v2"; "sqlite3_vfs_find"; "sqlite3_vfs_register"` appended to `Ffi.sql_syms` and `ffi.sql_syms()` in that order.
  - `enum { W_SQL_OPEN_OK, W_SQL_OPEN_DENIED, W_SQL_OPEN_FAILED, W_SQL_OPEN_SQLITE };` (0 to 3).
  - `static void w_sql_open_core(const void *const *api, const char *top, const char *root, const char *name, size_t nlen, int flags, WSqlBuf *out);` Result: `what`, `rc`, `db` as three int64 (machine order), then the message as int64 length + bytes. For `DENIED` and `FAILED` the message is the whole error; for `OK` and `SQLITE` it is the database's absolute path as `w_fs_full_path` would give it (shown in "cannot open" errors).
  - `W_RT WStr *w_sql_open(const void *const *api, WStr *root, WStr *name, int64_t flags);` (native; arguments borrowed, as for `w_sql_run`).
  - OCaml: `external sql_open : nativeint array -> string -> string -> int -> string = "limen_sql_open"`.
  - Runtime helper `("limen_sql_open", ([ Str; Str; Int ], Bytes))`, Limen `fn limen_sql_open(root: Str, name: Str, flags: Int) -> Bytes`.
  - `sql.lmn`: `fn ready(rc: Int, h: Handle, shown: Str) -> Result[Db, IoError]`.

- [ ] **Step 1: Write the failing test**

Rewrite `sql-sidecar-links` (lines 4024-4033): add links `walout.db-wal` and `shmout.db-shm` to `outside`, keep `evil.db-journal`, `broken.db-wal` and `fine.db-shm` (to `fine-target`, inside the grant). The program prints `sql.open(root.fs, name).is_err()` for `["evil.db", "broken.db", "fine.db", "walout.db", "shmout.db"]`, then the first 50 bytes of the `Denied` message for `evil.db` as now, then `{e}` for the `Err(e)` of `fine.db` (the `Ok` arm stays). Expected:

```
true
true
true
true
true
sql: refusing to open `evil.db`: `evil.db-journal`
Denied("sql: refusing to open `fine.db`: `fine.db-shm` is a symbolic link")
```

`sql-sidecar-untouched` stays as it is.

- [ ] **Step 2: Run it to see it fail**

Run: `LIMEN_NATIVE_ONLY=sql-sidecar dune exec ./test/test_limen.exe 2>&1 | grep sql-sidecar`
Expected: FAIL `sql-sidecar-links` (`fine.db` opens today).

- [ ] **Step 3: Grow the SQLite table**

Append the three enum entries and the three symbols (Interfaces). Declare the two new functions in `sql.lmn`'s `foreign "sqlite3"` block so the program links them (as `sqlite3_progress_handler` is declared but only called from C): `fn sqlite3_vfs_find(name: Int) -> Handle`, `fn sqlite3_vfs_register(vfs: Handle, make_default: Int32) -> Int32`.

- [ ] **Step 4: Implement `w_sql_open_core` in `src/runtime/limen_sql.h`**

In order: refuse a `name` with a NUL byte as the walk does (`FAILED`, "fs: the path contains a NUL byte"); `w_fs_entry(top, root, name, nlen, W_FS_FOLLOW | W_FS_CREATE, &at, &e)` (the walk `w_fs_full_path` does, so the errors are the ones `sql.open` gives today; `e.kind` 1 is `DENIED`, 2 `FAILED`); build the shown path as `w_fs_full_path` does (`limen_sandbox.h:1104-1110`); for each of `-journal`, `-wal`, `-shm`, `fstatat(at.dfd, <at.name><suffix>, &st, AT_SYMLINK_NOFOLLOW)` and on `S_ISLNK` give `DENIED` with exactly today's message (`sql.lmn:139`, using the `name` as given):

```
sql: refusing to open `<name>`: `<name><suffix>` is a symbolic link
```

then call `api[W_SQL_OPEN_V2]` (`int (*)(const char *, void **, int, const char *)`) on the shown path with `flags` and a NULL VFS, and write `OK` or `SQLITE` with its rc and handle. Document the result layout in the file's header comment next to `w_sql_core`'s.

- [ ] **Step 5: Native: `w_sql_open` in `limen_rt.c`, declared in `limen_rt.h`**

Copies `root` and `name` into NUL-terminated buffers (`WStr` data need not end in NUL), calls `w_sql_open_core(api, W_FS_TOP, ...)` on the calling thread, returns `Str_from(o.p, o.len)`.

- [ ] **Step 6: Interpreter: stub `limen_sql_open` in `src/ffi_stubs.c`**

`value limen_sql_open(value api, value root, value name, value flags)`: copies the strings, reads `top` as a copy of `w_fs_top_path` under `w_fs_top_mu`, calls `w_sql_open_core`, returns the buffer as a string. The interpreter's Fs keeps its granted directory per value (`src/caps.ml:239`), out of the helper's reach; the kept descriptor's path is that directory, because `sql.open` walks to the root (`fs.full_path(".")`) just before and the interpreter runs one top at a time (`limen_sandbox.h:728-730`). Say so in the stub's comment. A NULL `top`, or one `root` is not inside (`w_fs_inside`), is `DENIED` with `W_FS_NOT_GRANTED`'s text, "fs: not granted (run with --grant fs=DIR)".

- [ ] **Step 7: Wire the helper in both compilers**

- `src/ffi.ml`: add the `runtime_fns` entry and a line in the doc comment above it ("limen_sql_open(root, name, flags): open the database `name` under the Fs root `root` (runtime/limen_sql.h); it calls [sql_syms] too").
- `selfhost/ffi.lmn`: the same entry in `runtime_fns()`.
- `src/interp.ml`: lift the table building of `limen_sql_run` (lines 1631-1637) into a local `sql_api ()` and add `| "limen_sql_open", [ VStr root; VStr name; VInt flags ] -> VBytes (sql_open (sql_api ()) root name (Int64.to_int flags))`.
- `src/cgen.ml`: lift lines 608-609 into a local `use_sql_api ()` and add `| "limen_sql_open", [ root; name; flags ] -> use_sql_api (); set (sprintf "w_sql_open(w_sql_api, %s, %s, %s)" root name flags)`.
- `selfhost/cgen.lmn`: the same arm, producing the same C text.

- [ ] **Step 8: Rewrite `open` in `src/std/sql.lmn`**

Add `fn limen_sql_open(root: Str, name: Str, flags: Int) -> Bytes` to `foreign "limen"`. Split `connect` (lines 115-129): `ready(rc, h, shown)` is everything after `sqlite3_open_v2` (the error with `shown` in place of `path`, the limit, busy timeout and `temp_store` pragma); `memory()` calls `sqlite3_open_v2(":memory:", open_flags(false), 0)` then `ready`. `open` becomes:

```limen
pub fn open(fs: Fs, name: Str) -> Result[Db, IoError] {
    let root = fs.full_path(".")?
    let r = limen_sql_open(root, name, open_flags(fs.is_read_only()))
    let (msg, _) = text_at(r, 24)
    match limen_bytes_int(r, 0) {
        1 => Err(Denied(msg)),
        2 => Err(Failed(msg)),
        _ => ready(limen_bytes_int(r, 8), Handle { addr: limen_bytes_int(r, 16) }, msg),
    }
}
```

Update the comments at lines 24-30 and 63-64 to say opening goes through `limen_sql_open`.

- [ ] **Step 9: Run the sql tests**

Run: `LIMEN_NATIVE_ONLY=sql dune exec ./test/test_limen.exe 2>&1 | grep -E 'FAIL|sql-' | grep FAIL`
Expected: no output.

- [ ] **Step 10: Run the four checks**

Run: `dune build && dune exec ./test/test_limen.exe && sh selfhost/check.sh && sh selfhost/bootstrap.sh`
Expected: all pass (`check.sh` compares the C of `selfhost/tests/c/databases.lmn` and `sql-rows.lmn`, which now call `w_sql_open`).

- [ ] **Step 11: Commit**

```bash
git add src/runtime/limen_sql.h src/runtime/limen_rt.h src/runtime/limen_rt.c src/ffi_stubs.c src/ffi.ml selfhost/ffi.lmn src/interp.ml src/cgen.ml selfhost/cgen.lmn src/std/sql.lmn test/test_limen.ml
git commit -m "sql: open through a runtime helper that walks from the grant"
```

---

### Task 3: The VFS

The `limen` VFS in `src/runtime/limen_sql.h`, and `w_sql_open_core` switched to it. Mirrors SQLite's `os_unix.c` (`unixOpen`, `unixLock`, `posixUnlock`, `unixClose`, `unixOpenSharedMemory`, `unixShmMap`, `unixShmLock`, `unixShmUnmap`); the blocks below fix what differs or what the signatures leave open.

**Files:**
- Modify: `src/runtime/limen_sql.h` (new section after `w_sql_open_core`; switch the open call in `w_sql_open_core`)
- Test: `test/test_limen.ml` (new test after `sql-sidecar-untouched`)

**Interfaces:**
- Consumes: `w_sql_open_core` and the `W_SQL_OPEN_V2`, `W_SQL_VFS_FIND`, `W_SQL_VFS_REGISTER` table entries (Task 2).
- Produces (all `static` in `limen_sql.h`):
  - `static int w_sql_vfs_ready(const void *const *api);` registers the VFS once (`pthread_once`; `api` stored in a static first), returns 0 or SQLite's rc. `sqlite3_vfs_find(NULL)` is called before `sqlite3_vfs_register(&w_sql_vfs, 0)` and kept in `pAppData`.
  - `static int w_sql_dir_add(int dfd, const char *db, char *vname, size_t vn);` takes ownership of `dfd`, writes the VFS filename, returns the slot index or -1.
  - `static WSqlDir *w_sql_dir_ref(const char *vname, const char **base);` and `static void w_sql_dir_unref(WSqlDir *d);`
  - VFS name `"limen"`.

Values from sqlite3.h the code needs (declare as an enum in the file): `OK 0, ERROR 1, BUSY 5, READONLY 8, IOERR 10, NOTFOUND 12, FULL 13, CANTOPEN 14`; extended `IOERR_SHORT_READ 522, IOERR_WRITE 778, IOERR_FSYNC 1034, IOERR_DIR_FSYNC 1290, IOERR_TRUNCATE 1546, IOERR_FSTAT 1802, IOERR_UNLOCK 2058, IOERR_RDLOCK 2314, IOERR_DELETE 2570, IOERR_LOCK 3850, IOERR_SHMOPEN 4618, IOERR_SHMSIZE 4874, IOERR_SHMMAP 5386, IOERR_DELETE_NOENT 5898, READONLY_CANTINIT 1288, READONLY_DIRECTORY 1544`; open flags `READONLY 0x1, READWRITE 0x2, CREATE 0x4, DELETEONCLOSE 0x8, EXCLUSIVE 0x10, MAIN_DB 0x100, MAIN_JOURNAL 0x800, SUPER_JOURNAL 0x4000, WAL 0x80000`; locks `NONE 0, SHARED 1, RESERVED 2, PENDING 3, EXCLUSIVE 4`; `ACCESS_EXISTS 0, ACCESS_READWRITE 1`; `SYNC_FULL 3`; `SHM_UNLOCK 1, SHM_LOCK 2, SHM_SHARED 4, SHM_EXCLUSIVE 8, SHM_NLOCK 8`; `IOCAP_POWERSAFE_OVERWRITE 0x1000`. Lock bytes: `PENDING 0x40000000`, `RESERVED PENDING+1`, `SHARED_FIRST PENDING+2`, `SHARED_SIZE 510`; shm lock bytes from `SHM_BASE 120`, `SHM_DMS 128`.

- [ ] **Step 1: Write the failing test `sql-swap-wal`**

With `out`, grants `{ sqlg with fs = Some (sqlreal, false); foreign = [ "c" ] }`. SQLite's unix VFS follows a symlinked `-wal` (it opens `-shm` with `O_NOFOLLOW` already), so a `-wal` that becomes a link after `sql.open` checked it redirects WAL writes into the link's target:

```limen
use sql

foreign "c" {
    fn symlink(target: Str, path: Str) -> Int32
}

fn main(root: Root) -> Result[(), IoError] {
    let out = root.stdout
    let fs = root.fs
    fs.write("swap-victim", "keep")?
    fs.remove("swap.db")
    let first = sql.open(fs, "swap.db")?
    first.exec("PRAGMA journal_mode = WAL")?
    first.exec("CREATE TABLE t (x)")?
    first.close()?
    // sql.open checks the side files; -wal becomes a link before the first query
    let db = sql.open(fs, "swap.db")?
    out.print("{symlink(root.foreign, fs.full_path("swap-victim")?, fs.full_path("swap.db-wal")?)}")
    out.print("{db.exec("INSERT INTO t VALUES (1)").is_err()} {db.query("SELECT x FROM t").is_err()}")
    db.close()?
    out.print(fs.read("swap-victim")?)
    for f in ["swap.db-wal", "swap.db-shm", "swap.db", "swap-victim"] {
        fs.remove(f)
    }
    Ok(())
}
```

Expected: `"0\ntrue true\nkeep\n"`.

- [ ] **Step 2: Run it to see it fail**

Run: `LIMEN_NATIVE_ONLY=sql-swap dune exec ./test/test_limen.exe 2>&1 | grep sql-swap`
Expected: FAIL, with `swap-victim` no longer `keep` (SQLite wrote a WAL header into it). If it passes here, the test proves nothing: stop and report.

- [ ] **Step 3: Declare the structs and the slot table**

`WSqlVfs` (`sqlite3_vfs` v3: `iVersion, szOsFile, mxPathname, pNext, zName, pAppData, xOpen, xDelete, xAccess, xFullPathname, xDlOpen, xDlError, xDlSym, xDlClose, xRandomness, xSleep, xCurrentTime, xGetLastError, xCurrentTimeInt64, xSetSystemCall, xGetSystemCall, xNextSystemCall`), `WSqlIo` (`sqlite3_io_methods` v3: `iVersion, xClose, xRead, xWrite, xTruncate, xSync, xFileSize, xLock, xUnlock, xCheckReservedLock, xFileControl, xSectorSize, xDeviceCharacteristics, xShmMap, xShmLock, xShmBarrier, xShmUnmap, xFetch, xUnfetch`), `WSqlFile` (first member `const WSqlIo *methods`, then `fd`, `WSqlDir *dir`, `char *base`, `WSqlInode *inode`, `int lock`, `int dirsync`, `WSqlShmConn *shm`). `szOsFile = sizeof(WSqlFile)`, `mxPathname = 512`. The `xSetSystemCall` family is NULL; `xDlOpen` to `xCurrentTimeInt64` call the same method of the default VFS in `pAppData`.

One mutex, `static pthread_mutex_t w_sql_mu`, guards the slot table, the inode list and every field of `WSqlInode`, `WSqlShm` and `WSqlShmConn`. Never call into SQLite while holding it.

The slot table and the filename encoding:

```c
/* A database's directory, held for its connection: a dup of the walk's
   descriptor (F_DUPFD_CLOEXEC; the walk's may be the kept granted one),
   the database's name in it, and how many users it has: one for
   w_sql_open_core while it opens, and one per open file. At 0 the
   descriptor is closed and the slot is free; gen grows each time, so a
   stale filename never reaches the next database to get the slot. */
typedef struct { int dfd; char *db; uint64_t gen; int refs; } WSqlDir;
static WSqlDir **w_sql_dirs;   /* grown, never shrunk; pointers stay valid */
static int w_sql_ndirs;

/* The filename SQLite gets: "/limen-sql/<slot>-<gen>/<db>". SQLite appends
   "-journal", "-wal" and "-shm" to it, and xFullPathname returns it
   unchanged, so every file of the connection names the same slot.
   w_sql_dir_ref parses it strictly: the prefix, a decimal slot, '-', a
   decimal gen matching the slot's, '/', then a non-empty base with no
   '/'; anything else is NULL (SQLITE_CANTOPEN). It counts a user. */
```

`w_sql_open_core` now: `w_sql_vfs_ready(api)` (failure is `FAILED`, "sql: cannot register SQLite's VFS"), the walk and side-file checks as before, `w_sql_dir_add(dup of at.dfd, at.name, ...)`, `sqlite3_open_v2(vname, &db, flags, "limen")`, then `w_sql_dir_unref`, whatever the rc. The shown path in the result stays the real absolute path.

- [ ] **Step 4: Implement the VFS methods**

- `xOpen(vfs, zName, file, flags, outFlags)`: `methods = NULL` first, so SQLite never closes a file that failed. No name (a temporary file) is `CANTOPEN`. `w_sql_dir_ref`. `openat(dfd, base, (RO ? O_RDONLY : O_RDWR) | (CREATE ? O_CREAT : 0) | (EXCLUSIVE ? O_EXCL : 0) | O_NOFOLLOW | O_CLOEXEC | O_NOCTTY, mode)`, retried on `EINTR`. Mode: 0644 for the database; for the others the database's `st_mode & 0777` from `fstatat(dfd, dir->db, AT_SYMLINK_NOFOLLOW)`, and `fchown` to its owner when `geteuid() == 0`. A read-write open that fails with anything but `EISDIR` is retried read-only and `*outFlags` says `READONLY` (unix does). A new journal or WAL that fails with `EACCES` and does not exist is `READONLY_DIRECTORY`. Otherwise failure is `CANTOPEN` (a link gives `ELOOP` here: this is what closes the race). `DELETEONCLOSE`: `unlinkat` at once. `dirsync` is set for a created `MAIN_JOURNAL`, `SUPER_JOURNAL` or `WAL`. `fstat` the descriptor and attach the inode (below).
- `xRead`/`xWrite`: `pread`/`pwrite` loops over `EINTR`; a short read zero-fills the rest and is `IOERR_SHORT_READ`; a short write is `FULL` when `errno` is `ENOSPC` or nothing failed, else `IOERR_WRITE`.
- `xTruncate`: `ftruncate`, `IOERR_TRUNCATE`. `xFileSize`: `fstat`, `IOERR_FSTAT`; a size of 1 reads as 0 (unix does).
- `xSync`: `F_FULLFSYNC` (where defined) when `(flags & 0x0F) == SYNC_FULL`, else or on its failure `fsync`; `IOERR_FSYNC`. Then, once, if `dirsync`: `openat(dfd, ".", O_RDONLY | O_DIRECTORY | O_CLOEXEC)` (the held one may be `O_PATH` on Linux, which `fsync` refuses), `fsync`, close, errors ignored.
- `xFileControl`: `NOTFOUND`. `xSectorSize`: 4096. `xDeviceCharacteristics`: `IOCAP_POWERSAFE_OVERWRITE`. `xFetch`: `*pp = NULL`, `OK`. `xUnfetch`: `OK`.
- `xDelete(vfs, name, syncDir)`: `unlinkat(dfd, base, 0)`; `ENOENT` is `IOERR_DELETE_NOENT`, other errors `IOERR_DELETE`; with `syncDir`, the directory sync above, failure `IOERR_DIR_FSYNC`.
- `xAccess`: `EXISTS` is `fstatat(dfd, base, AT_SYMLINK_NOFOLLOW) == 0 && (!S_ISREG || st_size > 0)` (a link exists, so SQLite goes on to open it and is refused); `READWRITE` is not a link and `faccessat(dfd, base, R_OK | W_OK, 0) == 0`. An unknown slot answers 0.
- `xFullPathname`: copies the name; `CANTOPEN` if it does not fit in `nOut`.
- `xClose`: see the inode table.

- [ ] **Step 5: The inode lock table and `xLock`, `xUnlock`, `xCheckReservedLock`, `xClose`**

POSIX locks belong to the process, never conflict within it, and all go when any descriptor of the file closes; so connections in one process share one record per file and arbitrate between themselves:

```c
typedef struct WSqlInode {
  dev_t dev; ino_t ino;
  int refs;               /* open WSqlFiles */
  int shared;             /* connections holding SHARED or more */
  int lock;               /* the strongest any connection holds */
  int nlock;              /* files holding any lock */
  int *unused, nunused;   /* closed while nlock > 0; closed when it is 0 */
  struct WSqlShm *shm;    /* the -shm, while mapped */
  struct WSqlInode *next;
} WSqlInode;

/* setlk(fd, type, start, len): fcntl(F_SETLK); EAGAIN/EACCES -> BUSY, else IOERR_LOCK */

xLock(f, want):                                   /* under w_sql_mu */
  if f->lock >= want: return OK
  in = f->inode
  if f->lock != in->lock && (in->lock >= PENDING || want > SHARED): return BUSY
  if want == SHARED && (in->lock == SHARED || in->lock == RESERVED):
    f->lock = SHARED; in->shared++; in->nlock++; return OK
  if want == SHARED || (want == EXCLUSIVE && f->lock == RESERVED):
    rc = setlk(fd, want == SHARED ? F_RDLCK : F_WRLCK, PENDING, 1); if rc: return rc
    if want == EXCLUSIVE: f->lock = in->lock = PENDING
  if want == SHARED:
    rc = setlk(fd, F_RDLCK, SHARED_FIRST, SHARED_SIZE)
    if unlock(fd, PENDING, 1) fails && rc == OK: rc = IOERR_UNLOCK
    if rc: return rc
    f->lock = in->lock = SHARED; in->nlock++; in->shared = 1; return OK
  if want == EXCLUSIVE && in->shared > 1: rc = BUSY
  else rc = setlk(fd, F_WRLCK, want == RESERVED ? RESERVED : SHARED_FIRST, want == RESERVED ? 1 : SHARED_SIZE)
  if rc == OK: f->lock = in->lock = want
  else if want == EXCLUSIVE: f->lock = in->lock = PENDING
  return rc

xUnlock(f, to):                                   /* to is SHARED or NONE */
  if f->lock <= to: return OK
  if f->lock > SHARED:
    if to == SHARED && setlk(fd, F_RDLCK, SHARED_FIRST, SHARED_SIZE): return IOERR_RDLOCK
    if unlock(fd, PENDING, 2) fails: return IOERR_UNLOCK
    in->lock = SHARED
  if to == NONE:
    if --in->shared == 0:
      if unlock(fd, 0, 0) fails: rc = IOERR_UNLOCK, f->lock = in->lock = NONE
      else in->lock = NONE
    if --in->nlock == 0: close every in->unused
  f->lock = to (when rc == OK)

xCheckReservedLock: in->lock > SHARED, or else F_GETLK for F_WRLCK on RESERVED is not F_UNLCK.

xClose: xUnlock(NONE); then under w_sql_mu: if in->nlock > 0, move fd to in->unused
  instead of closing it; in->refs--, and at 0 close the unused ones, unlink and free
  the record; then close fd (if kept), w_sql_dir_unref, free base.
```

The record is found by `(st_dev, st_ino)` in `xOpen` (created at `refs` 0 if missing) for every file, not only the database.

- [ ] **Step 6: WAL shared memory**

`xShmMap`, `xShmLock`, `xShmBarrier`, `xShmUnmap` are called on the database file. One `WSqlShm` per inode (`in->shm`), one `WSqlShmConn` per connection:

```c
typedef struct WSqlShm {
  int fd, readonly, unlocked;
  int region_size, nregion; char **regions;
  int lock[8];            /* per shm lock: > 0 shared holders, -1 exclusive */
  int refs;
} WSqlShm;
typedef struct WSqlShmConn { WSqlShm *node; uint16_t shared, excl; } WSqlShmConn;
```

- Opening (on the first `xShmMap` of a connection): reuse `in->shm`, or open `<dir->db>-shm` with `openat(dfd, ..., O_RDWR | O_CREAT | O_NOFOLLOW | O_CLOEXEC, <db's mode>)`, falling back to `O_RDONLY | O_NOFOLLOW` with `readonly = 1`; neither is `CANTOPEN`. `fchown` as for the other side files. Then the dead-man switch, as `unixLockSharedMemory`:

```c
F_GETLK for F_WRLCK on SHM_DMS:
  error            -> IOERR_LOCK
  F_UNLCK (nobody) -> readonly ? (unlocked = 1, READONLY_CANTINIT)
                              : (setlk F_WRLCK on SHM_DMS, then ftruncate(fd, 3), failure IOERR_SHMOPEN)
  F_WRLCK          -> BUSY
then, if OK: setlk F_RDLCK on SHM_DMS (held for the node's life)
```
A result other than `OK` or `READONLY_CANTINIT` closes the descriptor and fails the map. Any `fcntl` failure on the `-shm` is `BUSY` (unix does).

- `xShmMap(f, i, size, extend, pp)`: open if needed; if `unlocked`, run the switch again. If `nregion <= i`: `fstat` the `-shm`; when it is smaller than `(i + 1) * size` and `extend` is 0, `*pp = NULL`, `OK`; when extending, write one zero byte at the end of each 4096-byte page up to that size (`IOERR_SHMSIZE` on failure). Map in groups so offsets stay page-aligned:

```c
per = pagesize < size ? 1 : pagesize / size;      /* 2 on 64 KB pages */
for (r = nregion; r < i + 1; r += per)            /* r is always a multiple of per */
  p = mmap(NULL, size * per, PROT_READ | (readonly ? 0 : PROT_WRITE), MAP_SHARED, fd, (off_t)size * r);
  if p == MAP_FAILED: return IOERR_SHMMAP
  regions[r + k] = p + size * k  for k in 0..per-1
```
`*pp = regions[i]`. A read-only node returns `READONLY` instead of `OK` when it succeeds.

- `xShmLock(f, ofst, n, flags)`, on the shm `fd` at `SHM_BASE + ofst`, mask `((1 << (ofst + n)) - (1 << ofst))`:

```c
UNLOCK:  if this conn holds none of mask: OK
         if SHARED and lock[ofst] > 1: lock[ofst]--, drop mask from conn->shared, OK
         else unlock the bytes; on success clear lock[ofst..ofst+n) and both masks
SHARED:  (n == 1) if conn->shared has it: OK
         if lock[ofst] < 0: BUSY
         if lock[ofst] == 0: setlk F_RDLCK (BUSY on failure)
         lock[ofst]++, conn->shared |= mask
EXCLUSIVE: if conn->excl already has all of mask: OK
         if any lock[k] != 0 for k in range: BUSY
         setlk F_WRLCK on n bytes (BUSY on failure); lock[k] = -1, conn->excl |= mask
```
- `xShmBarrier`: `__atomic_thread_fence(__ATOMIC_SEQ_CST)`, then lock and unlock `w_sql_mu`.
- `xShmUnmap(f, delete)`: free the connection's record; `refs--`; at 0, if `delete`, `unlinkat(dfd, "<dir->db>-shm", 0)`; `munmap` each group (at regions whose index is a multiple of `per`, length `size * per`), close the fd, `in->shm = NULL`.

- [ ] **Step 7: Run the swap test and the sql tests**

Run: `LIMEN_NATIVE_ONLY=sql dune exec ./test/test_limen.exe 2>&1 | grep -E 'sql-' | grep FAIL`
Expected: no output (`sql-swap-wal` passes; `sql-wal`, `sql-locks-survive-close`, `sql-busy-cancel`, `sql-file`, `sql-db-link` and the rest unchanged).

- [ ] **Step 8: Run the four checks**

Run: `dune build && dune exec ./test/test_limen.exe && sh selfhost/check.sh && sh selfhost/bootstrap.sh`
Expected: all pass.

- [ ] **Step 9: Commit**

```bash
git add src/runtime/limen_sql.h test/test_limen.ml
git commit -m "sql: a VFS that opens every file from the database's held directory"
```

---

### Task 4: Say so, and run the server under load

**Files:**
- Modify: `src/std/sql.lmn:12-20` (header comment)
- Modify: `src/runtime/limen_sql.h:1-44` (header comment: the VFS and the open helper)
- Modify: `SPEC.md:2092`, `SPEC.md:2108`
- Modify: `ROADMAP.md:150-156`

**Interfaces:**
- Consumes: the behaviour of Tasks 2 and 3.

- [ ] **Step 1: Update the comments and SPEC.md**

- `sql.lmn:12-20`: every file SQLite opens is opened from the database's directory, held from the grant, never through a symbolic link: the database (whose name may not be a link), and its `-journal`, `-wal` and `-shm`, any of which is refused up front if it is a link and fails to open if it becomes one later.
- `SPEC.md:2092`: drop "(`fs.full_path`)"; say the name is walked from the `Fs` as every Fs operation is.
- `SPEC.md:2108`: replace "The database file is not opened through a symlink, ... closing them in Limen itself is on the roadmap (0.8)." with: SQLite opens the database and its side files through Limen, each with `openat` and `O_NOFOLLOW` from the database's directory, which `open` reaches from the granted directory as every Fs operation does and holds while the database is open; so nothing swapped in later, by any process, can lead SQLite elsewhere. `open` refuses a database whose name, `-journal`, `-wal` or `-shm` is a symbolic link, even one pointing inside the grant.

- [ ] **Step 2: Mark the roadmap item done**

`ROADMAP.md:150-156`, in the list's style: "(done: SQLite opens every file through a Limen VFS, with `openat` and `O_NOFOLLOW` from the database's held directory, WAL included)", keeping the item's first sentence.

- [ ] **Step 3: Check the book needs nothing**

Run: `grep -rn -e '-journal' -e '-wal' -e '-shm' -e 'symlink' ~/Github/Personal/limen-book/tools/chapters`
Expected: no output (nothing to update; do not commit or push there).

- [ ] **Step 4: Load proof: `todo_api` with WAL on**

In a fresh directory `d=$(mktemp -d)`, copy `examples/todo_api.lmn` to `$d/todo_wal.lmn` and add `db.exec("PRAGMA journal_mode = WAL")?` on the line after `let db = sql.open(root.fs, "todos.db")?` (line 257). Build and serve:

```bash
_build/default/bin/main.exe build $d/todo_wal.lmn -o $d/todo_wal
$d/todo_wal --grant listen=127.0.0.1:8080 --grant fs=$d -- serve &
oha -z 60s -c 50 -m POST -d '{"title": "buy milk"}' http://127.0.0.1:8080/todos &
oha -z 60s -c 50 http://127.0.0.1:8080/todos/1
```

Expected: `ls $d` shows `todos.db-wal` and `todos.db-shm` while it runs; both `oha` reports show only 2xx statuses and no errors; `ps -o rss= -p <pid>` sampled every 10 s stays flat. Stop the server and delete `$d`. Nothing to commit from this step; give the numbers in the commit message of step 6.

- [ ] **Step 5: Run the four checks**

Run: `dune build && dune exec ./test/test_limen.exe && sh selfhost/check.sh && sh selfhost/bootstrap.sh`
Expected: all pass.

- [ ] **Step 6: Commit**

```bash
git add src/std/sql.lmn src/runtime/limen_sql.h SPEC.md ROADMAP.md
git commit -m "SPEC, ROADMAP: SQLite opens its files from the grant"
```
