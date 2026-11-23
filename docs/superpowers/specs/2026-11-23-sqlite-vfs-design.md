# SQLite files opened from the grant, not by path

`sql.open` hands SQLite an absolute path, and SQLite opens the database and
its `-journal`, `-wal` and `-shm` files by that path, following symlinks. A
process with write access to the granted directory can swap a name for a
symlink after Limen checks it and before SQLite opens it. The OS sandbox
refuses what the swap would reach; this closes it in Limen itself. It is the
`full_path` item of 0.8 in ROADMAP.md.

## Goals

- Every file SQLite opens for a `sql.open(fs, name)` database is opened with
  `openat` relative to a directory descriptor inside the grant, never
  through a symlink, so no swap after the check can redirect it.
- WAL works as it does today (decided): `PRAGMA journal_mode = WAL`, its
  `-wal` file and its `-shm` shared memory.
- Nothing a program sees changes: same results, same errors for a missing or
  read-only database, same behaviour on cancellation.

## Non-goals

- `sql.memory()`: no files, unchanged.
- Other foreign libraries that take paths. They need a `foreign` grant and
  are outside this.
- Windows.

## Approaches

1. **A Limen VFS that does its own opening and I/O** (picked). Registered
   with SQLite once, used for every `sql.open`. It opens with `openat` and
   `O_NOFOLLOW` from a held directory descriptor and does reads, writes,
   syncs, `fcntl` locks and the WAL shared memory itself.
2. *A thin wrapper around SQLite's unix VFS.* Rejected: the unix VFS still
   opens by path, so the race stays.
3. *`/proc/self/fd/N/name` paths.* Rejected: Linux only, and SQLite resolves
   links in the paths it is given.
4. *The same VFS without WAL.* About half the code, but `journal_mode = WAL`
   would silently stay in rollback mode. Rejected by Alfonso: a server wants
   WAL.

## The design

- The VFS lives in `src/runtime/limen_sql.h`, so the native runtime and the
  interpreter (through `ffi_stubs.c`) share one copy, as `w_sql_core` is
  shared today. SQLite's functions it needs (`sqlite3_vfs_find`,
  `sqlite3_vfs_register`) come through the existing `w_sql_api` table, so
  it still needs no `sqlite3.h` and no link of its own.
- **The held directory.** The database's directory is reached from the
  grant's root by the sandbox's existing walk (each name opened with
  `openat` and `O_NOFOLLOW`, `O_NOFOLLOW_ANY` or `openat2` where the system
  has them), and its descriptor is kept for as long as the connection is
  open. The database, `-journal`, `-wal` and `-shm` are all opened with
  `openat(dirfd, name, ... | O_NOFOLLOW | O_CLOEXEC)`. A name that is a
  symlink is refused with the same `Denied` message as today.
- **How the VFS finds the directory.** The filename handed to
  `sqlite3_open_v2` names a slot in a table of held directories rather than
  a real path; `xFullPathname` returns it unchanged, so SQLite's suffixed
  names (`<name>-wal`) still map to the same directory. The exact encoding
  is left to the builder.
- **I/O and locking** follow SQLite's unix VFS semantics: `pread`/`pwrite`,
  `fsync` (`F_FULLFSYNC` on macOS), POSIX advisory locks with the
  per-inode lock sharing the unix VFS uses so two connections in one process
  don't drop each other's locks, and the shared-memory methods (`xShmMap`,
  `xShmLock`, `xShmBarrier`, `xShmUnmap`) over the `-shm` file mapped with
  `mmap`. Everything else (`xRandomness`, `xSleep`, `xCurrentTime`,
  `xGetLastError`, `xDlOpen` and the rest) is delegated to the default VFS.
- **`xDelete` and `xAccess`** use `unlinkat` and `faccessat`/`fstatat` with
  `AT_SYMLINK_NOFOLLOW` on the held directory.
- **How `sql.lmn` reaches it** (my call). A new runtime helper in the
  `foreign "limen"` block, `limen_sql_open(root, name, flags)`, as
  `limen_sql_run` is today. `root` is the grant's root (`fs.full_path(".")`
  gives it), `name` the database's name relative to it. The helper
  registers the VFS once (on first use, under a once-guard), walks to the
  database's directory, checks the side files, opens the database through
  the VFS and returns SQLite's result code and the handle. `connect` for
  `memory()` keeps the default VFS.
- **Errors stay where they are** (my call). The helper checks
  `-journal`, `-wal` and `-shm` with `fstatat(..., AT_SYMLINK_NOFOLLOW)`
  before opening, so `sql.open` still returns the same `Denied` message up
  front instead of a `Failed` from a later query. The VFS refuses them at
  open time as well, which is what closes the race.
- **Symlinks.** The database's own name is refused when it is a link, as
  today (`full_path` does not follow a final link, and `sql.open` passes
  `SQLITE_OPEN_NOFOLLOW`). A `-journal`, `-wal` or `-shm` that is a link is
  now refused even when it points inside the grant (today only one leading
  out is). That is the one change a program could see, and only one that
  links its side files. The `Denied` message says so plainly (my call):
  "sql: refusing to open `{name}`: `{name}{suffix}` is a symbolic link".
- **Temporary files** are already kept in memory (`temp_store = MEMORY`);
  `xOpen` refuses a file with no name, so SQLite can never create one
  elsewhere.

## Constraints

- Both backends share the one C copy. Both compilers change where the
  SQLite function table and the runtime helpers are listed: the `W_SQL_*`
  table filled from `Ffi.sql_syms` (`src/ffi.ml`, `selfhost/ffi.lmn`), and
  the `limen` helpers in `interp.ml`, `cgen.ml` and `selfhost/cgen.lmn`,
  which gain `sqlite3_vfs_find`, `sqlite3_vfs_register` and
  `limen_sql_open`.
- SQLite 3.37 or later, as today (the runtime already calls `sqlite3_changes64`).
  The VFS structs are declared by hand to version 3 of `sqlite3_io_methods`
  and version 3 of `sqlite3_vfs`; `xFetch` returns no mapping, so SQLite
  reads with `xRead` (memory-mapped I/O stays off, as it is by default).
- Behaviour the unix VFS has and this one copies (my call): new side files
  get the database file's mode and owner, the directory is synced after a
  journal is created, and a read-only connection to a WAL database follows
  the unix VFS's read-only `-shm` handling.
- macOS and Linux, x86-64 and arm64, as CI runs today.
- No new error codes or messages a program can see.

## Proof

- The existing `sql` tests pass under both backends.
- New tests: WAL mode reads while another connection writes; a database
  whose `-wal` or `-shm` is a symlink out of the grant is refused; a test
  that swaps a name for a symlink between `sql.open` and the first query
  and sees the swap fail to redirect anything.
- `examples/todo_api.lmn` runs under load with WAL on, as the 0.4 run did.

## Left to the builder

- The filename encoding for the held-directory table, and when a slot is
  freed (at `sqlite3_close`, after its last file closes).
- How closely to mirror the unix VFS's inode lock table; it must be correct
  for several connections in one process to one database.
- Whether `xOpen` uses the walk directly or a small helper added to
  `limen_sandbox.h`.
