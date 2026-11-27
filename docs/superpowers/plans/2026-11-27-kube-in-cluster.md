# An operator in the cluster Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** `examples/site_operator.lmn` runs as a Deployment in a kind cluster and talks to the API server itself, through `Net.trusting`, `Net.presenting` and a new `kube` module.

**Architecture:** The trust lives on the `Net` capability, the way `only` narrows it: the runtime keeps one OpenSSL client context per trust and identity, shared by the `Net`s and connections that use it, so `http` and everything above `Net` is unchanged. `kube` is ordinary Limen over `http` and `json`.

**Tech Stack:** OCaml 5 (src/), Limen (selfhost/, src/std/), C11 runtime with OpenSSL (src/runtime/limen_tls.h, limen_rt.c, src/tls_stubs.c), dune, kind.

**Spec:** `docs/superpowers/specs/2026-11-27-kube-in-cluster-design.md`

## Global Constraints

- Every change goes into both compilers (`src/` and `selfhost/`) and both backends; SPEC.md describes it in the same commit.
- Gate per task: `dune build --root .`, the task's focused tests (`LIMEN_TEST_ONLY=<names> dune exec --root . ./test/test_limen.exe`), and for `selfhost/` changes a stage-1 build (`LIMEN_CACHE=off _build/default/bin/main.exe build selfhost/limen.lmn -o $TMPDIR/stage1`). The full suite, `selfhost/check.sh`, the bootstrap and Linux arm64 run once, in Task 6.
- The runtime needs only a C compiler, libc and OpenSSL. In the WebAssembly build `trusting` and `presenting` refuse, as `connect_tls` does (`W_WASM_REFUSE`), and `selfhost/eval_prims.lmn` lists them as refused.
- No switch to turn certificate verification off. Nothing read from the environment changes what a trusting `Net` accepts.
- Commit messages plain, no attribution of any kind; British spelling, no em dashes, no emojis. Commit locally per task; never push.
- Error texts start with `tls: ` as the existing TLS errors do.

## Review Focus

1. A `Net` derived with `trusting` from a revoked `Net`, or the parent revoked after: connections through the child are refused as through the parent. Test `net-trusting-revoked` in Task 2.
2. `trusting` with a PEM bundle of two CAs: a server signed by either is accepted. Test `net-trusting-bundle` in Task 2.
3. A PEM file with Windows line endings and a trailing comment line, as `ca.crt` files sometimes have: accepted. Test `net-trusting-crlf` in Task 2.
4. Many `trusting` calls with the same CA in a loop (a client per request): memory stays flat, contexts are freed. Test `net-trusting-leak` (native leakcheck reports 0 live) in Task 2.
5. The token file replaced by kubelet between two requests (new content, newer `modified_ms`): the second request sends the new token. Test `kube-token-rotates` in Task 4.

---

### Task 1: Test CA with a kept key, and a client-certificate fixture

**Files:**
- Create: `test/tls/kube/` with `ca.pem`, `ca-key.pem`, `server.pem`, `server-key.pem` (SANs `localhost`, `127.0.0.1`), `client.pem`, `client-key.pem`, `other-ca.pem`, and `make.sh` that regenerates them with `openssl` (10-year validity).
- Modify: `test/tls/README.md` (what each file is for; this CA is never put in `SSL_CERT_FILE`).

**Interfaces:**
- Produces: the paths above, used by Tasks 2 and 3.

- [ ] **Step 1:** Write `make.sh`; run it; commit its output and itself.
- [ ] **Step 2:** Check `openssl verify -CAfile test/tls/kube/ca.pem test/tls/kube/server.pem test/tls/kube/client.pem` prints `OK` for both.
- [ ] **Step 3:** Commit `test: a CA for trusting and presenting, with its key kept`.

### Task 2: `Net.trusting(ca)`

**Files:**
- Modify: `src/runtime/limen_tls.h` (a client context from PEM text; reference counted), `src/runtime/limen_rt.c` (`Cap_net_trusting`, the `WCap` for Net holds the context, `Cap_net_connect_tls` and `start_tls` on its connections use it; the no-TLS and wasm variants refuse), `src/tls_stubs.c` and `src/tls.ml` (the interpreter's side over the same C), `src/interp.ml` (`CNet` carries the trust beside the host list), `src/check.ml` and `selfhost/builtins.lmn` (`m "trusting" [ TStr ] (res (TCap Net) io_err)`, not an effect), `src/cgen.ml` and `selfhost/cgen.lmn` (`cap_trusting` -> `Cap_net_trusting`, sets `uses_tls`), `selfhost/eval_prims.lmn` (refused), `SPEC.md` (section 10 TLS and the capability table).
- Test: `test/test_limen.ml` beside the TLS tests (~6311).

**Interfaces:**
- Consumes: Task 1's fixtures.
- Produces: `Net.trusting(ca: Str) -> Result[Net, IoError]`; C `W_RT WIo Cap_net_trusting(WCap *n, WStr *pem)`; OCaml `Tls.trust : string -> (trust, string) result` and `Tls.client_with : trust option -> Unix.file_descr -> string -> session`.

- [ ] **Step 1: Tests**, each through `out` (both backends), a Limen server on `on_tls` with `test/tls/kube/server.pem`:
  - `net-trusting`: `net.connect_tls(addr)` fails with `tls: certificate verify failed` (the kube CA is not in `SSL_CERT_FILE`); `net.trusting(ca)?.connect_tls(addr)` reads the server's line; `net.trusting(other_ca)?` fails to connect.
  - `net-trusting-pem-errors`: `trusting("")`, `trusting("not pem")`, a truncated block each give `Err(Failed(...))` starting `tls: ` and naming PEM, at the call.
  - `net-trusting-isolated`: after `let a = net.trusting(ca)?`, the original `net` still refuses the kube server, and a connection to the `test/tls` server through `a` fails.
  - `net-trusting-only`: `net.trusting(ca)?.only("localhost:PORT")` keeps the trust; `only` to a host outside the grant refuses as before.
  - `net-trusting-http`: `http.request(net.trusting(ca)?, "GET", "https://localhost:PORT/")` gets 200.
  - Review Focus tests `net-trusting-revoked`, `net-trusting-bundle` (`ca + other_ca` in one string, a server signed by each), `net-trusting-crlf`, `net-trusting-leak` (2,000 `trusting` calls, native leakcheck 0 live).
- [ ] **Step 2:** Run `LIMEN_TEST_ONLY=net-trusting dune exec --root . ./test/test_limen.exe`: they fail (no method `trusting`).
- [ ] **Step 3:** Implement. Parse every `CERTIFICATE` block with `PEM_read_bio_X509` into a fresh `X509_STORE`; zero certificates is an error. The context copies `vtls_client_init`'s settings except the trust paths. Calling `trusting` on a trusting `Net` replaces the store.
- [ ] **Step 4:** Run the tests: pass. Build stage 1.
- [ ] **Step 5:** Commit `Net.trusting: TLS that trusts a given CA in place of the system's`.

### Task 3: `Net.presenting(cert, key)`

**Files:** as Task 2 (`Cap_net_presenting`, `cap_presenting`, `m "presenting" [ TStr; TStr ] (res (TCap Net) io_err)`), plus `SPEC.md`.

**Interfaces:**
- Consumes: Task 2's context and `CNet` shape.
- Produces: `Net.presenting(cert: Str, key: Str) -> Result[Net, IoError]`.

- [ ] **Step 1: Tests** against `openssl s_server -accept 0 -cert test/tls/kube/server.pem -key ... -CAfile test/tls/kube/ca.pem -Verify 1 -www` started by the test (skipped, with a line saying so, when `openssl` is not on `PATH`):
  - `net-presenting`: through `trusting(ca)` alone the request fails; through `trusting(ca)?.presenting(cert, key)?` it gets the `-www` page.
  - `net-presenting-errors`: a key that isn't the certificate's, bad PEM, an encrypted key each fail at the call with `tls: `.
  - `net-presenting-keeps-trust`: `presenting` then `trusting` and `trusting` then `presenting` both connect.
- [ ] **Step 2:** Run: fail.
- [ ] **Step 3:** Implement with `SSL_CTX_use_certificate_chain` from a memory BIO and `SSL_CTX_use_PrivateKey`, then `SSL_CTX_check_private_key`. A new context per call, keeping the parent's store.
- [ ] **Step 4:** Run: pass. Build stage 1.
- [ ] **Step 5:** Commit `Net.presenting: a client certificate for TLS connections`.

### Task 4: The `kube` module

**Files:**
- Create: `src/std/kube.lmn` (registered with the other standard modules), `test/kube/fake_api.lmn` (a fake API server for the tests, served with `on_tls` and `test/tls/kube/server.pem`).
- Modify: `SPEC.md` (a `kube` section beside `postgres`), the std module list in both compilers if it is listed there.
- Test: `test/test_limen.ml`.

**Interfaces:**
- Consumes: `Net.trusting`.
- Produces:
  - `pub struct Client` (opaque fields)
  - `pub fn in_cluster(fs: Fs, env: Env, net: Net) -> Result[Client, IoError]`: reads `token`, `ca.crt`, `namespace` from `fs`; `KUBERNETES_SERVICE_HOST`, `KUBERNETES_SERVICE_PORT` from `env`; an IPv6 host is bracketed.
  - `pub fn proxy(net: Net, base: Str) -> Client`
  - `pub fn namespace(self: Client) -> Str` (`"default"` for `proxy`)
  - `pub fn get(self: Client, path: Str) -> Result[(Int, json.Value), IoError]`, `list`, `delete` the same shape
  - `pub fn apply(self: Client, path: Str, manager: Str, body: json.Value) -> Result[(Int, json.Value), IoError]` (PATCH, `application/apply-patch+yaml`, `?fieldManager=MANAGER&force=true`)
  - `pub fn patch_status(self: Client, path: Str, body: json.Value) -> Result[(Int, json.Value), IoError]` (PATCH `PATH/status`, `application/merge-patch+json`)
  - `pub fn watch(self: Client, path: Str, resource_version: Str, f: fn(Str) -> Bool) -> Result[Int, IoError]` (`?watch=1&resourceVersion=V&allowWatchBookmarks=true`, each line to `f`, the status code back)
- Every answer is `Ok((code, value))`; a body that isn't JSON is `Err(Failed("kube: ..."))`. Before each request, `fs.info("token")?.modified_ms` is compared with the last read and the token read again when newer; after a 401 it is read again and the request retried once.

- [ ] **Step 1: Tests** with the fake server (it records each request's method, path, headers and body, and answers from a table):
  - `kube-in-cluster`: `Authorization: Bearer <token file>` sent, `namespace()` from the file, the address from the two env vars.
  - `kube-in-cluster-missing`: no token file, no env var each give an `Err` naming what is missing.
  - `kube-apply` (method, content type, query, body), `kube-status` (path ends `/status`, merge patch), `kube-errors` (a 404 and a 409 come back as `Ok` with the `Status` body).
  - `kube-watch`: three event lines reach `f`; returning `false` stops.
  - `kube-401-retry`: the fake answers 401 once; the token file has changed; the retry carries the new token and succeeds.
  - Review Focus `kube-token-rotates`.
  - `kube-proxy`: no `Authorization` header, plain http.
- [ ] **Step 2:** Run `LIMEN_TEST_ONLY=kube`: fail.
- [ ] **Step 3:** Implement.
- [ ] **Step 4:** Run: pass. `limen caps` on a program using `in_cluster` lists `Fs`, `Env`, `Net` and no more.
- [ ] **Step 5:** Commit `kube: a client for the Kubernetes API, in a pod or through kubectl proxy`.

### Task 5: The operator in the cluster

**Files:**
- Modify: `examples/site_operator.lmn` (onto `kube.Client`; `in_cluster` when `KUBERNETES_SERVICE_HOST` is set, `proxy` otherwise; its `call` keeps turning a status of 300 or more into its `Str` error; its tests keep passing with no cluster).
- Create: `examples/k8s/Dockerfile` (stage 1 builds Limen from the repository and the operator natively; stage 2 `debian:trixie-slim` with `libssl3` and the binary), `examples/k8s/operator.yaml` (Namespace `limen-system`, ServiceAccount, ClusterRole with `get,list,watch` on `staticsites`, `patch,update` on `staticsites/status`, `get,list,watch,create,patch,delete` on `configmaps`, `deployments`, `services`; ClusterRoleBinding; Deployment running the binary with `--grant net=$(KUBERNETES_SERVICE_HOST):$(KUBERNETES_SERVICE_PORT) --grant fs=/var/run/secrets/kubernetes.io/serviceaccount:ro --grant env=KUBERNETES_SERVICE_HOST --grant env=KUBERNETES_SERVICE_PORT`), `test/integration/kind.sh`.
- Modify: the comment at the top of `site_operator.lmn` (both ways to run it).

**Interfaces:**
- Consumes: Task 4's `kube` API.

- [ ] **Step 1:** `limen test examples/site_operator.lmn` passes (its tests through `kube.proxy` and a fake).
- [ ] **Step 2:** Write `kind.sh`: create cluster `limen-kind`, `docker build -f examples/k8s/Dockerfile -t limen-site-operator:dev .`, `kind load docker-image`, apply the CRD, `operator.yaml`, `hello-site.yaml`; wait up to 120 s for ConfigMap `hello-html`, Deployment `hello` and Service `hello`, and for `status.message` on the site; check that no `kubectl proxy` process runs; print `kind: the operator reconciled hello without a proxy`; delete the cluster on exit.
- [ ] **Step 3:** Run `sh test/integration/kind.sh` (needs Docker and kind): it prints the line above. Ask the user before installing kind if it is missing.
- [ ] **Step 4:** Commit `site_operator runs in the cluster through kube, with an image, RBAC and a kind check`.

### Task 6: Docs and the full gate

**Files:** `ROADMAP.md` (the Kubernetes section: TLS trust and in-cluster auth done, with the kind check), the book's chapter 14 (a few paragraphs on `trusting`, `presenting` and `kube`, in `../limen-book`, commit there, never push).

- [ ] **Step 1:** ROADMAP and book updated.
- [ ] **Step 2:** `dune build --root . && dune exec --root . ./test/test_limen.exe && sh selfhost/check.sh && sh selfhost/bootstrap.sh`: all pass, fixed point.
- [ ] **Step 3:** Linux arm64 in Docker, in parts (`LIMEN_TEST_ONLY` groups); only the known `tls-conn/tsan` flake may fail.
- [ ] **Step 4:** Commit `ROADMAP: an operator in the cluster without a proxy` (and the book's commit).
