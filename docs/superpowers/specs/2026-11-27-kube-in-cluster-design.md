# An operator in the cluster, without a proxy: design

## Intent

`examples/site_operator.lmn` is a working Kubernetes operator, but it
reaches the API server through `kubectl proxy`, which holds the
credentials. The goal is an operator that runs as a Deployment in a real
cluster and talks to the API server itself, with one grant for the API
server and a read-only grant for its service-account files, and
`limen caps` showing it can reach nothing more. This is the first piece
of the ROADMAP's "Kubernetes without a proxy" section; YAML, kubeconfig,
typed objects and gRPC come after it.

What stops it today: Limen's TLS trusts only the system's certificate
store (SPEC.md section 10, "TLS"), and an API server's certificate is
signed by the cluster's own CA. Reading the token and sending
`Authorization: Bearer` already works with `Fs`, `Env` and `http`.

## Goals

- A `Net` can be told which CA its TLS connections trust, and which
  client certificate they present, from PEM text the program read
  through authority it was given.
- A `kube` standard-library module that turns a pod's service-account
  files into a client for the API server, covering what
  `site_operator.lmn` hand-writes today: get and list, server-side apply,
  status updates, and watches.
- `site_operator.lmn` runs in a kind cluster as a Deployment, with RBAC
  and a container image, through `kube`, and still runs through
  `kubectl proxy` for development.

## Non-goals

- Kubeconfig files (they are YAML; they come with the `yaml` module).
- Typed Kubernetes objects, gRPC, protobuf.
- Exec credential plugins, OIDC, cloud-provider auth.
- A switch to turn certificate verification off. There still is none.

## Anti-goals

- Ambient trust: nothing read from the environment (`SSL_CERT_FILE` and
  the like) changes what a `Net` built with `trusting` accepts.
- Trust that leaks: a `Net` that trusts the cluster CA must not change
  what any other `Net` in the program trusts.
- A second HTTP stack: `http`, `postgres` and everything else built on
  `Net` work with a trusting `Net` unchanged.

## Approaches

Considered, with the user picking A:

- **A. Trust in the program, from granted authority (chosen).** The
  program reads the CA (and a client certificate) through `Fs` and hands
  it to the runtime. Fits the capability model: what a program trusts
  comes from authority it was given and is visible in its code. Costs a
  runtime change in both backends.
- B. A trust grant on the command line (`--grant tls-ca=PATH`). No
  program API, but programs can't choose their trust, client
  certificates fit badly, and it is a new kind of grant.
- C. `SSL_CERT_FILE` pointing at the cluster CA. Works today, but
  replaces the trust store for the whole process and is ambient. Useful
  only as a spike.

Within A, the trust lives on the `Net` capability rather than in a
separate value passed to every call. This is the builder's refinement,
shown to the user: `net.only(...)` already narrows a `Net`, and
`trusting` follows the same shape, so nothing above `Net` changes.

## Design

### `Net.trusting` and `Net.presenting`

```limen
let api = root.net.only("10.96.0.1:443")
let api = api.trusting(ca_pem)?                  // Result[Net, IoError]
let api = api.presenting(cert_pem, key_pem)?     // Result[Net, IoError]
```

- `net.trusting(ca: Str) -> Result[Net, IoError]`: a `Net` with the same
  allowlist whose TLS connections (`connect_tls`, `https` in `get` and
  `http`, `start_tls` on its connections) verify the server against the
  certificates in `ca` only, in place of the system store. `ca` is PEM:
  one or more `CERTIFICATE` blocks. Text that holds no certificate, or a
  block that does not parse, is `Err(Failed("tls: ..."))` at `trusting`,
  never at the first connection. Name checking is unchanged: the server's
  certificate must match the host or IP address connected to.
- `net.presenting(cert: Str, key: Str) -> Result[Net, IoError]`: a `Net`
  whose TLS connections present `cert` (PEM, the leaf first, then any
  intermediates) with private key `key` (PEM, unencrypted) when the
  server asks for a client certificate. A key that does not match the
  certificate is an error at `presenting`.
- Both keep everything else the `Net` had: its allowlist, its revocation
  (revoking the parent revokes the derived `Net`, as for `only`), and its
  trust or identity when only the other is set. Calling `trusting` again
  replaces the trust; it does not add to it.
- `only` on a trusting `Net` keeps the trust.
- They are not effects (no waiting, no I/O): they parse text.
- `limen caps` and the checker treat them as `only` is treated: a
  function that is given a `Net` and calls `trusting` still needs `Net`.

### Runtime

- One OpenSSL client context per distinct trust and identity, built when
  `trusting` or `presenting` is called, and shared by reference count by
  the `Net` values that hold it and by connections opened through them.
  The default context (the system store) stays as it is.
- The native runtime (`src/runtime/limen_tls.h`, `limen_rt.c`) and the
  interpreter (`src/tls_stubs.c` through `src/interp.ml`'s `CNet`) use the
  same C code for parsing and the context, so they accept and refuse the
  same PEM with the same messages.
- In the WebAssembly build both refuse, as `connect_tls` does.

### The `kube` module (`src/std/kube.lmn`)

```limen
use kube

let k = kube.in_cluster(root.fs.under("/var/run/secrets/kubernetes.io/serviceaccount").read_only(), root.env, root.net)?
let sites = k.list("/apis/limen.dev/v1/staticsites")?
k.apply("/api/v1/namespaces/default/configmaps/x", "limen-operator", body)?
k.watch("/apis/limen.dev/v1/staticsites", version, |line| ...)?
```

- `kube.in_cluster(fs, env, net) -> Result[Client, IoError]`: reads
  `token`, `ca.crt` and `namespace` through `fs`, the API server's
  address from `KUBERNETES_SERVICE_HOST` and `KUBERNETES_SERVICE_PORT`
  through `env`, and narrows `net` to that address and the CA.
- `kube.proxy(net, base) -> Client`: the same client over a plain
  `kubectl proxy` URL, no token, for development and the tests.
- `Client` methods, each sending `Accept: application/json` and the
  bearer token: `get(path)`, `list(path)`, `apply(path, manager, body)`
  (server-side apply, `application/apply-patch+yaml` with a JSON body,
  `force=true`), `patch_status(path, body)` (merge patch on `/status`),
  `delete(path)`, and `watch(path, resource_version, f)`, which passes
  each event line to `f` as `http.request_lines` does.
- Results (builder's call): every answer from the API server is
  `Ok((status code, json.Value))`, error statuses included, with the
  server's `kind: Status` body as the value, so a caller can act on a 404
  or a 409; `Err(IoError)` is for what never got an answer (connection,
  TLS, a body that isn't JSON). `site_operator.lmn` keeps its own
  wrapper that turns a status of 300 or more into its `Str` error, as its
  `call` does today.
- The token (builder's call, so `in_cluster` needs no `Clock`): before
  each request the client checks the token file's `modified_ms` with
  `Fs.info` and reads it again when it changed (kubelet rewrites it as
  bound tokens rotate, about hourly); after a 401 it reads it again and
  retries the request once before returning the 401.

### The example

- `site_operator.lmn` uses `kube.in_cluster` when
  `KUBERNETES_SERVICE_HOST` is set and `kube.proxy` otherwise; its
  existing logic moves onto `Client`.
- `examples/k8s/` gains a Dockerfile building the operator natively, a
  ServiceAccount, ClusterRole and binding with only the verbs it uses,
  and a Deployment running it with
  `--grant net=$(KUBERNETES_SERVICE_HOST):$(KUBERNETES_SERVICE_PORT)`,
  `--grant fs=/var/run/secrets/kubernetes.io/serviceaccount:ro` and the
  two `env` grants.
- The image (builder's call) builds Limen from this repository in a
  first stage, since the features are not released yet, and copies only
  the built operator into a slim runtime stage.
- The check that this is done (builder's call): `test/integration/kind.sh`
  creates a kind cluster, loads the image, applies the CRD, RBAC and
  Deployment, applies `hello-site.yaml`, waits for the ConfigMap,
  Deployment, Service and status to appear with no `kubectl proxy`
  running, and deletes the cluster. Run by hand like
  `test/integration/databases.sh`, not part of the gate.

### Tests

Fixtures (builder's call): a new test CA whose key is kept in the test
directory, with a server certificate and a client certificate it signs,
since the suite's existing CA key was discarded and the suite sets
`SSL_CERT_FILE` to that CA for the whole process. Limen's own TLS server
still never asks for client certificates; the `presenting` tests use
`openssl s_server -Verify` (the `openssl` tool is in the Linux test
image) and are skipped where it is missing.

- TLS against a local server with its own CA (the test suite already
  makes certificates): connecting with the system trust fails, with
  `trusting(ca)` succeeds, with `trusting(other_ca)` fails; a server that
  requires a client certificate refuses a `Net` without `presenting` and
  accepts one with it; bad PEM and a mismatched key fail at the call.
  Both backends, same messages.
- `trusting` does not change another `Net`'s trust.
- `kube` against a fake API server in the test suite (no cluster):
  token header, the 401 re-read, apply and watch.

## Constraints

- Every change goes into both compilers (`src/` and `selfhost/`) and both
  backends; SPEC.md describes it in the same commit; the gate (`dune
  test`, `selfhost/check.sh`, the bootstrap, Linux) passes.
- The runtime still needs only a C compiler, libc and OpenSSL.
- The plan's commit rules: plain messages, no attribution, British
  spelling, no em dashes.

## Left to the builder

- How the interpreter's `CNet` carries the trust (a record in place of
  the host list, or a handle into the C side).
- The exact error texts, in the style of the existing `tls: ` errors.
- How `kube` splits `site_operator.lmn`'s helpers between the module and
  the example.
