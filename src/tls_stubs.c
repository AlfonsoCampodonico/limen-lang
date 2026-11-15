/* The interpreter's TLS: thin OCaml bindings over the core the native
   runtime uses (runtime/limen_tls.h), so both backends verify, fail and
   word their errors the same way. Without LIMEN_TLS (OpenSSL not found at
   build time) every entry point fails with the same message the native
   runtime gives. */
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/custom.h>
#include <string.h>

#define NO_TLS "tls: limen was built without TLS (OpenSSL was not found when it was compiled)"

#ifdef LIMEN_TLS
#include "runtime/limen_tls.h"

/* A session or a server context in a custom block; freed at most once,
   by close/free or else by the GC. */
#define Tls_val(v) (*((VTls **)Data_custom_val(v)))
#define Ctx_val(v) (*((SSL_CTX **)Data_custom_val(v)))

static void tls_finalize(value v) { vtls_free(Tls_val(v)); Tls_val(v) = NULL; }
static void ctx_finalize(value v) { if (Ctx_val(v)) SSL_CTX_free(Ctx_val(v)); Ctx_val(v) = NULL; }
static struct custom_operations tls_ops = { "limen.tls", tls_finalize, custom_compare_default, custom_hash_default,
                                            custom_serialize_default, custom_deserialize_default,
                                            custom_compare_ext_default, custom_fixed_length_default };
static struct custom_operations ctx_ops = { "limen.tls_ctx", ctx_finalize, custom_compare_default, custom_hash_default,
                                            custom_serialize_default, custom_deserialize_default,
                                            custom_compare_ext_default, custom_fixed_length_default };

static value wrap_tls(VTls *t) {
  value v = caml_alloc_custom(&tls_ops, sizeof(VTls *), 0, 1);
  Tls_val(v) = t;
  return v;
}

/* client : Unix.file_descr -> string -> t   (raises Failure msg) */
value limen_tls_client(value fd, value host) {
  CAMLparam2(fd, host);
  char err[300];
  VTls *t = vtls_client(Int_val(fd), String_val(host), err, sizeof err);
  if (!t) caml_failwith(err);
  CAMLreturn(wrap_tls(t));
}

/* server_ctx : string -> string -> ctx   (raises Failure msg) */
value limen_tls_server_ctx(value cert, value key) {
  CAMLparam2(cert, key);
  CAMLlocal1(v);
  char err[300];
  SSL_CTX *c = vtls_server_ctx(String_val(cert), caml_string_length(cert), String_val(key), caml_string_length(key), err, sizeof err);
  if (!c) caml_failwith(err);
  v = caml_alloc_custom(&ctx_ops, sizeof(SSL_CTX *), 0, 1);
  Ctx_val(v) = c;
  CAMLreturn(v);
}

/* server : ctx -> Unix.file_descr -> t   (raises Failure msg) */
value limen_tls_server(value ctx, value fd) {
  CAMLparam2(ctx, fd);
  char err[300];
  if (!Ctx_val(ctx)) caml_failwith("tls: the listener is closed");
  VTls *t = vtls_new(Ctx_val(ctx), Int_val(fd), 1, err, sizeof err);
  if (!t) caml_failwith(err);
  CAMLreturn(wrap_tls(t));
}

/* step : t -> int -> bytes -> int -> int -> int
   op 0 read into buf[off..off+len), 1 write from it, 2 handshake. Returns
   bytes (or 0), -1 want read, -2 want write; raises Failure on an error.
   The call never blocks, so the OCaml runtime lock stays held and buf
   cannot move during it. */
value limen_tls_step(value t, value op, value buf, value off, value len) {
  CAMLparam5(t, op, buf, off, len);
  char err[300];
  VTls *s = Tls_val(t);
  if (!s) caml_failwith("tls: the connection is closed");
  int k = vtls_step(s, Int_val(op), (char *)Bytes_val(buf) + Long_val(off), (int)Long_val(len), err, sizeof err);
  if (k == VTLS_ERROR) caml_failwith(err);
  CAMLreturn(Val_int(k));
}

/* close : t -> unit   sends close_notify and frees the session */
value limen_tls_close(value t) {
  CAMLparam1(t);
  VTls *s = Tls_val(t);
  if (s) { vtls_close_notify(s); vtls_free(s); Tls_val(t) = NULL; }
  CAMLreturn(Val_unit);
}

value limen_tls_ctx_free(value c) {
  CAMLparam1(c);
  if (Ctx_val(c)) { SSL_CTX_free(Ctx_val(c)); Ctx_val(c) = NULL; }
  CAMLreturn(Val_unit);
}

value limen_tls_available(value u) { (void)u; return Val_true; }

#else

value limen_tls_client(value fd, value host) { (void)fd; (void)host; caml_failwith(NO_TLS); }
value limen_tls_server_ctx(value c, value k) { (void)c; (void)k; caml_failwith(NO_TLS); }
value limen_tls_server(value c, value fd) { (void)c; (void)fd; caml_failwith(NO_TLS); }
value limen_tls_step(value t, value op, value b, value o, value l) { (void)t; (void)op; (void)b; (void)o; (void)l; caml_failwith(NO_TLS); }
value limen_tls_close(value t) { (void)t; return Val_unit; }
value limen_tls_ctx_free(value c) { (void)c; return Val_unit; }
value limen_tls_available(value u) { (void)u; return Val_false; }

#endif
