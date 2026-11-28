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
/* a trusting or presenting Net's client context: one reference, dropped by the GC */
#define Trust_val(v) (*((VTlsClient **)Data_custom_val(v)))

static void tls_finalize(value v) { vtls_free(Tls_val(v)); Tls_val(v) = NULL; }
static void ctx_finalize(value v) { if (Ctx_val(v)) SSL_CTX_free(Ctx_val(v)); Ctx_val(v) = NULL; }
static void trust_finalize(value v) { vtls_client_drop(Trust_val(v)); Trust_val(v) = NULL; }
static struct custom_operations tls_ops = { "limen.tls", tls_finalize, custom_compare_default, custom_hash_default,
                                            custom_serialize_default, custom_deserialize_default,
                                            custom_compare_ext_default, custom_fixed_length_default };
static struct custom_operations ctx_ops = { "limen.tls_ctx", ctx_finalize, custom_compare_default, custom_hash_default,
                                            custom_serialize_default, custom_deserialize_default,
                                            custom_compare_ext_default, custom_fixed_length_default };

static struct custom_operations trust_ops = { "limen.tls_trust", trust_finalize, custom_compare_default, custom_hash_default,
                                              custom_serialize_default, custom_deserialize_default,
                                              custom_compare_ext_default, custom_fixed_length_default };

static value wrap_tls(VTls *t) {
  value v = caml_alloc_custom(&tls_ops, sizeof(VTls *), 0, 1);
  Tls_val(v) = t;
  return v;
}

/* client_with : trust option -> Unix.file_descr -> string -> t   (raises Failure msg) */
value limen_tls_client(value with, value fd, value host) {
  CAMLparam3(with, fd, host);
  char err[300];
  VTlsClient *w = Is_block(with) ? Trust_val(Field(with, 0)) : NULL;
  VTls *t = vtls_client(w, Int_val(fd), String_val(host), err, sizeof err);
  if (!t) caml_failwith(err);
  CAMLreturn(wrap_tls(t));
}

/* The C memory behind a context, for caml_alloc_custom_mem, so the GC runs
   sooner when many are dropped: an SSL_CTX with its tables is some tens of
   kilobytes, plus about the size of the PEM text it was made from. */
#define VTLS_CTX_MEM 32768

static value wrap_trust(VTlsClient *c, size_t text) {
  value v = caml_alloc_custom_mem(&trust_ops, sizeof(VTlsClient *), VTLS_CTX_MEM + text);
  Trust_val(v) = c;
  return v;
}

/* trust_raw : trust option -> string -> trust   (raises Failure msg): the
   context from's (None: the default) with the trust replaced */
value limen_tls_trust(value from, value pem) {
  CAMLparam2(from, pem);
  char err[300];
  VTlsClient *f = Is_block(from) ? Trust_val(Field(from, 0)) : NULL;
  VTlsClient *c = vtls_trusting(f, String_val(pem), caml_string_length(pem), err, sizeof err);
  if (!c) caml_failwith(err);
  CAMLreturn(wrap_trust(c, caml_string_length(pem)));
}

/* present_raw : trust option -> string -> string -> trust   (raises
   Failure msg): the context from's with a certificate and key to present */
value limen_tls_present(value from, value cert, value key) {
  CAMLparam3(from, cert, key);
  char err[300];
  VTlsClient *f = Is_block(from) ? Trust_val(Field(from, 0)) : NULL;
  VTlsClient *c = vtls_presenting(f, String_val(cert), caml_string_length(cert), String_val(key), caml_string_length(key), err, sizeof err);
  if (!c) caml_failwith(err);
  CAMLreturn(wrap_trust(c, caml_string_length(cert) + caml_string_length(key)));
}

/* server_ctx : string -> string -> ctx   (raises Failure msg) */
value limen_tls_server_ctx(value cert, value key) {
  CAMLparam2(cert, key);
  CAMLlocal1(v);
  char err[300];
  SSL_CTX *c = vtls_server_ctx(String_val(cert), caml_string_length(cert), String_val(key), caml_string_length(key), err, sizeof err);
  if (!c) caml_failwith(err);
  v = caml_alloc_custom_mem(&ctx_ops, sizeof(SSL_CTX *), VTLS_CTX_MEM + caml_string_length(cert) + caml_string_length(key));
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

/* crypto : int -> string -> string -> string -> string -> string
   the result after a status byte, as w_crypto gives it */
value limen_crypto_op(value op, value a, value b, value c, value d) {
  CAMLparam5(op, a, b, c, d);
  CAMLlocal1(r);
  unsigned char *out = NULL; size_t n = 0; char err[300];
  int k = vcrypto(Int_val(op), (const unsigned char *)String_val(a), caml_string_length(a), (const unsigned char *)String_val(b), caml_string_length(b),
                  (const unsigned char *)String_val(c), caml_string_length(c), (const unsigned char *)String_val(d), caml_string_length(d), &out, &n, err, sizeof err);
  if (k != 0) {
    size_t m = strlen(err);
    r = caml_alloc_string(m + 1);
    Bytes_val(r)[0] = 1;
    memcpy(Bytes_val(r) + 1, err, m);
    CAMLreturn(r);
  }
  r = caml_alloc_string(n + 1);
  Bytes_val(r)[0] = 0;
  if (n) memcpy(Bytes_val(r) + 1, out, n);
  OPENSSL_cleanse(out, n);
  free(out);
  CAMLreturn(r);
}

#else

value limen_tls_client(value w, value fd, value host) { (void)w; (void)fd; (void)host; caml_failwith(NO_TLS); }
value limen_tls_trust(value f, value pem) { (void)f; (void)pem; caml_failwith(NO_TLS); }
value limen_tls_present(value f, value c, value k) { (void)f; (void)c; (void)k; caml_failwith(NO_TLS); }
value limen_tls_server_ctx(value c, value k) { (void)c; (void)k; caml_failwith(NO_TLS); }
value limen_tls_server(value c, value fd) { (void)c; (void)fd; caml_failwith(NO_TLS); }
value limen_tls_step(value t, value op, value b, value o, value l) { (void)t; (void)op; (void)b; (void)o; (void)l; caml_failwith(NO_TLS); }
value limen_tls_close(value t) { (void)t; return Val_unit; }
value limen_tls_ctx_free(value c) { (void)c; return Val_unit; }
value limen_tls_available(value u) { (void)u; return Val_false; }
value limen_crypto_op(value op, value a, value b, value c, value d) {
  (void)op; (void)a; (void)b; (void)c; (void)d;
  value r = caml_alloc_string(1 + strlen(NO_TLS));
  Bytes_val(r)[0] = 1;
  memcpy(Bytes_val(r) + 1, NO_TLS, strlen(NO_TLS));
  return r;
}

#endif
