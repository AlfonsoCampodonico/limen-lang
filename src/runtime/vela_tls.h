/* Vela TLS core, on OpenSSL (1.1.1 or later; 3.x on macOS from Homebrew,
   libssl on Linux). Shared by both backends so they behave and fail alike:
   the native compiler pastes it into programs that use TLS, ahead of
   vela_rt.h, and the interpreter's C stub (src/tls_stubs.c) includes it.

   Everything here is non-blocking: a step either makes progress or says
   which way the socket must become ready (VTLS_WANT_READ / VTLS_WANT_WRITE),
   and the caller parks the task on its poller and tries again. The socket is
   always non-blocking, so no call here can hold up a thread.

   Clients verify the server's certificate chain against the default trust
   store (SSL_CERT_FILE / SSL_CERT_DIR when set, as for every OpenSSL
   program, else the store OpenSSL was built with) and its name or IP
   address against the host that was dialled. Servers do not ask for client
   certificates. TLS 1.2 is the minimum both ways. */
#include <openssl/ssl.h>
#include <openssl/err.h>
#include <openssl/pem.h>
#include <openssl/x509v3.h>
#include <pthread.h>
#include <stdint.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <arpa/inet.h>

/* One connection. The lock is held only around a single non-blocking
   OpenSSL call, never while a task waits, so a reader and a writer of the
   same connection can use it from different threads. */
typedef struct VTls { SSL *ssl; pthread_mutex_t mu; int server; } VTls;

enum { VTLS_READ, VTLS_WRITE, VTLS_HANDSHAKE };
#define VTLS_WANT_READ (-1)
#define VTLS_WANT_WRITE (-2)
#define VTLS_ERROR (-3)

static SSL_CTX *vtls_client_ctx;
static char vtls_client_err[256];
static pthread_once_t vtls_once = PTHREAD_ONCE_INIT;

/* "tls: WHAT: reason" from OpenSSL's error queue (or just "tls: WHAT"). */
static void vtls_queue_err(char *err, size_t en, const char *what) {
  unsigned long e = ERR_peek_last_error();
  const char *r = e ? ERR_reason_error_string(e) : NULL;
  if (r) snprintf(err, en, "tls: %s: %s", what, r);
  else snprintf(err, en, "tls: %s", what);
  ERR_clear_error();
}

static void vtls_common(SSL_CTX *c) {
  SSL_CTX_set_min_proto_version(c, TLS1_2_VERSION);
  /* writes may finish part of the data; a retried write may come from a
     moved buffer (the interpreter's strings can move) */
  SSL_CTX_set_mode(c, SSL_MODE_ENABLE_PARTIAL_WRITE | SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER);
  long opts = SSL_OP_NO_COMPRESSION;
#ifdef SSL_OP_NO_RENEGOTIATION
  opts |= SSL_OP_NO_RENEGOTIATION;
#endif
#ifdef SSL_OP_IGNORE_UNEXPECTED_EOF
  /* many servers close without close_notify; HTTP frames its own bodies */
  opts |= SSL_OP_IGNORE_UNEXPECTED_EOF;
#endif
  SSL_CTX_set_options(c, opts);
}

static void vtls_client_init(void) {
  SSL_CTX *c = SSL_CTX_new(TLS_client_method());
  if (!c) { vtls_queue_err(vtls_client_err, sizeof vtls_client_err, "cannot set up TLS"); return; }
  vtls_common(c);
  SSL_CTX_set_verify(c, SSL_VERIFY_PEER, NULL);
  if (SSL_CTX_set_default_verify_paths(c) != 1) {
    vtls_queue_err(vtls_client_err, sizeof vtls_client_err, "cannot load the trusted certificates");
    SSL_CTX_free(c);
    return;
  }
  ERR_clear_error();
  vtls_client_ctx = c;
}

static VTls *vtls_new(SSL_CTX *ctx, int fd, int server, char *err, size_t en) {
  SSL *s = SSL_new(ctx);
  if (!s) { vtls_queue_err(err, en, "cannot start a session"); return NULL; }
  if (SSL_set_fd(s, fd) != 1) { SSL_free(s); vtls_queue_err(err, en, "cannot start a session"); return NULL; }
  VTls *t = (VTls *)calloc(1, sizeof *t);
  if (!t) { SSL_free(s); snprintf(err, en, "tls: out of memory"); return NULL; }
  t->ssl = s; t->server = server;
  pthread_mutex_init(&t->mu, NULL);
  if (server) SSL_set_accept_state(s); else SSL_set_connect_state(s);
  return t;
}

/* A client session on connected socket fd that will check the certificate
   against host: a DNS name (also sent as SNI) or an IP address. */
static VTls *vtls_client(int fd, const char *host, char *err, size_t en) {
  pthread_once(&vtls_once, vtls_client_init);
  if (!vtls_client_ctx) { snprintf(err, en, "%s", vtls_client_err); return NULL; }
  VTls *t = vtls_new(vtls_client_ctx, fd, 0, err, en);
  if (!t) return NULL;
  struct in_addr a4; struct in6_addr a6;
  int ok;
  if (inet_pton(AF_INET, host, &a4) == 1 || inet_pton(AF_INET6, host, &a6) == 1)
    ok = X509_VERIFY_PARAM_set1_ip_asc(SSL_get0_param(t->ssl), host) == 1;
  else {
    SSL_set_hostflags(t->ssl, X509_CHECK_FLAG_NO_PARTIAL_WILDCARDS);
    ok = SSL_set_tlsext_host_name(t->ssl, host) == 1 && SSL_set1_host(t->ssl, host) == 1;
  }
  if (!ok) {
    vtls_queue_err(err, en, "cannot set the host to verify");
    SSL_free(t->ssl); pthread_mutex_destroy(&t->mu); free(t);
    return NULL;
  }
  return t;
}

/* PEM files are never encrypted here: no prompt for a passphrase */
static int vtls_no_pass(char *buf, int size, int rw, void *u) { (void)buf; (void)size; (void)rw; (void)u; return 0; }

/* A server context from PEM text: the certificate (then any intermediate
   certificates of its chain) and its private key. NULL with err set if they
   cannot be used. */
static SSL_CTX *vtls_server_ctx(const char *cert, size_t cl, const char *key, size_t kl, char *err, size_t en) {
  ERR_clear_error();
  if (cl > INT32_MAX || kl > INT32_MAX) { snprintf(err, en, "tls: the certificate or key is too large"); return NULL; }
  SSL_CTX *c = SSL_CTX_new(TLS_server_method());
  if (!c) { vtls_queue_err(err, en, "cannot set up TLS"); return NULL; }
  vtls_common(c);
  BIO *b = BIO_new_mem_buf(cert, (int)cl);
  X509 *x = b ? PEM_read_bio_X509(b, NULL, vtls_no_pass, NULL) : NULL;
  if (!x) { BIO_free(b); SSL_CTX_free(c); ERR_clear_error(); snprintf(err, en, "tls: no PEM certificate in the certificate text"); return NULL; }
  if (SSL_CTX_use_certificate(c, x) != 1) { X509_free(x); BIO_free(b); SSL_CTX_free(c); vtls_queue_err(err, en, "cannot use the certificate"); return NULL; }
  X509_free(x);
  while ((x = PEM_read_bio_X509(b, NULL, vtls_no_pass, NULL)) != NULL)
    if (SSL_CTX_add_extra_chain_cert(c, x) != 1) { X509_free(x); break; }   /* takes x on success */
  ERR_clear_error();   /* the end of the text reads as an error */
  BIO_free(b);
  b = BIO_new_mem_buf(key, (int)kl);
  EVP_PKEY *k = b ? PEM_read_bio_PrivateKey(b, NULL, vtls_no_pass, NULL) : NULL;
  BIO_free(b);
  if (!k) { SSL_CTX_free(c); ERR_clear_error(); snprintf(err, en, "tls: no unencrypted PEM private key in the key text"); return NULL; }
  int used = SSL_CTX_use_PrivateKey(c, k);
  EVP_PKEY_free(k);
  if (used != 1 || SSL_CTX_check_private_key(c) != 1) {
    SSL_CTX_free(c); ERR_clear_error();
    snprintf(err, en, "tls: the private key does not match the certificate");
    return NULL;
  }
  return c;
}

static void vtls_fail(VTls *t, int e, char *err, size_t en) {
  if (e == SSL_ERROR_SSL && !t->server) {
    long v = SSL_get_verify_result(t->ssl);
    if (v != X509_V_OK) {
      snprintf(err, en, "tls: certificate verify failed: %s", X509_verify_cert_error_string(v));
      ERR_clear_error();
      return;
    }
  }
  if (e == SSL_ERROR_SSL) { vtls_queue_err(err, en, "handshake or record error"); return; }
  if (e == SSL_ERROR_SYSCALL) {
    if (errno != 0) snprintf(err, en, "tls: %s", strerror(errno));
    else snprintf(err, en, "tls: the connection was closed during the handshake");
    ERR_clear_error();
    return;
  }
  snprintf(err, en, "tls: error %d", e);
  ERR_clear_error();
}

/* One non-blocking step. VTLS_READ returns the bytes read, 0 at the end of
   the stream; VTLS_WRITE the bytes written (at least 1); VTLS_HANDSHAKE 0
   once the handshake is done. Otherwise VTLS_WANT_READ / VTLS_WANT_WRITE
   (wait for the socket, then repeat the same call) or VTLS_ERROR with a
   message in err. */
static int vtls_step(VTls *t, int op, void *buf, int n, char *err, size_t en) {
  pthread_mutex_lock(&t->mu);
  ERR_clear_error();
  errno = 0;
  int r = op == VTLS_READ ? SSL_read(t->ssl, buf, n) : op == VTLS_WRITE ? SSL_write(t->ssl, buf, n) : SSL_do_handshake(t->ssl);
  int res;
  if (r > 0) res = op == VTLS_HANDSHAKE ? 0 : r;
  else {
    int e = SSL_get_error(t->ssl, r);
    if (e == SSL_ERROR_WANT_READ) res = VTLS_WANT_READ;
    else if (e == SSL_ERROR_WANT_WRITE) res = VTLS_WANT_WRITE;
    else if (e == SSL_ERROR_ZERO_RETURN && op == VTLS_READ) res = 0;
    else if (e == SSL_ERROR_ZERO_RETURN) {
      snprintf(err, en, op == VTLS_HANDSHAKE ? "tls: the connection was closed during the handshake" : "tls: the peer closed the connection");
      res = VTLS_ERROR;
    }
    else { vtls_fail(t, e, err, en); res = VTLS_ERROR; }
  }
  pthread_mutex_unlock(&t->mu);
  return res;
}

/* Tell the peer we are done (close_notify), without waiting for its reply. */
static void vtls_close_notify(VTls *t) {
  pthread_mutex_lock(&t->mu);
  if (SSL_is_init_finished(t->ssl)) { ERR_clear_error(); SSL_shutdown(t->ssl); }
  ERR_clear_error();
  pthread_mutex_unlock(&t->mu);
}

static void vtls_free(VTls *t) {
  if (!t) return;
  SSL_free(t->ssl);
  pthread_mutex_destroy(&t->mu);
  free(t);
}
