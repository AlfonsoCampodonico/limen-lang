/* Limen TLS core, on OpenSSL (1.1.1 or later; 3.x on macOS from Homebrew,
   libssl on Linux). Shared by both backends so they behave and fail alike:
   the native compiler pastes it into programs that use TLS, ahead of
   limen_rt.h, and the interpreter's C stub (src/tls_stubs.c) includes it.

   Everything here is non-blocking: a step either makes progress or says
   which way the socket must become ready (VTLS_WANT_READ / VTLS_WANT_WRITE),
   and the caller parks the task on its poller and tries again. The socket is
   always non-blocking, so no call here can hold up a thread.

   Clients verify the server's certificate chain against the default trust
   store (SSL_CERT_FILE / SSL_CERT_DIR when set, as for every OpenSSL
   program, else the store OpenSSL was built with, else the system's own
   bundle when that store isn't there: a statically linked limen on a Mac
   without Homebrew), or, through a Net made with `trusting`, against the
   certificates it was given only (a VTlsClient), and its name or IP
   address against the host that was dialled. Through a Net made with
   `presenting`, a client shows a certificate when the server asks for one.
   Servers do not ask for client certificates. TLS 1.2 is the minimum both
   ways. */
#include <limits.h>
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
#include <unistd.h>
#include <arpa/inet.h>

/* One connection. The lock is held only around a single non-blocking
   OpenSSL call, never while a task waits, so a reader and a writer of the
   same connection can use it from different threads. */
typedef struct VTls { SSL *ssl; pthread_mutex_t mu; int server; } VTls;

enum { VTLS_READ, VTLS_WRITE, VTLS_HANDSHAKE };
#define VTLS_WANT_READ (-1)
#define VTLS_WANT_WRITE (-2)
#define VTLS_ERROR (-3)

/* A stream that ends without the peer's close message (close_notify) may
   have been cut short by an attacker, so it is an error with this exact
   message; Net.get accepts it when the response's own framing shows it is
   complete. The interpreter has the same text (Caps.tls_cut). */
#define VTLS_CUT_MSG "tls: the connection was closed without a TLS close message, so the data may be cut short"

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
  SSL_CTX_set_options(c, opts);
}

/* What every client context does: verify the server. */
static void vtls_client_common(SSL_CTX *c) {
  vtls_common(c);
  SSL_CTX_set_verify(c, SSL_VERIFY_PEER, NULL);
}

static void vtls_client_init(void) {
  SSL_CTX *c = SSL_CTX_new(TLS_client_method());
  if (!c) { vtls_queue_err(vtls_client_err, sizeof vtls_client_err, "cannot set up TLS"); return; }
  vtls_client_common(c);
  if (SSL_CTX_set_default_verify_paths(c) != 1) {
    vtls_queue_err(vtls_client_err, sizeof vtls_client_err, "cannot load the trusted certificates");
    SSL_CTX_free(c);
    return;
  }
  /* The store OpenSSL was built with may not exist on this machine (a
     release binary linked against Homebrew's OpenSSL, run without
     Homebrew). Then use the system's bundle, unless the environment names
     a store. */
  if (!getenv("SSL_CERT_FILE") && !getenv("SSL_CERT_DIR") &&
      access(X509_get_default_cert_file(), R_OK) != 0 &&
      access(X509_get_default_cert_dir(), R_OK) != 0) {
    static const char *const bundles[] = {
      "/etc/ssl/cert.pem",                      /* macOS, Alpine, BSDs */
      "/etc/ssl/certs/ca-certificates.crt",     /* Debian, Ubuntu */
      "/etc/pki/tls/certs/ca-bundle.crt",       /* Fedora, RHEL */
    };
    for (size_t i = 0; i < sizeof bundles / sizeof *bundles; i++)
      if (access(bundles[i], R_OK) == 0 && SSL_CTX_load_verify_locations(c, bundles[i], NULL) == 1) break;
  }
  /* Work out the trusted certificates' extensions (key ids, usage) now,
     while no other thread can see them. OpenSSL otherwise fills them in on
     first use, under the certificate's lock with a lock-free acquire/release
     check in front. That is correct, but ThreadSanitizer cannot see the
     atomics inside an uninstrumented libcrypto, so two handshakes checking
     their chains against the same CA at once read as a race on the CA's key
     id. Certificates an SSL_CERT_DIR lookup loads later are still filled in
     lazily, which is just as safe. */
  STACK_OF(X509_OBJECT) *objs = X509_STORE_get0_objects(SSL_CTX_get_cert_store(c));
  for (int i = 0; i < sk_X509_OBJECT_num(objs); i++) {
    X509 *x = X509_OBJECT_get0_X509(sk_X509_OBJECT_value(objs, i));
    if (x) (void)X509_check_purpose(x, -1, 0);
  }
  ERR_clear_error();
  vtls_client_ctx = c;
}

/* A client context of a Net's own, in place of the default one: made by
   `trusting` (its own trust store, nothing from the environment) or
   `presenting` (a certificate and key to show a server that asks). Each
   call makes a new one from the parts of the one it was called on, so
   `trusting` keeps the identity and `presenting` keeps the trust. Shared by
   reference count between the Nets that hold it and the Conns opened
   through them (for start_tls); a session keeps the SSL_CTX itself (SSL_new
   counts it), so dropping the last holder never pulls it from under a live
   connection. */
typedef struct VTlsClient {
  int64_t rc;      /* holders, counted atomically */
  SSL_CTX *ctx;    /* verifies against `trust`, presents `cert` */
  X509_STORE *trust;   /* the certificates given to `trusting`, else the default context's store (ctx holds it too) */
  X509 *cert;          /* `presenting`: the leaf, or NULL */
  STACK_OF(X509) *chain;   /* its intermediates, or NULL */
  EVP_PKEY *key;       /* its private key, or NULL */
} VTlsClient;

static inline VTlsClient *vtls_client_dup(VTlsClient *c) {
  if (c) __atomic_fetch_add(&c->rc, 1, __ATOMIC_RELAXED);
  return c;
}
static void vtls_client_free(VTlsClient *c) {
  X509_STORE_free(c->trust);
  X509_free(c->cert);
  sk_X509_pop_free(c->chain, X509_free);
  EVP_PKEY_free(c->key);
  SSL_CTX_free(c->ctx);
  free(c);
}
/* 1 when that was the last holder and the context is gone */
static int vtls_client_drop(VTlsClient *c) {
  if (!c || __atomic_sub_fetch(&c->rc, 1, __ATOMIC_ACQ_REL) != 0) return 0;
  vtls_client_free(c);
  return 1;
}

/* PEM files are never encrypted here: no prompt for a passphrase */
static int vtls_no_pass(char *buf, int size, int rw, void *u) { (void)buf; (void)size; (void)rw; (void)u; return 0; }

/* The trust a new context starts from: from's, else the default context's
   store (the system's, as vtls_client_init loads it). NULL with err set
   when the default trust cannot be loaded. */
static X509_STORE *vtls_parent_trust(const VTlsClient *from, char *err, size_t en) {
  if (from) return from->trust;
  pthread_once(&vtls_once, vtls_client_init);
  if (!vtls_client_ctx) { snprintf(err, en, "%s", vtls_client_err); return NULL; }
  return SSL_CTX_get_cert_store(vtls_client_ctx);
}

/* A client context that verifies against trust and, when cert is set,
   presents cert, chain and key. It takes references of its own to all of
   them. NULL with err set on failure. */
static VTlsClient *vtls_client_make(X509_STORE *trust, X509 *cert, STACK_OF(X509) *chain, EVP_PKEY *key, char *err, size_t en) {
  SSL_CTX *c = SSL_CTX_new(TLS_client_method());
  VTlsClient *v = c ? (VTlsClient *)calloc(1, sizeof *v) : NULL;
  if (!v) { if (c) SSL_CTX_free(c); vtls_queue_err(err, en, "cannot set up TLS"); return NULL; }
  vtls_client_common(c);
  v->rc = 1; v->ctx = c;
  /* the context takes one reference to the store, the VTlsClient the other */
  X509_STORE_up_ref(trust); X509_STORE_up_ref(trust);
  SSL_CTX_set_cert_store(c, trust);
  v->trust = trust;
  if (cert) {
    X509_up_ref(cert); v->cert = cert;
    EVP_PKEY_up_ref(key); v->key = key;
    if (chain && !(v->chain = X509_chain_up_ref(chain))) { vtls_client_free(v); vtls_queue_err(err, en, "cannot set up TLS"); return NULL; }
    int used = SSL_CTX_use_certificate(c, cert) == 1 && (!chain || SSL_CTX_set1_chain(c, chain) == 1);
    if (!used) { vtls_client_free(v); vtls_queue_err(err, en, "cannot use the certificate"); return NULL; }
    if (SSL_CTX_use_PrivateKey(c, key) != 1 || SSL_CTX_check_private_key(c) != 1) {
      vtls_client_free(v); ERR_clear_error();
      snprintf(err, en, "tls: the private key does not match the certificate");
      return NULL;
    }
  }
  ERR_clear_error();
  return v;
}

/* Reading PEM text block by block: after the last block the read ends with
   "no start line"; any other error is a block that is cut short or does
   not decode. */
static int vtls_pem_bad(void) {
  unsigned long e = ERR_peek_last_error();
  return e && !(ERR_GET_LIB(e) == ERR_LIB_PEM && ERR_GET_REASON(e) == PEM_R_NO_START_LINE);
}

/* `net.trusting(ca)` on a Net whose context is from (NULL: the default): a
   client context that trusts the CERTIFICATE blocks in the PEM text ca and
   nothing else, and presents from's certificate, if any. Other blocks (a
   key, say) are passed over; text with no certificate, or a certificate
   block that does not parse, is an error here rather than at the first
   connection. NULL with err set on failure. */
static VTlsClient *vtls_trusting(const VTlsClient *from, const char *pem, size_t n, char *err, size_t en) {
  ERR_clear_error();
  if (n > INT32_MAX) { snprintf(err, en, "tls: the CA text is too large"); return NULL; }
  X509_STORE *st = X509_STORE_new();
  BIO *b = st ? BIO_new_mem_buf(pem, (int)n) : NULL;
  if (!b) { X509_STORE_free(st); snprintf(err, en, "tls: out of memory"); return NULL; }
  int count = 0, full = 0;
  X509 *x;
  while ((x = PEM_read_bio_X509(b, NULL, vtls_no_pass, NULL)) != NULL) {
    /* fill in the extensions now, as vtls_client_init does for its store */
    (void)X509_check_purpose(x, -1, 0);
    /* since OpenSSL 1.1.1 a certificate already in the store is accepted,
       so this fails only when memory runs out */
    if (X509_STORE_add_cert(st, x) != 1) full = 1;
    X509_free(x);
    count++;
    if (full) break;
  }
  int bad = !full && vtls_pem_bad();
  BIO_free(b);
  ERR_clear_error();
  if (full || bad || count == 0) {
    X509_STORE_free(st);
    snprintf(err, en, full ? "tls: out of memory"
                      : bad ? "tls: a PEM certificate in the CA text does not parse"
                      : "tls: no PEM certificate in the CA text");
    return NULL;
  }
  VTlsClient *v = vtls_client_make(st, from ? from->cert : NULL, from ? from->chain : NULL, from ? from->key : NULL, err, en);
  X509_STORE_free(st);   /* v holds its own references */
  return v;
}

/* `net.presenting(cert, key)` on a Net whose context is from (NULL: the
   default): a client context with from's trust that presents the
   certificate in cert (the leaf, then any intermediates of its chain) with
   the unencrypted private key in key when a server asks for one. Text with
   no certificate, a certificate block that does not parse, no usable key
   or a key that is not the certificate's is an error here. NULL with err
   set on failure. */
static VTlsClient *vtls_presenting(const VTlsClient *from, const char *cert, size_t cl, const char *key, size_t kl, char *err, size_t en) {
  ERR_clear_error();
  if (cl > INT32_MAX || kl > INT32_MAX) { snprintf(err, en, "tls: the certificate or key is too large"); return NULL; }
  X509_STORE *trust = vtls_parent_trust(from, err, en);
  if (!trust) return NULL;
  BIO *b = BIO_new_mem_buf(cert, (int)cl);
  STACK_OF(X509) *chain = sk_X509_new_null();
  if (!b || !chain) { BIO_free(b); sk_X509_free(chain); snprintf(err, en, "tls: out of memory"); return NULL; }
  X509 *leaf = NULL, *x;
  int full = 0;
  while ((x = PEM_read_bio_X509(b, NULL, vtls_no_pass, NULL)) != NULL) {
    (void)X509_check_purpose(x, -1, 0);
    if (!leaf) leaf = x;
    else if (!sk_X509_push(chain, x)) { X509_free(x); full = 1; break; }
  }
  int bad = !full && vtls_pem_bad();
  BIO_free(b);
  ERR_clear_error();
  const char *why = full ? "tls: out of memory"
                  : bad ? "tls: a PEM certificate in the certificate text does not parse"
                  : !leaf ? "tls: no PEM certificate in the certificate text" : NULL;
  EVP_PKEY *k = NULL;
  if (!why) {
    b = BIO_new_mem_buf(key, (int)kl);
    k = b ? PEM_read_bio_PrivateKey(b, NULL, vtls_no_pass, NULL) : NULL;
    BIO_free(b);
    ERR_clear_error();
    if (!k) why = "tls: no unencrypted PEM private key in the key text";
  }
  VTlsClient *v = NULL;
  if (why) snprintf(err, en, "%s", why);
  else v = vtls_client_make(trust, leaf, sk_X509_num(chain) > 0 ? chain : NULL, k, err, en);
  X509_free(leaf);
  sk_X509_pop_free(chain, X509_free);
  EVP_PKEY_free(k);
  return v;
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
   against host: a DNS name (also sent as SNI) or an IP address. With a
   VTlsClient (a trusting Net's), its context; else the default one. */
static VTls *vtls_client(const VTlsClient *with, int fd, const char *host, char *err, size_t en) {
  SSL_CTX *ctx;
  if (with) ctx = with->ctx;
  else {
    pthread_once(&vtls_once, vtls_client_init);
    if (!vtls_client_ctx) { snprintf(err, en, "%s", vtls_client_err); return NULL; }
    ctx = vtls_client_ctx;
  }
  VTls *t = vtls_new(ctx, fd, 0, err, en);
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

/* The peer closed the socket without a close message (OpenSSL 3 reports
   it as an error; 1.1.1 as a system error with no errno). */
static int vtls_is_cut(int e) {
#ifdef SSL_R_UNEXPECTED_EOF_WHILE_READING
  if (e == SSL_ERROR_SSL && ERR_GET_REASON(ERR_peek_last_error()) == SSL_R_UNEXPECTED_EOF_WHILE_READING) return 1;
#endif
  return e == SSL_ERROR_SYSCALL && errno == 0 && ERR_peek_last_error() == 0;
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
    else if (vtls_is_cut(e)) {
      snprintf(err, en, "%s", op == VTLS_HANDSHAKE ? "tls: the connection was closed during the handshake" : VTLS_CUT_MSG);
      ERR_clear_error();
      res = VTLS_ERROR;
    }
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

/* ---------- cryptography beyond TLS: the crypto module ----------

   One entry point for the std module's runtime helper limen_crypto, so
   both backends run the same code. op 1 seals with ChaCha20-Poly1305 (a:
   the 32-byte key, b: the 12-byte nonce, c: associated data, d: the
   plaintext; out: ciphertext then the 16-byte tag), 2 opens (d:
   ciphertext then tag), 3 gives the Ed25519 public key of a 32-byte seed
   (a), 4 signs d with the seed a, 5 verifies the signature c of d against
   the public key a (out: one byte, 1 or 0), 6 gives the X25519 public key
   of the private key a, 7 agrees a shared secret from the private key a
   and the peer's public key b (refusing the all-zero result of a
   low-order point). 0 and *out (malloc'd) on success, -1 and err on
   failure. */
#include <openssl/evp.h>
static int vcrypto(int op, const unsigned char *a, size_t an, const unsigned char *b, size_t bn,
                   const unsigned char *c, size_t cn, const unsigned char *d, size_t dn,
                   unsigned char **out, size_t *outn, char *err, size_t en) {
  *out = NULL; *outn = 0;
  if (op == 1 || op == 2) {
    if (an != 32) { snprintf(err, en, "crypto: a key is 32 bytes, not %zu", an); return -1; }
    if (bn != 12) { snprintf(err, en, "crypto: a nonce is 12 bytes, not %zu", bn); return -1; }
    if (op == 2 && dn < 16) { snprintf(err, en, "crypto: the message is shorter than its tag"); return -1; }
    /* OpenSSL takes an int length: refuse what it cannot take whole, so
       nothing is left unencrypted or uninitialised */
    if (cn > (size_t)INT_MAX || dn > (size_t)INT_MAX - 32) { snprintf(err, en, "crypto: at most 2 GB at a time"); return -1; }
    size_t textn = op == 1 ? dn : dn - 16;
    unsigned char *o = (unsigned char *)malloc(textn + 16 + 1);
    EVP_CIPHER_CTX *x = EVP_CIPHER_CTX_new();
    int len = 0, ok = o && x;
    ok = ok && EVP_CipherInit_ex(x, EVP_chacha20_poly1305(), NULL, NULL, NULL, op == 1) == 1;
    ok = ok && EVP_CIPHER_CTX_ctrl(x, EVP_CTRL_AEAD_SET_IVLEN, 12, NULL) == 1;
    ok = ok && EVP_CipherInit_ex(x, NULL, NULL, a, b, op == 1) == 1;
    ok = ok && (cn == 0 || EVP_CipherUpdate(x, NULL, &len, c, (int)cn) == 1);
    ok = ok && (textn == 0 || (EVP_CipherUpdate(x, o, &len, d, (int)textn) == 1 && (size_t)len == textn));
    if (ok && op == 2) ok = EVP_CIPHER_CTX_ctrl(x, EVP_CTRL_AEAD_SET_TAG, 16, (void *)(d + textn)) == 1;
    int fin = ok ? EVP_CipherFinal_ex(x, o + textn, &len) : 0;
    if (ok && op == 1 && fin == 1) ok = EVP_CIPHER_CTX_ctrl(x, EVP_CTRL_AEAD_GET_TAG, 16, o + textn) == 1;
    if (x) EVP_CIPHER_CTX_free(x);
    if (!ok || fin != 1) {
      free(o);
      if (ok && op == 2) snprintf(err, en, "crypto: the message was changed, or the key, nonce or associated data is not the one it was sealed with");
      else snprintf(err, en, "crypto: ChaCha20-Poly1305 failed");
      return -1;
    }
    *out = o; *outn = op == 1 ? textn + 16 : textn;
    return 0;
  }
  if (op == 3 || op == 6) {
    if (an != 32) { snprintf(err, en, "crypto: a private key is 32 bytes, not %zu", an); return -1; }
    EVP_PKEY *k = EVP_PKEY_new_raw_private_key(op == 3 ? EVP_PKEY_ED25519 : EVP_PKEY_X25519, NULL, a, 32);
    unsigned char *o = (unsigned char *)malloc(32);
    size_t n = 32;
    int ok = k && o && EVP_PKEY_get_raw_public_key(k, o, &n) == 1 && n == 32;
    if (k) EVP_PKEY_free(k);
    if (!ok) { free(o); snprintf(err, en, "crypto: cannot make the public key"); return -1; }
    *out = o; *outn = 32;
    return 0;
  }
  if (op == 4) {
    if (an != 32) { snprintf(err, en, "crypto: a signing key is 32 bytes, not %zu", an); return -1; }
    EVP_PKEY *k = EVP_PKEY_new_raw_private_key(EVP_PKEY_ED25519, NULL, a, 32);
    EVP_MD_CTX *m = EVP_MD_CTX_new();
    unsigned char *o = (unsigned char *)malloc(64);
    size_t n = 64;
    int ok = k && m && o && EVP_DigestSignInit(m, NULL, NULL, NULL, k) == 1 && EVP_DigestSign(m, o, &n, d, dn) == 1 && n == 64;
    if (m) EVP_MD_CTX_free(m);
    if (k) EVP_PKEY_free(k);
    if (!ok) { free(o); snprintf(err, en, "crypto: Ed25519 signing failed"); return -1; }
    *out = o; *outn = 64;
    return 0;
  }
  if (op == 5) {
    unsigned char *o = (unsigned char *)malloc(1);
    if (!o) { snprintf(err, en, "crypto: out of memory"); return -1; }
    o[0] = 0;
    if (an == 32 && cn == 64) {
      EVP_PKEY *k = EVP_PKEY_new_raw_public_key(EVP_PKEY_ED25519, NULL, a, 32);
      EVP_MD_CTX *m = EVP_MD_CTX_new();
      if (k && m && EVP_DigestVerifyInit(m, NULL, NULL, NULL, k) == 1 && EVP_DigestVerify(m, c, cn, d, dn) == 1) o[0] = 1;
      if (m) EVP_MD_CTX_free(m);
      if (k) EVP_PKEY_free(k);
      ERR_clear_error();
    }
    *out = o; *outn = 1;
    return 0;
  }
  if (op == 7) {
    if (an != 32 || bn != 32) { snprintf(err, en, "crypto: X25519 keys are 32 bytes"); return -1; }
    EVP_PKEY *k = EVP_PKEY_new_raw_private_key(EVP_PKEY_X25519, NULL, a, 32);
    EVP_PKEY *p = EVP_PKEY_new_raw_public_key(EVP_PKEY_X25519, NULL, b, 32);
    EVP_PKEY_CTX *x = k ? EVP_PKEY_CTX_new(k, NULL) : NULL;
    unsigned char *o = (unsigned char *)malloc(32);
    size_t n = 32;
    int ok = k && p && x && o && EVP_PKEY_derive_init(x) == 1 && EVP_PKEY_derive_set_peer(x, p) == 1 && EVP_PKEY_derive(x, o, &n) == 1 && n == 32;
    if (x) EVP_PKEY_CTX_free(x);
    if (p) EVP_PKEY_free(p);
    if (k) EVP_PKEY_free(k);
    if (ok) {
      unsigned char z = 0;
      for (int i = 0; i < 32; i++) z |= o[i];
      if (!z) ok = 0;
    }
    if (!ok) { free(o); ERR_clear_error(); snprintf(err, en, "crypto: no shared secret with that public key"); return -1; }
    *out = o; *outn = 32;
    return 0;
  }
  snprintf(err, en, "crypto: unknown operation %d", op);
  return -1;
}
