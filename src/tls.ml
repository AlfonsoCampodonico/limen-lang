(* The interpreter's TLS: bindings to tls_stubs.c, which uses the same core
   as the native runtime (runtime/limen_tls.h). Every call is non-blocking;
   errors are raised as [Failure message]. *)

type session
type ctx

(* A trusting or presenting Net's client context: the certificates it
   verifies servers against (the system's, or those given to `trusting`),
   and the certificate it presents, if any (runtime/limen_tls.h,
   VTlsClient). *)
type trust

(* A client session on a connected socket, verifying the certificate
   against [host], with a Net's own context or else the default one. *)
external client_with : trust option -> Unix.file_descr -> string -> session = "limen_tls_client"

(* The context of a Net (None: the default) with its trust replaced by the
   CERTIFICATE blocks in PEM text, keeping any certificate it presents; or
   the error, worded as natively. *)
external trust_raw : trust option -> string -> trust = "limen_tls_trust"
let trust from pem = match trust_raw from pem with t -> Ok t | exception Failure m -> Error m

(* The context of a Net with a certificate (and chain) and key to present,
   keeping its trust; or the error, worded as natively. *)
external present_raw : trust option -> string -> string -> trust = "limen_tls_present"
let present from cert key = match present_raw from cert key with t -> Ok t | exception Failure m -> Error m

(* A server context from PEM text: certificate (and chain), then key. *)
external server_ctx : string -> string -> ctx = "limen_tls_server_ctx"

(* A server session on an accepted socket; its handshake runs in the first step. *)
external server : ctx -> Unix.file_descr -> session = "limen_tls_server"

(* [step s op buf off len]: op 0 reads into buf, 1 writes from it, 2 runs the
   handshake. Returns the bytes moved (0 at the end of a read stream, 0 when
   the handshake is done), or [want_read] / [want_write]. *)
external step : session -> int -> bytes -> int -> int -> int = "limen_tls_step"

(* Send close_notify and free the session. *)
external close : session -> unit = "limen_tls_close"
external ctx_free : ctx -> unit = "limen_tls_ctx_free"
external available : unit -> bool = "limen_tls_available"

(* The crypto module's runtime helper: [crypto op a b c d], the result after
   a status byte (runtime/limen_tls.h, vcrypto). *)
external crypto : int -> string -> string -> string -> string -> string = "limen_crypto_op"

let want_read = -1
let want_write = -2

(* What a socket speaks: plain TCP, a TLS session, or (a listener) the
   context its connections get their sessions from. *)
type side = Plain | Session of session | Server of ctx
