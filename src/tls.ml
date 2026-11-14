(* The interpreter's TLS: bindings to tls_stubs.c, which uses the same core
   as the native runtime (runtime/tessera_tls.h). Every call is non-blocking;
   errors are raised as [Failure message]. *)

type session
type ctx

(* A client session on a connected socket, verifying the certificate against [host]. *)
external client : Unix.file_descr -> string -> session = "tessera_tls_client"

(* A server context from PEM text: certificate (and chain), then key. *)
external server_ctx : string -> string -> ctx = "tessera_tls_server_ctx"

(* A server session on an accepted socket; its handshake runs in the first step. *)
external server : ctx -> Unix.file_descr -> session = "tessera_tls_server"

(* [step s op buf off len]: op 0 reads into buf, 1 writes from it, 2 runs the
   handshake. Returns the bytes moved (0 at the end of a read stream, 0 when
   the handshake is done), or [want_read] / [want_write]. *)
external step : session -> int -> bytes -> int -> int -> int = "tessera_tls_step"

(* Send close_notify and free the session. *)
external close : session -> unit = "tessera_tls_close"
external ctx_free : ctx -> unit = "tessera_tls_ctx_free"
external available : unit -> bool = "tessera_tls_available"

let want_read = -1
let want_write = -2

(* What a socket speaks: plain TCP, a TLS session, or (a listener) the
   context its connections get their sessions from. *)
type side = Plain | Session of session | Server of ctx
