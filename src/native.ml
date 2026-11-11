(* Checked program -> C source -> native binary (via the system C compiler). *)

let c_source ?(tests = false) items =
  let prog, g = Lower.lower_program ~tests items in
  let prog = Own.program prog in
  Cgen.program g prog

let cc () = match Sys.getenv_opt "CC" with Some c when c <> "" -> c | _ -> "cc"

(* Compile [c_text] to [exe]. Returns the compiler's output on failure. *)
(* No FMA contraction: float results must not depend on the C compiler. *)
(* A program that may use TLS (Cgen marks it) gets the TLS core pasted in
   ahead of the runtime and is linked with OpenSSL, as found when vela was
   built (Openssl_config). Without OpenSSL it still compiles, and its TLS
   operations fail with an error saying so. *)
let tls_parts c_text =
  if Openssl_config.found && String.starts_with ~prefix:Cgen.tls_marker c_text then
    ("#define _GNU_SOURCE\n#define VELA_TLS 1\n" ^ Tls_text.text ^ c_text, Openssl_config.cflags, Openssl_config.libs)
  else (c_text, [], [])

let compile_c ?(flags = [ "-O2" ]) c_text exe : (unit, string) result =
  let c_file = exe ^ ".c" in
  let c_text, tls_cflags, tls_libs = tls_parts c_text in
  Out_channel.with_open_bin c_file (fun oc -> output_string oc c_text);
  let argv =
    Array.of_list
      ((cc () :: flags) @ tls_cflags
      @ [ "-fwrapv"; "-ffp-contract=off"; "-fstack-clash-protection"; "-fexceptions"; "-w"; "-o"; exe; c_file ]
      @ tls_libs @ [ "-lm" ])
  in
  let out, inp, err = Unix.open_process_args_full (cc ()) argv (Unix.environment ()) in
  close_out inp;
  let o = In_channel.input_all out in
  let e = In_channel.input_all err in
  match Unix.close_process_full (out, inp, err) with
  | Unix.WEXITED 0 -> Ok ()
  | _ -> Error (o ^ e)

let build ?flags ?tests items exe = compile_c ?flags (c_source ?tests items) exe
