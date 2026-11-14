(* Write to OUT.c the C of FILE's test binary, as `tessera test --native`
   compiles it (Native.c_source ~tests:true), after the same checks. When
   the program does not load or check, print the problems as `tessera
   build` does and exit 1. *)
open Tessera

let () =
  let file = Sys.argv.(1) and out = Sys.argv.(2) in
  let src = In_channel.with_open_bin file In_channel.input_all in
  let items, ds, _ = Modules.load_and_check file src in
  List.iter (fun d -> prerr_string (Diag.render d); prerr_newline ()) (Diag.warnings ds);
  match Diag.errors ds with
  | [] ->
    if List.exists (function Ast.IFn { fn_test = Some _; _ } -> true | _ -> false) items then
      Out_channel.with_open_bin (out ^ ".c") (fun oc -> output_string oc (Native.c_source ~tests:true items))
    else print_endline "no tests"
  | errors ->
    List.iter (fun d -> prerr_string (Diag.render d); prerr_newline ()) errors;
    let n = List.length errors in
    Printf.eprintf "%d error%s\n" n (if n = 1 then "" else "s");
    exit 1
