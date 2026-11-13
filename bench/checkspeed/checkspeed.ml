(* How fast `vela check` is, by phase, on one file, in CPU time (wall time is
   at the mercy of whatever else the machine runs):
     dune exec bench/checkspeed/checkspeed.exe -- FILE
   gen.py writes a large program of ordinary code to try it on. *)
open Vela

let time name f =
  let t0 = Sys.time () in
  let r = f () in
  Printf.printf "%-8s %6.3f s\n%!" name (Sys.time () -. t0);
  r

let () =
  let file = Sys.argv.(1) in
  let src = In_channel.with_open_bin file In_channel.input_all in
  let lines = List.length (String.split_on_char '\n' src) in
  ignore (time "lex" (fun () -> Lexer.lex file src));
  ignore (time "parse" (fun () -> Parser.parse_file file src));
  let t0 = Sys.time () in
  let _, ds, _ = time "check" (fun () -> Modules.load_and_check file src) in
  let total = Sys.time () -. t0 in
  Printf.printf "%d lines, %d errors; `vela check` (lex, parse, load, check): %.0f lines/s\n" lines (List.length (Diag.errors ds))
    (float lines /. total)
