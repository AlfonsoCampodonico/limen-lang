(* How fast `tessera check` is, by phase, on one file, in CPU time (wall time is
   at the mercy of whatever else the machine runs):
     dune exec bench/checkspeed/checkspeed.exe -- FILE
   gen.py writes a large program of ordinary code to try it on. Each phase is
   run on its own, then the whole check once more; the words column is what
   the phase allocated, in millions. *)
open Tessera

let time name f =
  Gc.full_major ();
  let w0 = Gc.minor_words () and t0 = Sys.time () in
  let r = f () in
  let t = Sys.time () -. t0 in
  Printf.printf "%-10s %6.3f s %6.0fM words\n%!" name t ((Gc.minor_words () -. w0) /. 1e6);
  (r, t)

let () =
  let file = Sys.argv.(1) in
  let src = In_channel.with_open_bin file In_channel.input_all in
  let lines = List.length (String.split_on_char '\n' src) in
  ignore (time "lex" (fun () -> Lexer.lex file src));
  ignore (time "lex+parse" (fun () -> Parser.parse_file file src));
  let (items, _), _ = time "load" (fun () -> Modules.load file src) in
  ignore (time "check" (fun () -> Check.check_program items));
  let (_, ds, _), total = time "all" (fun () -> Modules.load_and_check file src) in
  Printf.printf "%d lines, %d errors; `tessera check` (lex, parse, load, check): %.0f lines/s\n" lines (List.length (Diag.errors ds))
    (float lines /. total)
