(* Print `let files = [ (PATH, CONTENTS); ... ]` for the files named on the
   command line, each PATH relative to src/design. src/dune runs it to build
   design_files.ml: the shared stylesheet, the mark, the fonts and the
   `limen new` templates, carried in the compiler so what it writes is
   self-contained. *)
let () =
  print_string "let files = [\n";
  Array.iteri
    (fun i path ->
      if i > 0 then begin
        let data = In_channel.with_open_bin path In_channel.input_all in
        let name =
          let p = "design/" in
          if String.length path > String.length p && String.sub path 0 (String.length p) = p then
            String.sub path (String.length p) (String.length path - String.length p)
          else path
        in
        Printf.printf "  (%S, %S);\n" name data
      end)
    Sys.argv;
  print_string "]\n"
