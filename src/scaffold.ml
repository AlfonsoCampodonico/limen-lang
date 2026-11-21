(* `limen new [--web] DIR`: a new project from the templates in
   src/design/starter, with `__NAME__` replaced by the directory's name. The
   web starter also gets the shared stylesheet (with the site's fonts, which
   it serves itself), the mark and the fonts. *)

let replace_all ~sub ~by s =
  let b = Buffer.create (String.length s) in
  let n = String.length sub in
  let rec go i =
    if i > String.length s - n then Buffer.add_string b (String.sub s i (String.length s - i))
    else if String.sub s i n = sub then (Buffer.add_string b by; go (i + n))
    else (Buffer.add_char b s.[i]; go (i + 1))
  in
  go 0;
  Buffer.contents b

(* The fonts, served by the app from /assets/fonts: no font service. *)
let font_faces =
  {|/* The fonts are served by this site (assets/fonts, SIL Open Font License,
   subset to Latin): no visitor's browser contacts a font service. */
@font-face { font-family: "Inter"; src: url("/assets/fonts/Inter.woff2") format("woff2"); font-weight: 400 700; font-style: normal; font-display: swap; }
@font-face { font-family: "JetBrains Mono"; src: url("/assets/fonts/JetBrainsMono-Regular.woff2") format("woff2"); font-weight: 400; font-style: normal; font-display: swap; }
@font-face { font-family: "JetBrains Mono"; src: url("/assets/fonts/JetBrainsMono-Medium.woff2") format("woff2"); font-weight: 500; font-style: normal; font-display: swap; }

|}

let valid_name n =
  n <> "" && String.for_all (fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '-' || c = '_') n

(* The files of a new project called [name]: (path inside it, contents). *)
let files ~web name =
  let fill (p, s) = (p, if Filename.check_suffix p ".woff2" then s else replace_all ~sub:"__NAME__" ~by:name s) in
  let base = List.map fill (Design.starter (if web then "web" else "plain")) in
  if not web then base
  else
    base
    @ [ ("assets/style.css", font_faces ^ Design.css); ("assets/logo.svg", Design.file "logo.svg") ]
    @ List.filter_map
        (fun (p, s) -> if String.length p > 6 && String.sub p 0 6 = "fonts/" then Some ("assets/" ^ p, s) else None)
        Design_files.files

let rec mkdir_p d =
  if d <> "" && d <> "." && d <> "/" && not (Sys.file_exists d) then begin
    mkdir_p (Filename.dirname d);
    Sys.mkdir d 0o755
  end

(* Write a new project into [dir], which must not exist or be empty.
   Returns the paths written, or why not. *)
let create ~web dir =
  let name = Filename.basename (if Filename.check_suffix dir "/" then Filename.chop_suffix dir "/" else dir) in
  if not (valid_name name) then Error (Printf.sprintf "%s: a project's name is letters, digits, - and _" name)
  else if Sys.file_exists dir && not (Sys.is_directory dir) then Error (dir ^ " exists and is not a directory")
  else if Sys.file_exists dir && Sys.readdir dir <> [||] then Error (dir ^ " is not empty")
  else begin
    let fs = files ~web name in
    List.iter
      (fun (p, s) ->
        let path = Filename.concat dir p in
        mkdir_p (Filename.dirname path);
        Out_channel.with_open_bin path (fun oc -> output_string oc s))
      fs;
    Ok (List.map fst fs)
  end
