(* The design shared with limen-lang.org: src/design, copied from limen-book
   by dev/design_sync.py and carried in the compiler (Design_files), so pages
   it writes need nothing from the network. *)

let file name =
  match List.assoc_opt name Design_files.files with Some s -> s | None -> invalid_arg ("Design.file " ^ name)

(* tokens, the site's components and the parts for Limen's own outputs *)
let css = file "limen.css"

(* The mark for inlining in a page, decorative beside the name: its shapes
   in a bare `<svg>` sized by the stylesheet. *)
let mark =
  let s = file "logo.svg" in
  let after_title =
    let t = "</title>" in
    let rec find i = if i + String.length t > String.length s then 0 else if String.sub s i (String.length t) = t then i + String.length t else find (i + 1) in
    find 0
  in
  let body = String.sub s after_title (String.length s - after_title) in
  let body = String.concat "" (List.map String.trim (String.split_on_char '\n' body)) in
  "<svg viewBox=\"0 0 180 180\" aria-hidden=\"true\">" ^ String.sub body 0 (String.length body - String.length "</svg>") ^ "</svg>"

(* `limen new` templates: (path inside the new directory, contents) *)
let starter kind =
  let p = "starter/" ^ kind ^ "/" in
  let n = String.length p in
  List.filter_map
    (fun (name, data) ->
      if String.length name > n && String.sub name 0 n = p then Some (String.sub name n (String.length name - n), data) else None)
    Design_files.files
