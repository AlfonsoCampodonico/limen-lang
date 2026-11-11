(* Registry of loaded source texts, used to turn offsets into line/column. *)
let table : (string, string) Hashtbl.t = Hashtbl.create 8

let add file text = Hashtbl.replace table file text
let text file = Hashtbl.find_opt table file

let line_col file off =
  match text file with
  | None -> (1, off + 1)
  | Some s ->
    let line = ref 1 and col = ref 1 in
    for i = 0 to min off (String.length s) - 1 do
      if s.[i] = '\n' then (incr line; col := 1) else incr col
    done;
    (!line, !col)

let line_text file ln =
  match text file with
  | None -> ""
  | Some s ->
    (match List.nth_opt (String.split_on_char '\n' s) (ln - 1) with
     | Some l -> l
     | None -> "")
