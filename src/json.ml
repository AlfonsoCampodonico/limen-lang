(* Minimal JSON, enough for the language server protocol. *)
type t = Null | Bool of bool | Int of int | Float of float | String of string | List of t list | Obj of (string * t) list

exception Error of string

let parse (s : string) : t =
  let n = String.length s in
  let i = ref 0 in
  let peek () = if !i < n then s.[!i] else '\000' in
  let rec ws () = if !i < n && (s.[!i] = ' ' || s.[!i] = '\n' || s.[!i] = '\r' || s.[!i] = '\t') then (incr i; ws ()) in
  let expect c = ws (); if peek () <> c then raise (Error (Printf.sprintf "expected %c at %d" c !i)); incr i in
  let lit w v = if !i + String.length w <= n && String.sub s !i (String.length w) = w then (i := !i + String.length w; v) else raise (Error "bad literal") in
  let utf8 b code =
    if code < 0x80 then Buffer.add_char b (Char.chr code)
    else if code < 0x800 then begin
      Buffer.add_char b (Char.chr (0xC0 lor (code lsr 6)));
      Buffer.add_char b (Char.chr (0x80 lor (code land 0x3F)))
    end
    else begin
      Buffer.add_char b (Char.chr (0xE0 lor (code lsr 12)));
      Buffer.add_char b (Char.chr (0x80 lor ((code lsr 6) land 0x3F)));
      Buffer.add_char b (Char.chr (0x80 lor (code land 0x3F)))
    end
  in
  let str () =
    expect '"';
    let b = Buffer.create 16 in
    let rec go () =
      if !i >= n then raise (Error "unterminated string");
      let c = s.[!i] in
      incr i;
      if c = '"' then ()
      else if c = '\\' then begin
        let e = s.[!i] in
        incr i;
        (match e with
         | 'n' -> Buffer.add_char b '\n'
         | 't' -> Buffer.add_char b '\t'
         | 'r' -> Buffer.add_char b '\r'
         | 'b' -> Buffer.add_char b '\b'
         | 'f' -> Buffer.add_char b '\012'
         | 'u' ->
           let code = int_of_string ("0x" ^ String.sub s !i 4) in
           i := !i + 4;
           utf8 b code
         | c -> Buffer.add_char b c);
        go ()
      end
      else (Buffer.add_char b c; go ())
    in
    go ();
    Buffer.contents b
  in
  let rec value () =
    ws ();
    match peek () with
    | '{' ->
      incr i;
      ws ();
      if peek () = '}' then (incr i; Obj [])
      else
        let rec fields acc =
          let k = (ws (); str ()) in
          expect ':';
          let v = value () in
          ws ();
          if peek () = ',' then (incr i; fields ((k, v) :: acc))
          else (expect '}'; Obj (List.rev ((k, v) :: acc)))
        in
        fields []
    | '[' ->
      incr i;
      ws ();
      if peek () = ']' then (incr i; List [])
      else
        let rec items acc =
          let v = value () in
          ws ();
          if peek () = ',' then (incr i; items (v :: acc)) else (expect ']'; List (List.rev (v :: acc)))
        in
        items []
    | '"' -> String (str ())
    | 't' -> lit "true" (Bool true)
    | 'f' -> lit "false" (Bool false)
    | 'n' -> lit "null" Null
    | _ ->
      let start = !i in
      while !i < n && String.contains "+-0123456789.eE" s.[!i] do incr i done;
      let t = String.sub s start (!i - start) in
      if t = "" then raise (Error (Printf.sprintf "unexpected character at %d" start));
      (match int_of_string_opt t with Some k -> Int k | None -> Float (float_of_string t))
  in
  let v = value () in
  v

let rec to_string = function
  | Null -> "null"
  | Bool b -> string_of_bool b
  | Int k -> string_of_int k
  | Float f ->
    (* the shortest text that reads back as the same number *)
    let s = Printf.sprintf "%.15g" f in
    if float_of_string s = f then s else Printf.sprintf "%.17g" f
  | String s -> Diag.jstr s
  | List l -> "[" ^ String.concat "," (List.map to_string l) ^ "]"
  | Obj fs -> "{" ^ String.concat "," (List.map (fun (k, v) -> Diag.jstr k ^ ":" ^ to_string v) fs) ^ "}"

let member k = function Obj fs -> ( match List.assoc_opt k fs with Some v -> v | None -> Null) | _ -> Null
let to_int = function Int k -> k | Float f -> int_of_float f | _ -> 0
let to_str = function String s -> s | _ -> ""
let path j ks = List.fold_left (fun j k -> member k j) j ks
