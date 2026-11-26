(* `limen test --coverage`: which lines of the program's own code its tests
   ran. A line counts when a statement or a block's value starts on it; the
   interpreter records the ones that run (Interp.coverage). Test functions
   and the standard library are left out. *)
open Ast

(* Where every statement and block value of [items]' non-test functions
   starts, in files that are the program's own. *)
let coverable (items : item list) : (string * int) list =
  let out = ref [] in
  let add (sp : Span.t) = out := (sp.file, sp.lo) :: !out in
  let rec expr (e : expr) =
    let go = expr in
    match e.edesc with
    | EInt _ | EFloat _ | EBool _ | EUnit | EVar _ -> ()
    | EStr parts -> List.iter (function SPExpr x -> go x | SPLit _ -> ()) parts
    | EList es -> List.iter go es
    | EBin (_, a, b) -> go a; go b
    | EUn (_, a) -> go a
    | ECall (c, args) -> go c; List.iter go args
    | EMethod (r, _, _, args) -> go r; List.iter go args
    | EField (r, _, _) -> go r
    | EIndex (a, i) -> go a; go i
    | ETry (a, c) -> go a; Option.iter go c
    | EIf (c, b, els) -> go c; block b; Option.iter go els
    | EMatch (s, arms) -> go s; List.iter (fun a -> Option.iter go a.aguard; go a.abody) arms
    | EBlock b -> block b
    | EClosure (_, body) -> go body
    | EStruct (_, _, fs) -> List.iter (fun (_, _, x) -> go x) fs
    | EGroup (_, _, d, b) -> Option.iter (fun (c, ms) -> go c; go ms) d; block b
    | EDyn (x, _) -> go x
    | ESelect arms ->
      List.iter
        (fun a ->
          (match a.sop with
           | SelRecv (_, c) -> go c
           | SelSend (c, x) -> go c; go x
           | SelAfter (c, ms) -> go c; go ms
           | SelDefault -> ());
          Option.iter go a.sguard;
          go a.sbody)
        arms
    | EPipe (a, b) -> go a; go b
    | EWith (b, fs) -> go b; List.iter (fun (_, _, x) -> go x) fs
    | ESafe (b, _, rest, _) -> go b; go rest
    | ENamed (_, _, x) | EMut x -> go x
  and block (b : block) =
    List.iter
      (fun s ->
        add s.sspan;
        match s.sdesc with
        | SLet (_, _, _, _, e) | SExpr e | SLetPat (_, e) -> expr e
        | SAssign (a, b) -> expr a; expr b
        | SReturn e -> Option.iter expr e
        | SWhile (c, b) -> expr c; block b
        | SFor (_, _, e, b) | SForPat (_, e, b) -> expr e; block b
        | SBreak | SContinue -> ()
        | SDefer b -> block b)
      b.stmts;
    Option.iter (fun e -> add e.espan; expr e) b.tail
  in
  List.iter
    (function
      | IFn d when d.fn_test = None && d.fn_foreign = None && not (String.starts_with ~prefix:"<" d.fn_span.file) -> block d.fn_body
      | _ -> ())
    items;
  List.sort_uniq compare !out

(* Runs of consecutive numbers, as `51-53, 88`. *)
let ranges lines =
  let rec go acc = function
    | [] -> List.rev acc
    | x :: rest -> (
      match acc with
      | (lo, hi) :: more when x = hi + 1 -> go ((lo, x) :: more) rest
      | _ -> go ((x, x) :: acc) rest)
  in
  String.concat ", " (List.map (fun (lo, hi) -> if lo = hi then string_of_int lo else Printf.sprintf "%d-%d" lo hi) (go [] lines))

type file = { path : string; lines : int; run : int; missed : int list }

(* Per file: its coverable lines, how many ran, and the lines that did not. *)
let report (items : item list) (ran : (string * int, unit) Hashtbl.t) : file list =
  let line (f, off) = fst (Source.line_col f off) in
  let by_file = Hashtbl.create 8 in
  List.iter
    (fun ((f, _) as at) ->
      let l = line at in
      let lines = Option.value ~default:[] (Hashtbl.find_opt by_file f) in
      Hashtbl.replace by_file f ((l, Hashtbl.mem ran at) :: lines))
    (coverable items);
  Hashtbl.fold
    (fun path entries acc ->
      (* a line ran if any statement starting on it did *)
      let lines = List.sort_uniq compare (List.map fst entries) in
      let ran_lines = List.sort_uniq compare (List.filter_map (fun (l, r) -> if r then Some l else None) entries) in
      let missed = List.filter (fun l -> not (List.mem l ran_lines)) lines in
      { path; lines = List.length lines; run = List.length lines - List.length missed; missed } :: acc)
    by_file []
  |> List.sort (fun a b -> compare a.path b.path)

let percent f = if f.lines = 0 then 100 else 100 * f.run / f.lines

let to_text (fs : file list) =
  let total = List.fold_left (fun a f -> a + f.lines) 0 fs and run = List.fold_left (fun a f -> a + f.run) 0 fs in
  let head = Printf.sprintf "coverage: %d%% of %d lines ran" (if total = 0 then 100 else 100 * run / total) total in
  String.concat "\n"
    (head
    :: List.map
         (fun f ->
           Printf.sprintf "  %s: %d%% (%d of %d)%s" f.path (percent f) f.run f.lines
             (if f.missed = [] then "" else "; not run: " ^ ranges f.missed))
         fs)
  ^ "\n"
