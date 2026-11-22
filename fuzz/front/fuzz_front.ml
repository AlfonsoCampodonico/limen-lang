(* Fuzz the front end: mutate real programs and run each mutant through the
   lexer, the parser and the checker (with the module loader and the
   capability checks, as `limen check` runs them), in this process. A
   malformed program must get an ordinary diagnostic, so any exception
   other than the lexer's and parser's own [Diag.Fatal], a diagnostic whose
   code is not E or W and four digits, or an input that takes longer than
   the time limit is a problem.

   With --selfhost BIN (selfhost/limen.lmn, built), each mutant also goes
   to the self-hosted compiler's `check`: a crash, a hang, or a
   disagreement with the OCaml checker on whether the program has errors is
   a problem. With --exact LIMEN (bin/main.exe) as well, any difference
   between what `limen check` and the self-hosted compiler print (stdout,
   stderr and exit status) is a problem too.

     dune build ./fuzz/front/fuzz_front.exe
     _build/default/fuzz/front/fuzz_front.exe --seed 1 --count 5000 examples selfhost/tests

   The mutations: byte flips, token deletion, duplication and swaps,
   fragments of other programs spliced in, deep nesting, huge literals and
   odd Unicode. Each mutant is written into a copy of its program's
   directory under --work (a fresh temporary directory by default), so a
   program's modules and embedded files are still there.

   An input that takes longer than the time limit is run once more, and is
   a problem only if it is slow again (a busy machine is not a hang).
   --replay checks the files named instead, as they are, once each. --save
   DIR keeps every problem's input. The last line is `N inputs, K problems`.
   A temporary work directory is removed at the end. *)
open Limen

let sprintf = Printf.sprintf

(* ---------- the corpus ---------- *)

type prog = {
  path : string; (* as found *)
  src : string;
  dir : int; (* index into [dirs] *)
  base : string; (* file name in its directory *)
  spans : (int * int) array; (* the tokens of the unmutated program, if it lexes *)
  kinds : char array; (* each token's kind: [n]ame, [l]iteral, [k]eyword, [s]ymbol *)
}

let read_file p = In_channel.with_open_bin p In_channel.input_all
let write_file p s = Out_channel.with_open_bin p (fun oc -> output_string oc s)

let rec lmn_files acc p =
  if Sys.is_directory p then begin
    let b = Filename.basename p in
    if b = "_build" || b = ".git" || b = "node_modules" then acc
    else Array.fold_left (fun acc f -> lmn_files acc (Filename.concat p f)) acc (let a = Sys.readdir p in Array.sort compare a; a)
  end
  else if Filename.check_suffix p ".lmn" then p :: acc
  else acc

let tokens_of path src =
  match Lexer.lex path src with
  | exception _ -> ([||], [||])
  | t ->
    let n = max 0 (t.count - 1) in
    ( Array.init n (fun i -> (Lexer.lo t i, Lexer.hi t i)),
      Array.init n (fun i ->
          match t.toks.(i) with Lexer.IDENT _ -> 'n' | INT _ | FLOAT _ | STR _ -> 'l' | KW _ -> 'k' | SYM _ | EOF -> 's') )

(* Every name and every literal of the corpus, as written, to swap in for
   another: the program still parses, so the checker sees it. *)
let names = ref [||] and literals = ref [||]

(* ---------- mutations ---------- *)

let pick rand a = a.(Random.State.int rand (Array.length a))

let unicode =
  [| "\xc3\xa9"; "\xcc\x81"; "\xe2\x80\xae"; "\xef\xbb\xbf"; "\x00"; "\xed\xa0\x80"; "\xed\xbf\xbf"; "\xc0\xaf"; "\xc0\x80"; "\xf4\x90\x80\x80";
     "\xf8\x88\x80\x80\x80"; "\xff"; "\xfe"; "\xe2\x80\xa8"; "\xe2\x80\xa9"; "\xe2\x80\x8b"; "\xe2\x80\x8d"; "\xf0\x9f\x98\x80"; "\xc2\xa0";
     "\xe3\x80\x80"; "\r"; "\r\n"; "\t"; "\x0b"; "\x0c"; "\x80"; "\xbf"; "\xe2\x82"; "\xf0\x9f"; "\xef\xbc\x88"; "\xce\xbb"; "\xd0\xb0";
     "\xef\xbf\xbf"; "\xef\xbf\xbd"; "\xd8\xa7"; "\xe0\xa4\x95\xe0\xa5\x8d"; "\x7f"; "\x1b[31m"; "\\u{110000}"; "\\u{d800}"; "\\u{}";
     "\\u{0000000}"; "\\u{10ffff}"; "\\u"; "\\"; "\\{"; "\\x"; "\"\"\""; "\"\"\"\n"; "\"\"\"\\\n" |]

let pieces =
  [| "("; ")"; "["; "]"; "{"; "}"; ","; ":"; ";"; "."; ".."; "..<"; "?"; "??"; "|"; "||"; "|>"; "->"; "=>"; "="; "=="; "<"; ">"; "<<";
     ">>"; "-"; "!"; "&"; "^"; "~"; "%"; "/"; "*"; "+"; "\""; "\n"; " "; "//"; "/*"; "*/"; "_"; "0"; "1.5"; "fn"; "let"; "mut"; "if";
     "else"; "match"; "for"; "in"; "while"; "return"; "struct"; "enum"; "true"; "false"; "group"; "break"; "continue"; "pub"; "use";
     "interface"; "dyn"; "select"; "defer"; "self"; "test"; "bench"; "foreign"; "type"; "virtual"; "with"; "as"; "Root"; "Int"; "Str";
     "Option"; "Result"; "List"; "Map"; "Array"; "Out"; "Shared"; "Atomic"; "None"; "Some"; "Ok"; "Err"; "main"; "root"; "x"; "@";
     "#"; "$"; "`"; "'"; "\\" |]

let digits rand n alphabet = String.init n (fun _ -> alphabet.[Random.State.int rand (String.length alphabet)])

let huge rand =
  let n = pick rand [| 19; 20; 21; 40; 309; 400; 1000; 100000 |] in
  match Random.State.int rand 12 with
  | 0 -> digits rand n "0123456789"
  | 1 -> "-" ^ digits rand n "0123456789"
  | 2 -> "0x" ^ digits rand n "0123456789abcdefABCDEF"
  | 3 -> "0b" ^ digits rand n "01"
  | 4 -> "1." ^ digits rand n "0123456789"
  | 5 -> "1e" ^ digits rand (min n 30) "0123456789"
  | 6 -> digits rand n "0123456789" ^ ".5e-" ^ digits rand 6 "0123456789"
  | 7 -> "\"" ^ String.make (n * 10) 'a' ^ "\""
  | 8 -> String.make n 'x'
  | 9 -> "\"\\u{" ^ digits rand (min n 50) "0123456789abcdef" ^ "}\""
  | 10 -> "9223372036854775808"
  | _ -> "\"" ^ String.concat "" (List.init (min n 5000) (fun _ -> "{x}")) ^ "\""

let rep s n =
  let b = Buffer.create (String.length s * n) in
  for _ = 1 to n do Buffer.add_string b s done;
  Buffer.contents b

let nest rand =
  let n = pick rand [| 10; 50; 100; 300; 1000; 3000; 10000; 30000; 100000 |] in
  match Random.State.int rand 18 with
  | 0 -> rep "(" n ^ "1" ^ rep ")" n
  | 1 -> rep "[" n ^ "1" ^ rep "]" n
  | 2 -> rep "{ " n ^ "1" ^ rep " }" n
  | 3 -> rep "-" n ^ "1"
  | 4 -> rep "!" n ^ "true"
  | 5 -> rep "if true { " n ^ "1" ^ rep " } else { 2 }" n
  | 6 -> rep "|x| " n ^ "x"
  | 7 -> "1" ^ rep " + 1" n
  | 8 -> "x" ^ rep ".f()" n
  | 9 -> rep "\"{" n ^ "1" ^ rep "}\"" n
  | 10 -> rep "Some(" n ^ "1" ^ rep ")" n
  | 11 -> rep "(" n
  | 12 -> rep "match x { _ => " n ^ "1" ^ rep " }" n
  | 13 -> "1" ^ rep " ?? 1" n
  | 14 -> rep "f(" n ^ rep ")" n
  | 15 -> "x" ^ rep "?" n
  | 16 -> rep "[(" n ^ rep ")]" n
  | _ -> rep "{" n

(* A whole program around a deep or huge piece, in the places a parser
   recurses: expressions, types and patterns. *)
let wrap rand piece =
  match Random.State.int rand 4 with
  | 0 -> sprintf "fn main(root: Root) {\n    let x = %s\n    root.stdout.print(\"{x}\")\n}\n" piece
  | 1 ->
    let n = pick rand [| 10; 100; 1000; 10000; 100000 |] in
    let t = match Random.State.int rand 4 with 0 -> rep "List[" n ^ "Int" ^ rep "]" n | 1 -> rep "(" n ^ "Int" ^ rep ")" n | 2 -> rep "fn(" n ^ rep ")" n | _ -> rep "Option[" n ^ "Int" ^ rep "]" n in
    sprintf "fn f(x: %s) {}\n\nfn main(root: Root) {}\n" t
  | 2 ->
    let n = pick rand [| 10; 100; 1000; 10000; 100000 |] in
    let p = match Random.State.int rand 3 with 0 -> rep "Some(" n ^ "y" ^ rep ")" n | 1 -> rep "(" n ^ "y" ^ rep ", 1)" n | _ -> rep "[" n ^ "y" ^ rep "]" n in
    sprintf "fn main(root: Root) {\n    match %s {\n        %s => 1,\n        _ => 2,\n    }\n}\n" piece p
  | _ -> sprintf "fn main(root: Root) {\n    %s\n}\n" piece

let insert s at piece = String.sub s 0 at ^ piece ^ String.sub s at (String.length s - at)
let remove s lo hi = String.sub s 0 lo ^ String.sub s hi (String.length s - hi)

let mutate_once rand (corpus : prog array) (p : prog) s =
  let n = String.length s in
  let at () = if n = 0 then 0 else Random.State.int rand (n + 1) in
  (* tokens of the unmutated program, while they still fit *)
  let toks = if Array.length p.spans > 0 && snd p.spans.(Array.length p.spans - 1) <= n then p.spans else [||] in
  let nt = Array.length toks in
  let tok_range () =
    let i = Random.State.int rand nt in
    let j = min (nt - 1) (i + Random.State.int rand 4) in
    (fst toks.(i), snd toks.(j))
  in
  let boundary () = if nt = 0 then at () else fst toks.(Random.State.int rand nt) in
  let same_kind () =
    (* a name or literal of the program for another from the corpus *)
    let i = Random.State.int rand nt in
    match p.kinds.(i) with
    | ('n' | 'l') as k when (if k = 'n' then !names else !literals) <> [||] ->
      let lo, hi = toks.(i) in
      String.sub s 0 lo ^ pick rand (if k = 'n' then !names else !literals) ^ String.sub s hi (n - hi)
    | _ -> s
  in
  match Random.State.int rand 20 with
  | 16 | 17 | 18 | 19 when nt > 0 -> same_kind ()
  | 0 when n > 0 ->
    let k = Random.State.int rand n in
    let b = Bytes.of_string s in
    Bytes.set b k (Char.chr (Char.code s.[k] lxor (1 lsl Random.State.int rand 8)));
    Bytes.to_string b
  | 1 when n > 0 ->
    let k = Random.State.int rand n in
    let b = Bytes.of_string s in
    Bytes.set b k (Char.chr (Random.State.int rand 256));
    Bytes.to_string b
  | 2 | 3 when nt > 0 ->
    let lo, hi = tok_range () in
    remove s lo hi
  | 4 when nt > 0 ->
    let lo, hi = tok_range () in
    insert s hi (String.sub s lo (hi - lo))
  | 5 when nt > 1 ->
    (* swap two tokens *)
    let i = Random.State.int rand nt and j = Random.State.int rand nt in
    let i, j = (min i j, max i j) in
    if i = j then s
    else
      let (a, b), (c, d) = (toks.(i), toks.(j)) in
      String.sub s 0 a ^ String.sub s c (d - c) ^ String.sub s b (c - b) ^ String.sub s a (b - a) ^ String.sub s d (n - d)
  | 6 | 7 ->
    (* a fragment of another program *)
    let q = pick rand corpus in
    let qn = Array.length q.spans in
    if qn = 0 then s
    else
      let i = Random.State.int rand qn in
      let j = min (qn - 1) (i + Random.State.int rand 40) in
      let frag = String.sub q.src (fst q.spans.(i)) (snd q.spans.(j) - fst q.spans.(i)) in
      if nt > 0 && Random.State.bool rand then
        let lo, hi = tok_range () in
        String.sub s 0 lo ^ frag ^ String.sub s hi (n - hi)
      else insert s (boundary ()) (" " ^ frag ^ " ")
  | 8 -> insert s (boundary ()) (pick rand pieces)
  | 9 -> insert s (if Random.State.bool rand then at () else boundary ()) (pick rand unicode)
  | 10 when nt > 0 ->
    let lo, hi = tok_range () in
    String.sub s 0 lo ^ huge rand ^ String.sub s hi (n - hi)
  | 11 when nt > 0 ->
    let lo, hi = tok_range () in
    String.sub s 0 lo ^ nest rand ^ String.sub s hi (n - hi)
  | 12 -> String.sub s 0 (at ())
  | 13 ->
    (* a line deleted or repeated *)
    let lines = Array.of_list (String.split_on_char '\n' s) in
    let k = Random.State.int rand (Array.length lines) in
    let l = Array.to_list lines in
    String.concat "\n" (List.concat (List.mapi (fun i x -> if i <> k then [ x ] else if Random.State.bool rand then [] else [ x; x ]) l))
  | 14 -> wrap rand (if Random.State.bool rand then nest rand else huge rand)
  | _ -> insert s (boundary ()) (pick rand unicode)

let mutate rand corpus p =
  let rounds = if Random.State.int rand 3 > 0 then 1 else 2 + Random.State.int rand 3 in
  let s = ref p.src in
  for _ = 1 to rounds do
    s := mutate_once rand corpus p !s
  done;
  !s

(* ---------- running one input ---------- *)

exception Timeout

let limited secs f =
  Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise Timeout));
  ignore (Unix.setitimer Unix.ITIMER_REAL { Unix.it_interval = 0.; it_value = secs });
  Fun.protect f ~finally:(fun () ->
      ignore (Unix.setitimer Unix.ITIMER_REAL { Unix.it_interval = 0.; it_value = 0. });
      Sys.set_signal Sys.sigalrm Sys.Signal_default)

let good_code c =
  String.length c = 5 && (c.[0] = 'E' || c.[0] = 'W') && String.for_all (fun ch -> ch >= '0' && ch <= '9') (String.sub c 1 4)

type verdict = Fine of bool (* has errors *) | Problem of string

let describe_exn = function
  | Stack_overflow -> "stack overflow"
  | Out_of_memory -> "out of memory"
  | Timeout -> "took longer than the time limit"
  | e -> "exception " ^ Printexc.to_string e

(* How many inputs parsed, so reached the checker. *)
let parsed = ref 0

(* The lexer and the parser on their own, then the loader and the checker
   as `limen check` runs them. *)
let check_ocaml secs path src =
  let stage = ref "lexer" in
  match
    limited secs (fun () ->
        (try ignore (Lexer.lex path src) with Diag.Fatal d -> if not (good_code d.code) then failwith ("lexer diagnostic code " ^ d.code));
        stage := "parser";
        (try ignore (Parser.parse_file path src); incr parsed
         with Diag.Fatal d -> if not (good_code d.code) then failwith ("parser diagnostic code " ^ d.code));
        stage := "checker";
        let _, ds, _ = Modules.load_and_check path src in
        List.iter (fun (d : Diag.t) -> if not (good_code d.code) then failwith ("diagnostic code " ^ d.code)) ds;
        Diag.errors ds <> [])
  with
  | has -> Fine has
  | exception e -> Problem (sprintf "%s: %s" !stage (describe_exn e))

(* Run a program in [dir] with a time limit; its exit status, or None when
   it was killed for taking too long. Its stdout and stderr go to [out]. *)
let run_limited secs dir prog args out =
  let fd = Unix.openfile out [ Unix.O_WRONLY; O_CREAT; O_TRUNC; O_CLOEXEC ] 0o644 in
  let env = Array.append [| "PWD=" ^ dir |] (Array.of_list (List.filter (fun v -> not (String.starts_with ~prefix:"PWD=" v)) (Array.to_list (Unix.environment ())))) in
  let here = Sys.getcwd () in
  Sys.chdir dir;
  let pid = Fun.protect (fun () -> Unix.create_process_env prog (Array.append [| prog |] args) env Unix.stdin fd fd) ~finally:(fun () -> Sys.chdir here) in
  Unix.close fd;
  let deadline = Unix.gettimeofday () +. secs in
  let rec wait () =
    match Unix.waitpid [ Unix.WNOHANG ] pid with
    | 0, _ ->
      if Unix.gettimeofday () > deadline then begin
        (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
        ignore (Unix.waitpid [] pid);
        None
      end
      else (Unix.sleepf 0.001; wait ())
    | _, st -> Some st
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> wait ()
  in
  wait ()

let tail text =
  let lines = List.filter (( <> ) "") (String.split_on_char '\n' text) in
  let n = List.length lines in
  String.concat " | " (List.filteri (fun i _ -> i >= n - 3) lines)

let status = function
  | Unix.WEXITED c -> sprintf "exit %d" c
  | WSIGNALED s -> sprintf "signal %d" s
  | WSTOPPED s -> sprintf "stopped %d" s

(* ---------- the run ---------- *)

let () =
  let seed = ref (int_of_float (Unix.gettimeofday ()) land 0xffffff) in
  let count = ref 1000 in
  let secs = ref 30. in
  let selfhost = ref None and exact = ref None and save = ref None and work = ref None and replay = ref false in
  let paths = ref [] in
  Arg.parse
    [ ("--seed", Arg.Set_int seed, "N  the random seed");
      ("--count", Arg.Set_int count, "N  how many mutants");
      ("--time", Arg.Set_float secs, "S  the time limit for one input, in seconds (30)");
      ("--selfhost", Arg.String (fun s -> selfhost := Some s), "BIN  the self-hosted compiler, selfhost/limen.lmn built");
      ("--exact", Arg.String (fun s -> exact := Some s), "LIMEN  compare everything `limen check` prints with the self-hosted compiler's");
      ("--save", Arg.String (fun s -> save := Some s), "DIR  keep each problem's input here");
      ("--work", Arg.String (fun s -> work := Some s), "DIR  where mutants are written");
      ("--replay", Arg.Set replay, "  check the files named, as they are") ]
    (fun p -> paths := p :: !paths)
    "fuzz_front [OPTIONS] PATH...";
  let abs p = if Filename.is_relative p then Filename.concat (Sys.getcwd ()) p else p in
  let files = List.rev (List.fold_left lmn_files [] (List.rev_map abs !paths)) in
  if files = [] then (prerr_endline "fuzz_front: no .lmn files"; exit 2);
  let temporary = !work = None in
  let work =
    match !work with
    | Some w -> (try Unix.mkdir w 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()); abs w
    | None ->
      let w = Filename.concat (Filename.get_temp_dir_name ()) (sprintf "limen-fuzz-front-%d" (Unix.getpid ())) in
      (try Unix.mkdir w 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
      w
  in
  (* as the self-hosted compiler sees it, through its grant *)
  let work = Unix.realpath work in
  Option.iter (fun d -> try Unix.mkdir d 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()) !save;
  (* each directory of the corpus, copied once into the work directory: its
     files, not its subdirectories *)
  let dirs = Hashtbl.create 16 and dir_names = ref [] in
  let dir_of path =
    let d = Filename.dirname path in
    match Hashtbl.find_opt dirs d with
    | Some k -> k
    | None ->
      let k = Hashtbl.length dirs in
      Hashtbl.replace dirs d k;
      let into = Filename.concat work (sprintf "d%d" k) in
      (try Unix.mkdir into 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
      if not !replay then
        Array.iter
          (fun f ->
            let p = Filename.concat d f in
            if (not (Sys.is_directory p)) && (Unix.stat p).st_size < 1_000_000 then write_file (Filename.concat into f) (read_file p))
          (Sys.readdir d);
      dir_names := (k, d) :: !dir_names;
      k
  in
  let corpus =
    Array.of_list
      (List.map
         (fun path ->
           let src = read_file path in
           let spans, kinds = tokens_of path src in
           { path; src; dir = dir_of path; base = Filename.basename path; spans; kinds })
         files)
  in
  let collect kind =
    Array.of_list
      (List.sort_uniq compare
         (List.concat_map
            (fun p -> List.filteri (fun i _ -> p.kinds.(i) = kind) (Array.to_list (Array.map (fun (lo, hi) -> String.sub p.src lo (hi - lo)) p.spans)))
            (Array.to_list corpus)))
  in
  names := collect 'n';
  literals := collect 'l';
  let rand = Random.State.make [| !seed |] in
  let n = if !replay then Array.length corpus else !count in
  Printf.printf "seed %d, %d programs in the corpus\n%!" !seed (Array.length corpus);
  let problems = ref 0 and differ = ref 0 and with_errors = ref 0 and shown = ref 0 in
  let report kind input msg =
    incr problems;
    let saved =
      match !save with
      | None -> ""
      | Some d ->
        let f = Filename.concat d (sprintf "%s-%d-%d.lmn" kind !seed !problems) in
        write_file f input;
        write_file (f ^ ".txt") (msg ^ "\n");
        " (saved as " ^ f ^ ")"
    in
    if !shown < 30 then begin
      incr shown;
      Printf.printf "%s: %s%s\n%!" (String.uppercase_ascii kind) msg saved
    end
  in
  let note_diff input msg =
    incr differ;
    report "differs" input (String.sub msg 0 (min 2000 (String.length msg)))
  in
  (* once more when it was too slow: only slow twice is a problem *)
  let again run = match run () with None -> run () | r -> r in
  for i = 0 to n - 1 do
    let p = if !replay then corpus.(i) else pick rand corpus in
    let input = if !replay then p.src else mutate rand corpus p in
    let rel = if !replay then Filename.dirname p.path else sprintf "d%d" p.dir in
    let path = if !replay then p.path else Filename.concat (Filename.concat work rel) p.base in
    if not !replay then write_file path input;
    let what = sprintf "#%d, from %s" i p.path in
    let ocaml =
      match check_ocaml !secs path input with
      | Problem m when String.ends_with ~suffix:"time limit" m -> check_ocaml !secs path input
      | r -> r
    in
    (match ocaml with
     | Problem m -> report "ocaml" input (sprintf "%s: %s" what m)
     | Fine has -> (
       if has then incr with_errors;
       match !selfhost with
       | None -> ()
       | Some bin -> (
         let dir = Filename.dirname path in
         let grant = if !replay then "/" else work in
         let out = Filename.concat work "selfhost.out" in
         match again (fun () -> run_limited !secs dir bin [| "--grant"; "fs=" ^ grant ^ ":ro"; "--grant"; "env=PWD"; "--"; "check"; p.base |] out) with
         | None -> report "selfhost" input (sprintf "%s: the self-hosted compiler took longer than %gs" what !secs)
         | Some (Unix.WEXITED (0 | 1 as code)) -> (
           let got = read_file out in
           if (code = 1) <> has then
             report "disagree" input
               (sprintf "%s: the OCaml checker finds %s, the self-hosted one %s: %s" what (if has then "errors" else "none")
                  (if has then "none" else "errors") (tail got))
           else
             match !exact with
             | None -> ()
             | Some limen -> (
               let want_out = Filename.concat work "limen.out" in
               match again (fun () -> run_limited !secs dir limen [| "check"; p.base |] want_out) with
               | Some st ->
                 let want = read_file want_out in
                 if want <> got || st <> Unix.WEXITED code then
                   note_diff input (sprintf "%s\n--- limen check (%s)\n%s\n--- self-hosted (exit %d)\n%s" what (status st) want code got)
               | None -> ()))
         | Some st -> report "selfhost" input (sprintf "%s: the self-hosted compiler crashed (%s): %s" what (status st) (tail (read_file out))))));
    if not !replay then write_file path p.src
  done;
  if !exact <> None then Printf.printf "%d inputs print differently in the two compilers\n" !differ;
  if temporary then ignore (Sys.command (Filename.quote_command "rm" [ "-rf"; work ]));
  Printf.printf "%d inputs, %d parsed, %d with errors, %d problems\n" n !parsed !with_errors !problems;
  exit (if !problems = 0 then 0 else 1)
