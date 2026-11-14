(* Foreign functions: what crosses the boundary to C, shared by the checker
   and both backends.

   A `foreign "lib" { fn name(p: T, ...) -> R }` declaration names a C
   function in library [lib]. Each parameter and the result has one of these
   boundary types:

     Tessera type      C type          notes
     Int            int64_t
     Int32          int32_t         an Int in Tessera; out of range panics
     Float          double
     Bool           int             0 is false
     Str            const char *    the bytes, then a NUL; valid during the call only
     Bytes          const void *    likewise; pass the length yourself
     Handle         void *          an opaque pointer, `Handle { addr }` in Tessera
     Out[T]         T *             T is Int, Int32, Float or Handle: an out-parameter
     a C struct     struct { ... }  declared in the block, passed and returned by value
     fn(A) -> R     function ptr    a callback: a top-level Tessera function C calls

   A Str result is copied from a NUL-terminated C string (NULL gives ""). A
   function's Tessera type takes a Foreign capability first, then the parameters
   that are not Out; its value is the C result (unless it returns nothing)
   followed by the Out values, as a tuple when there is more than one.

   A `...` among the parameters makes the function variadic: the parameters
   before it are the C function's own, the ones after it the variadic
   arguments this declaration passes (C's default promotions apply: Int32
   and Bool go as int, Float as double). *)
open Types

type cty =
  | Int
  | Int32
  | Float
  | Bool
  | Str
  | Bytes
  | Handle
  | Unit
  | Struct of string * (string * cty) list (* a C struct by value: its Tessera name and its fields *)
  | Fn of cty list * cty (* a callback: C calls a top-level Tessera function through it *)

type param = { name : string; cty : cty; out : bool }

(* [trusted]: from a standard library module trusted to call C (see
   Ast.foreign); no Foreign capability is passed or checked. [fixed]: for a
   variadic function, how many parameters come before the `...`. *)
type csig = { lib : string; sym : string; params : param list; ret : cty; trusted : bool; fixed : int option }

(* What the interpreter's call stubs (ffi_stubs.c) can pass, and so the
   limits of both backends: at most 8 integer-like and 8 Float values, each
   field of a struct counting as one; a struct has 1 to 8 fields. A variadic
   function has 1 to 6 integer-like parameters before the `...` and at most
   8 after it. A callback takes at most 8 integer-like and 8 Float
   parameters. *)
let max_ints = 8
let max_floats = 8
let max_fields = 8
let max_fixed = 6
let max_varargs = 8

let handle_ty = TNamed ("Handle", [])

let rec tessera_ty = function
  | Int | Int32 -> TInt
  | Float -> TFloat
  | Bool -> TBool
  | Str -> TStr
  | Bytes -> TBytes
  | Handle -> handle_ty
  | Unit -> TUnit
  | Struct (n, _) -> TNamed (n, [])
  | Fn (ps, r) -> TFn (List.map tessera_ty ps, tessera_ty r)

let c_name = function
  | Int -> "int64_t"
  | Int32 -> "int32_t"
  | Float -> "double"
  | Bool -> "int"
  | Str -> "const char *"
  | Bytes -> "const void *"
  | Handle -> "void *"
  | Unit -> "void"
  | Struct _ | Fn _ -> invalid_arg "Ffi.c_name" (* named by the native backend *)

(* The integer-like and Float values a parameter list passes, a struct's
   fields each counting as one. *)
let counts (ps : param list) =
  List.fold_left
    (fun (i, f) p ->
      if p.out then (i + 1, f)
      else
        match p.cty with
        | Float -> (i, f + 1)
        | Struct (_, fs) -> List.fold_left (fun (i, f) (_, t) -> if t = Float then (i, f + 1) else (i + 1, f)) (i, f) fs
        | _ -> (i + 1, f))
    (0, 0) ps

(* A struct's C layout: each field at its natural alignment (4 bytes for
   Int32 and Bool, 8 for the rest), the size rounded up to the largest. *)
let field_size = function Int32 | Bool -> 4 | _ -> 8

let layout (fs : (string * cty) list) =
  let off = ref 0 and align = ref 1 in
  let offs =
    List.map
      (fun (_, t) ->
        let a = field_size t in
        align := max !align a;
        off := (!off + a - 1) / a * a;
        let o = !off in
        off := !off + a;
        o)
      fs
  in
  (offs, (!off + !align - 1) / !align * !align)

(* The values a call produces, in order: the result, then the out-parameters. *)
let results s = (if s.ret = Unit then [] else [ s.ret ]) @ List.filter_map (fun p -> if p.out then Some p.cty else None) s.params

let result_ty s =
  match results s with
  | [] -> TUnit
  | [ t ] -> tessera_ty t
  | ts -> TNamed (Ast.tuple_name (List.length ts), List.map tessera_ty ts)

let visible_params s =
  let ps = List.filter_map (fun p -> if p.out then None else Some (tessera_ty p.cty)) s.params in
  if s.trusted then ps else TCap Foreign :: ps

(* A library name is a plain word: linked as -l<name>, loaded as lib<name>.so
   or lib<name>.dylib. "c" is the C library itself. *)
let lib_ok l =
  l <> ""
  && l.[0] <> '-'
  && String.for_all (fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_' || c = '.' || c = '+' || c = '-') l

let ident_ok s =
  s <> ""
  && (not (s.[0] >= '0' && s.[0] <= '9'))
  && String.for_all (fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_') s

let boundary_types =
  "Int, Int32, Float, Bool, Str, Bytes, Handle, a struct declared in a foreign block, a callback `fn(...) -> T`, or Out[Int | Int32 | Float | Handle] for a parameter"

let field_cty (te : Ast.ty_expr) =
  match te.tdesc with
  | TyName ("Int", []) -> Some Int
  | TyName ("Int32", []) -> Some Int32
  | TyName ("Float", []) -> Some Float
  | TyName ("Bool", []) -> Some Bool
  | TyName ("Handle", []) -> Some Handle
  | _ -> None

(* The fields of a struct declared in a foreign block, or the errors in it. *)
let struct_fields (s : Ast.struct_decl) : ((string * cty) list, (Span.t * string) list) result =
  let tys = match s.st_foreign with Some (_, ts) -> ts | None -> invalid_arg "Ffi.struct_fields" in
  let errs = ref [] in
  let fs =
    List.filter_map
      (fun ((f, _, _), te) ->
        match field_cty te with
        | Some t -> Some (f, t)
        | None ->
          errs := (te.Ast.tspan, "a field of a C struct is Int, Int32, Float, Bool or Handle") :: !errs;
          None)
      (List.combine s.st_fields tys)
  in
  if s.st_fields = [] then errs := (s.st_span, "a C struct needs at least one field") :: !errs;
  if List.length s.st_fields > max_fields then
    errs := (s.st_span, Printf.sprintf "a C struct has at most %d fields" max_fields) :: !errs;
  match !errs with [] -> Ok fs | es -> Error (List.rev es)

(* The runtime's helpers for trusted standard library modules, with the
   signature each must be declared with.
   - tessera_copy(p, n): n bytes of C memory at p, as Bytes.
   - tessera_handle_new(p) -> id: put a C pointer in the runtime's handle table
     under a fresh random id, so Tessera values hold an id that cannot be forged
     into a pointer. tessera_handle_acquire(id) gives the pointer back (NULL if
     the id is unknown or closed) and counts a use in progress;
     tessera_handle_release(id) ends that use. tessera_handle_close(id) closes the
     id; the pointer comes back (to free) from whichever of close and the
     last release happens last, and NULL from the others. *)
let runtime_fns =
  [ ("tessera_copy", ([ Handle; Int ], Bytes)); ("tessera_handle_new", ([ Handle ], Int)); ("tessera_handle_acquire", ([ Int ], Handle));
    ("tessera_handle_release", ([ Int ], Handle)); ("tessera_handle_close", ([ Int ], Handle)) ]

(* The C signature of a foreign declaration, or the errors in it. [structs]
   finds a struct declaration by its (qualified) name. *)
let of_decl ?(structs : string -> Ast.struct_decl option = fun _ -> None) (d : Ast.fn_decl) : (csig, (Span.t * string) list) result =
  let lib, sym, trusted, variadic =
    match d.fn_foreign with Some fo -> (fo.flib, fo.fsym, fo.ftrusted, fo.fvariadic) | None -> invalid_arg "Ffi.of_decl"
  in
  let errs = ref [] in
  let err sp m = errs := (sp, m) :: !errs in
  let base (te : Ast.ty_expr) =
    match te.tdesc with
    | TyName ("Int", []) -> Some Int
    | TyName ("Int32", []) -> Some Int32
    | TyName ("Float", []) -> Some Float
    | TyName ("Bool", []) -> Some Bool
    | TyName ("Str", []) -> Some Str
    | TyName ("Bytes", []) -> Some Bytes
    | TyName ("Handle", []) -> Some Handle
    | TyName (n, []) -> (
      match structs n with
      | Some ({ st_foreign = Some _; _ } as s) ->
        (* a bad field is reported at the struct *)
        Some (Struct (n, match struct_fields s with Ok fs -> fs | Error _ -> [ ("0", Int) ]))
      | Some _ ->
        err te.tspan
          (Printf.sprintf "`%s` is not a C struct: a struct passed to C is declared in the foreign block, as in `struct %s { x: Int }`"
             (Option.value ~default:n (List.nth_opt (List.rev (String.split_on_char '.' n)) 0))
             (Option.value ~default:n (List.nth_opt (List.rev (String.split_on_char '.' n)) 0)));
        Some Int
      | None -> None)
    | _ -> None
  in
  (* a callback's parameter or result *)
  let callback (te : Ast.ty_expr) =
    match te.tdesc with
    | TyFn (ps, r) ->
      let ps =
        List.map
          (fun (p : Ast.ty_expr) ->
            match base p with
            | Some ((Int | Int32 | Float | Bool | Str | Handle) as t) -> t
            | _ ->
              err p.tspan "a callback's parameter is Int, Int32, Float, Bool, Str or Handle";
              Int)
          ps
      in
      let r =
        match r.tdesc with
        | TyUnit -> Unit
        | _ -> (
          match base r with
          | Some ((Int | Int32 | Float | Bool | Handle) as t) -> t
          | Some Str -> err r.tspan "a callback cannot return Str: C would not know who frees it"; Unit
          | _ -> err r.tspan "a callback returns Int, Int32, Float, Bool, Handle or nothing"; Unit)
      in
      let fl = List.length (List.filter (fun t -> t = Float) ps) in
      if List.length ps - fl > max_ints || fl > max_floats then
        err te.tspan (Printf.sprintf "a callback takes at most %d integer-like and %d Float parameters" max_ints max_floats);
      Some (Fn (ps, r))
    | _ -> None
  in
  let params =
    List.map
      (fun (p : Ast.param) ->
        let te = p.prm_ty in
        match te.tdesc with
        | TyName ("Out", [ inner ]) -> (
          match base inner with
          | Some ((Int | Int32 | Float | Handle) as t) -> { name = p.prm_name; cty = t; out = true }
          | _ ->
            err inner.tspan "an `Out` parameter holds an Int, Int32, Float or Handle";
            { name = p.prm_name; cty = Int; out = true })
        | TyFn _ -> { name = p.prm_name; cty = Option.get (callback te); out = false }
        | _ -> (
          match base te with
          | Some t -> { name = p.prm_name; cty = t; out = false }
          | None ->
            err te.tspan ("this type cannot cross to C; use " ^ boundary_types);
            { name = p.prm_name; cty = Int; out = false }))
      d.fn_params
  in
  (* "tessera" is the runtime's own library of helpers for trusted standard
     library modules (see runtime_fns) *)
  let runtime_lib = lib = "tessera" in
  if runtime_lib then begin
    if not trusted then err d.fn_span "library `tessera` is the runtime's own, for the standard library; use `foreign.copy(p, n)` instead";
    if not (List.mem_assoc sym runtime_fns) then
      err d.fn_span (Printf.sprintf "library `tessera` has no function `%s`; it has %s" sym (String.concat ", " (List.map fst runtime_fns)))
  end;
  let ret =
    match d.fn_ret with
    | None -> Unit
    | Some { tdesc = TyUnit; _ } -> Unit
    | Some te -> (
      match base te with
      | Some Bytes when runtime_lib -> Bytes
      | Some Bytes ->
        err te.tspan "a foreign function cannot return Bytes: C gives no length; return a Handle and copy with `foreign.copy(p, n)`";
        Unit
      | Some (Struct _) when variadic <> None -> err te.tspan "a variadic function cannot return a struct"; Unit
      | Some t -> t
      | None ->
        (match te.tdesc with
         | TyFn _ -> err te.tspan "a foreign function cannot return a function"
         | _ -> err te.tspan "a foreign function returns Int, Int32, Float, Bool, Str, Handle, a C struct or nothing");
        Unit)
  in
  if not (lib_ok lib) then err d.fn_span (Printf.sprintf "`%s` is not a library name: use letters, digits, `_`, `.`, `+` and `-`" lib);
  if not (ident_ok sym) then err d.fn_span (Printf.sprintf "`%s` is not a C function name" sym);
  (match variadic with
   | None ->
     let ints, floats = counts params in
     if ints > max_ints || floats > max_floats then
       err d.fn_span
         (Printf.sprintf
            "a foreign function takes at most %d integer-like and %d Float values (an Out counts as integer-like, and each field of a struct as one)"
            max_ints max_floats)
   | Some k ->
     let fixed = List.filteri (fun i _ -> i < k) params and rest = List.filteri (fun i _ -> i >= k) params in
     let spans = Array.of_list (List.map (fun (p : Ast.param) -> p.prm_ty.tspan) d.fn_params) in
     if k = 0 then err d.fn_span "a variadic function needs a parameter before `...`, as in C";
     if k > max_fixed then err d.fn_span (Printf.sprintf "a variadic function takes at most %d parameters before `...`" max_fixed);
     List.iteri
       (fun i p ->
         match p.cty with
         | (Float | Struct _) when not p.out ->
           err spans.(i) "a parameter before `...` is integer-like: not a Float or a struct"
         | _ -> ())
       fixed;
     if List.length rest > max_varargs then err d.fn_span (Printf.sprintf "a variadic function passes at most %d arguments after `...`" max_varargs);
     List.iteri
       (fun i p ->
         match p.cty with
         | (Struct _ | Fn _) when not p.out -> err spans.(k + i) "an argument after `...` is Int, Int32, Float, Bool, Str, Bytes or Handle"
         | _ when p.out -> err spans.(k + i) "an `Out` parameter goes before `...`"
         | _ -> ())
       rest);
  (match List.assoc_opt sym runtime_fns with
   | Some (ps, r) when runtime_lib && (List.map (fun p -> if p.out then Unit else p.cty) params <> ps || ret <> r) ->
     err d.fn_span (Printf.sprintf "`%s` is declared with the wrong types for the runtime's helper" sym)
   | _ -> ());
  match !errs with [] -> Ok { lib; sym; params; ret; trusted; fixed = variadic } | es -> Error (List.rev es)

(* Libraries a program's foreign declarations name, each once, in order. *)
let libs (items : Ast.item list) =
  List.fold_left
    (fun acc it -> match it with Ast.IFn { fn_foreign = Some { flib = l; _ }; _ } when l <> "tessera" && not (List.mem l acc) -> acc @ [ l ] | _ -> acc)
    [] items

let denied_msg lib = Printf.sprintf "foreign: library `%s` is not granted (grant it with --grant foreign=%s)" lib lib
