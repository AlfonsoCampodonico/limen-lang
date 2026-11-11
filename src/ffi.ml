(* Foreign functions: what crosses the boundary to C, shared by the checker
   and both backends.

   A `foreign "lib" { fn name(p: T, ...) -> R }` declaration names a C
   function in library [lib]. Each parameter and the result has one of these
   boundary types:

     Vela type      C type          notes
     Int            int64_t
     Int32          int32_t         an Int in Vela; out of range panics
     Float          double
     Bool           int             0 is false
     Str            const char *    the bytes, then a NUL; valid during the call only
     Bytes          const void *    likewise; pass the length yourself
     Handle         void *          an opaque pointer, `Handle { addr }` in Vela
     Out[T]         T *             T is Int, Int32, Float or Handle: an out-parameter

   A Str result is copied from a NUL-terminated C string (NULL gives ""). A
   function's Vela type takes a Foreign capability first, then the parameters
   that are not Out; its value is the C result (unless it returns nothing)
   followed by the Out values, as a tuple when there is more than one. *)
open Types

type cty = Int | Int32 | Float | Bool | Str | Bytes | Handle | Unit

type param = { name : string; cty : cty; out : bool }

(* [trusted]: from a standard library module trusted to call C (see
   Ast.foreign); no Foreign capability is passed or checked. *)
type csig = { lib : string; sym : string; params : param list; ret : cty; trusted : bool }

(* The calling convention both backends rely on (and the interpreter's
   generic call stub implements): at most 8 integer-class and 8 floating
   point arguments. *)
let max_ints = 8
let max_floats = 8

let handle_ty = TNamed ("Handle", [])

let vela_ty = function
  | Int | Int32 -> TInt
  | Float -> TFloat
  | Bool -> TBool
  | Str -> TStr
  | Bytes -> TBytes
  | Handle -> handle_ty
  | Unit -> TUnit

let c_name = function
  | Int -> "int64_t"
  | Int32 -> "int32_t"
  | Float -> "double"
  | Bool -> "int"
  | Str -> "const char *"
  | Bytes -> "const void *"
  | Handle -> "void *"
  | Unit -> "void"

(* The values a call produces, in order: the result, then the out-parameters. *)
let results s = (if s.ret = Unit then [] else [ s.ret ]) @ List.filter_map (fun p -> if p.out then Some p.cty else None) s.params

let result_ty s =
  match results s with
  | [] -> TUnit
  | [ t ] -> vela_ty t
  | ts -> TNamed (Ast.tuple_name (List.length ts), List.map vela_ty ts)

let visible_params s =
  let ps = List.filter_map (fun p -> if p.out then None else Some (vela_ty p.cty)) s.params in
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

let boundary_types = "Int, Int32, Float, Bool, Str, Bytes, Handle, or Out[Int | Int32 | Float | Handle] for a parameter"

(* The runtime's helpers for trusted standard library modules, with the
   signature each must be declared with.
   - vela_copy(p, n): n bytes of C memory at p, as Bytes.
   - vela_handle_new(p) -> id: put a C pointer in the runtime's handle table
     under a fresh random id, so Vela values hold an id that cannot be forged
     into a pointer. vela_handle_acquire(id) gives the pointer back (NULL if
     the id is unknown or closed) and counts a use in progress;
     vela_handle_release(id) ends that use. vela_handle_close(id) closes the
     id; the pointer comes back (to free) from whichever of close and the
     last release happens last, and NULL from the others. *)
let runtime_fns =
  [ ("vela_copy", ([ Handle; Int ], Bytes)); ("vela_handle_new", ([ Handle ], Int)); ("vela_handle_acquire", ([ Int ], Handle));
    ("vela_handle_release", ([ Int ], Handle)); ("vela_handle_close", ([ Int ], Handle)) ]

(* The C signature of a foreign declaration, or the errors in it. *)
let of_decl (d : Ast.fn_decl) : (csig, (Span.t * string) list) result =
  let lib, sym, trusted = match d.fn_foreign with Some fo -> (fo.flib, fo.fsym, fo.ftrusted) | None -> invalid_arg "Ffi.of_decl" in
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
        | _ -> (
          match base te with
          | Some t -> { name = p.prm_name; cty = t; out = false }
          | None ->
            err te.tspan ("this type cannot cross to C; use " ^ boundary_types);
            { name = p.prm_name; cty = Int; out = false }))
      d.fn_params
  in
  (* "vela" is the runtime's own library of helpers for trusted standard
     library modules (see runtime_fns) *)
  let runtime_lib = lib = "vela" in
  if runtime_lib then begin
    if not trusted then err d.fn_span "library `vela` is the runtime's own, for the standard library; use `foreign.copy(p, n)` instead";
    if not (List.mem_assoc sym runtime_fns) then
      err d.fn_span (Printf.sprintf "library `vela` has no function `%s`; it has %s" sym (String.concat ", " (List.map fst runtime_fns)))
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
      | Some t -> t
      | None ->
        err te.tspan "a foreign function returns Int, Int32, Float, Bool, Str, Handle or nothing";
        Unit)
  in
  if not (lib_ok lib) then err d.fn_span (Printf.sprintf "`%s` is not a library name: use letters, digits, `_`, `.`, `+` and `-`" lib);
  if not (ident_ok sym) then err d.fn_span (Printf.sprintf "`%s` is not a C function name" sym);
  let ints = List.length (List.filter (fun p -> p.out || p.cty <> Float) params) in
  let floats = List.length (List.filter (fun p -> (not p.out) && p.cty = Float) params) in
  if ints > max_ints || floats > max_floats then
    err d.fn_span
      (Printf.sprintf "a foreign function takes at most %d integer-like and %d Float parameters (an Out counts as integer-like)" max_ints
         max_floats);
  (match List.assoc_opt sym runtime_fns with
   | Some (ps, r) when runtime_lib && (List.map (fun p -> if p.out then Unit else p.cty) params <> ps || ret <> r) ->
     err d.fn_span (Printf.sprintf "`%s` is declared with the wrong types for the runtime's helper" sym)
   | _ -> ());
  match !errs with [] -> Ok { lib; sym; params; ret; trusted } | es -> Error (List.rev es)

(* Libraries a program's foreign declarations name, each once, in order. *)
let libs (items : Ast.item list) =
  List.fold_left
    (fun acc it -> match it with Ast.IFn { fn_foreign = Some { flib = l; _ }; _ } when l <> "vela" && not (List.mem l acc) -> acc @ [ l ] | _ -> acc)
    [] items

let denied_msg lib = Printf.sprintf "foreign: library `%s` is not granted (grant it with --grant foreign=%s)" lib lib
