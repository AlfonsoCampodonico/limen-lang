(* Struct embedding: `struct Admin { User, level: Int }`.

   An embedded struct is an ordinary field named after its type (`User`), so
   the backends see nothing new. Two things make it embedding:

   - Promotion. The checker resolves `admin.name` and `admin.greet()` with
     [lookup] and rewrites them to the explicit paths `admin.User.name` and
     `admin.User.greet()`.
   - Forwarders. So that `Admin` satisfies interfaces through promoted
     methods (bounds `[T: Greeter]` and `dyn Greeter` vtables), [forwarders]
     adds, for each promoted method, a method `Admin::greet` whose body is
     `self.User.greet(...)`. They are generated when modules load, before
     checking, and are checked like any other function.

   Lookup rule (as in Go): the outer struct's own fields and methods come
   first; then the embedded structs, one depth at a time. The first depth
   where the name exists wins. If two structs at that depth have it, the name
   is ambiguous, which is an error only where it is used. *)
open Ast

(* The struct type an embedded field holds: its name and type arguments. *)
let embed_type (s : struct_decl) f =
  match List.find_opt (fun (n, _, _) -> n = f) s.st_fields with
  | Some (_, _, { tdesc = TyName (n, args); _ }) -> Some (n, args)
  | _ -> None

type found =
  | Own (* the struct itself has it *)
  | Path of string list * string (* the embedded fields to go through, and the struct that has it *)
  | Ambiguous of string list list (* the paths of the structs at the nearest depth that have it *)
  | Missing

(* Find a member of struct [n]. [has t] says whether struct [t] itself
   declares the member. *)
let lookup ~(structs : string -> struct_decl option) ~(has : string -> bool) n : found =
  if has n then Own
  else
    let embeds_of (t, path) =
      match structs t with
      | None -> []
      | Some s ->
        List.filter_map
          (fun f -> match embed_type s f with Some (tn, _) when structs tn <> None -> Some (tn, path @ [ f ]) | _ -> None)
          s.st_embeds
    in
    (* [seen]: the struct types at shallower depths, so a struct that embeds
       itself (an error reported elsewhere) cannot loop *)
    let rec level seen xs =
      let next = List.filter (fun (t, _) -> not (List.mem t seen)) (List.concat_map embeds_of xs) in
      if next = [] then Missing
      else
        match List.filter (fun (t, _) -> has t) next with
        | [ (t, p) ] -> Path (p, t)
        | [] -> level (seen @ List.map fst next) next
        | many -> Ambiguous (List.map snd many)
    in
    level [ n ] [ (n, []) ]

(* ---------- forwarders ---------- *)

(* Forwarder name -> the method it calls. *)
let forwarder_of : (string, string) Hashtbl.t = Hashtbl.create 16
let is_forwarder key = Hashtbl.mem forwarder_of key

let rec strip (t : ty_expr) =
  let tdesc =
    match t.tdesc with
    | TyName (n, args) -> TyName (n, List.map strip args)
    | TyFn (ps, r) -> TyFn (List.map strip ps, strip r)
    | d -> d
  in
  { tdesc; tspan = Span.dummy }

let rec subst env (t : ty_expr) =
  match t.tdesc with
  | TyName (n, []) when List.mem_assoc n env -> List.assoc n env
  | TyName (n, args) -> { t with tdesc = TyName (n, List.map (subst env) args) }
  | TyFn (ps, r) -> { t with tdesc = TyFn (List.map (subst env) ps, subst env r) }
  | _ -> t

let method_name key = match String.rindex_opt key ':' with Some i -> String.sub key (i + 1) (String.length key - i - 1) | None -> key

(* The method of struct [s] that calls [d] (a method of the struct reached
   through [path]), if it can be written: a method on one instantiation only
   (`fn f(self: Box[Int])`) cannot be forwarded from a generic struct. *)
let forwarder structs (s : struct_decl) path (d : fn_decl) : fn_decl option =
  let sp = d.fn_span in
  let tyn n args = { tdesc = TyName (n, args); tspan = sp } in
  (* the embedded value's type, in terms of [s]'s type parameters *)
  let rec walk (cur : struct_decl) env = function
    | [] -> Some (tyn cur.st_name (List.map (fun g -> subst env (tyn g [])) cur.st_gen))
    | f :: rest -> (
      match embed_type cur f with
      | Some (tn, args) -> (
        match structs tn with
        | Some (next : struct_decl) when List.length next.st_gen = List.length args ->
          walk next (List.combine next.st_gen (List.map (subst env) args)) rest
        | _ -> None)
      | None -> None)
  in
  let fail = ref false in
  let mapping = ref [] in
  (match (walk s [] path, d.fn_params) with
   | Some { tdesc = TyName (_, eargs); _ }, { prm_ty = { tdesc = TyName (_, sargs); _ }; _ } :: _ when List.length eargs = List.length sargs ->
     List.iter2
       (fun (sa : ty_expr) (ea : ty_expr) ->
         match sa.tdesc with
         | TyName (g, []) when List.mem g d.fn_gen -> (
           match List.assoc_opt g !mapping with
           | Some prev -> if strip prev <> strip ea then fail := true
           | None -> mapping := (g, ea) :: !mapping)
         | _ -> if strip sa <> strip ea then fail := true)
       sargs eargs
   | _ -> fail := true);
  (* the method's other type parameters, renamed away from the struct's *)
  let rest = List.filter (fun g -> not (List.mem_assoc g !mapping)) d.fn_gen in
  let rec fresh g i = let c = g ^ string_of_int i in if List.mem c s.st_gen || List.mem c d.fn_gen then fresh g (i + 1) else c in
  let renamed = List.map (fun g -> (g, if List.mem g s.st_gen then fresh g 2 else g)) rest in
  let env = !mapping @ List.map (fun (g, g') -> (g, tyn g' [])) renamed in
  let bounds =
    List.map
      (fun (g, bs) ->
        match List.assoc_opt g renamed with
        | Some g' -> (g', bs)
        | None -> (
          match List.assoc_opt g !mapping with
          | Some { tdesc = TyName (g', []); _ } when List.mem g' s.st_gen -> (g', bs)
          | _ -> fail := true; (g, bs)))
      d.fn_bounds
  in
  let bounds =
    List.fold_left
      (fun acc (g, bs) -> match List.assoc_opt g acc with Some prev -> (g, prev @ bs) :: List.remove_assoc g acc | None -> acc @ [ (g, bs) ])
      [] bounds
  in
  if !fail then None
  else begin
    let mk e = { edesc = e; espan = sp; ety = Types.TErr; einst = []; ecallee = None } in
    let params = List.tl d.fn_params in
    let recv = List.fold_left (fun r f -> mk (EField (r, f, sp))) (mk (EVar "self")) path in
    let call = mk (EMethod (recv, method_name d.fn_name, sp, List.map (fun p -> mk (EVar p.prm_name)) params)) in
    Some
      { d with
        fn_name = s.st_name ^ "::" ^ method_name d.fn_name;
        fn_gen = s.st_gen @ List.map snd renamed;
        fn_params =
          { prm_name = "self"; prm_span = sp; prm_ty = tyn s.st_name (List.map (fun g -> tyn g []) s.st_gen) }
          :: List.map (fun p -> { p with prm_ty = subst env p.prm_ty }) params;
        fn_ret = Option.map (subst env) d.fn_ret;
        fn_body = { stmts = []; tail = Some call; bspan = sp };
        fn_test = None;
        fn_bounds = bounds;
        fn_foreign = None }
  end

(* The forwarders for every method promoted onto a struct of [items]. *)
let forwarders (items : item list) : item list =
  Hashtbl.reset forwarder_of;
  let structs = Hashtbl.create 16 and methods = Hashtbl.create 32 in
  List.iter
    (function
      | IStruct s -> Hashtbl.replace structs s.st_name s
      | IFn d when d.fn_test = None && String.contains d.fn_name ':' -> Hashtbl.replace methods d.fn_name d
      | _ -> ())
    items;
  let find = Hashtbl.find_opt structs in
  List.concat_map
    (function
      | IStruct s when s.st_embeds <> [] ->
        (* every method of every struct reachable through embedding *)
        let rec reach seen t =
          if List.mem t seen then seen
          else
            match find t with
            | None -> seen
            | Some st ->
              List.fold_left (fun seen f -> match embed_type st f with Some (tn, _) -> reach seen tn | None -> seen) (t :: seen) st.st_embeds
        in
        let below = List.filter (fun t -> t <> s.st_name) (reach [] s.st_name) in
        let names =
          List.sort_uniq compare
            (Hashtbl.fold
               (fun k _ acc ->
                 if List.exists (fun t -> String.starts_with ~prefix:(t ^ "::") k) below then method_name k :: acc else acc)
               methods [])
        in
        List.filter_map
          (fun m ->
            match lookup ~structs:find ~has:(fun t -> Hashtbl.mem methods (t ^ "::" ^ m)) s.st_name with
            | Path (path, t) -> (
              match forwarder find s path (Hashtbl.find methods (t ^ "::" ^ m)) with
              | Some d ->
                Hashtbl.replace forwarder_of d.fn_name (t ^ "::" ^ m);
                Some (IFn d)
              | None -> None)
            | _ -> None)
          names
      | _ -> [])
    items

(* The items written in source: without the generated forwarders, for tools
   (references, hover, outline) that point at what the user wrote. *)
let user_items items = List.filter (function IFn d -> not (is_forwarder d.fn_name) | _ -> true) items

(* A type as written, for tools: `Box[Int]`. *)
let rec show_ty (t : ty_expr) =
  match t.tdesc with
  | TyName (n, []) -> n
  | TyName ("dyn", [ i ]) -> "dyn " ^ show_ty i
  | TyName (n, args) when Ast.is_tuple_name n -> "(" ^ String.concat ", " (List.map show_ty args) ^ ")"
  | TyName (n, args) -> n ^ "[" ^ String.concat ", " (List.map show_ty args) ^ "]"
  | TyUnit -> "()"
  | TyFn (ps, r) -> "fn(" ^ String.concat ", " (List.map show_ty ps) ^ ") -> " ^ show_ty r
  | TyInt n -> string_of_int n

(* The fields promoted onto struct [s] from the structs it embeds, with the
   struct each comes from: those a field access `x.f` reaches unambiguously.
   [has_method t f]: whether struct [t] declares method [f]. *)
let promoted_fields ~(structs : string -> struct_decl option) ~has_method (s : struct_decl) =
  let rec reach seen t =
    if List.mem t seen then seen
    else match structs t with
      | None -> seen
      | Some st -> List.fold_left (fun seen f -> match embed_type st f with Some (tn, _) -> reach seen tn | None -> seen) (seen @ [ t ]) st.st_embeds
  in
  let has_field t f = match structs t with Some st -> List.exists (fun (n, _, _) -> n = f) st.st_fields | None -> false in
  List.concat_map
    (fun t ->
      match structs t with
      | None -> []
      | Some st ->
        List.filter_map
          (fun (f, _, te) ->
            match lookup ~structs ~has:(fun u -> has_field u f || has_method u f) s.st_name with
            | Path (_, owner) when owner = t -> Some (f, te, t)
            | _ -> None)
          st.st_fields)
    (List.tl (reach [] s.st_name))
