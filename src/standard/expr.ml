
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Type definitions *)
(* ************************************************************************* *)

(* private aliases *)
type hash = int
type index = int
type 'a tag = 'a Tag.t

(* Builtins *)
type builtin = (var, cst, term) Builtin.t

(* Alias *)
and pattern = term

(* Variables *)
and var = {
  var_index     : index; (** unique *)
  var_ty        : term;
  var_name      : string;
  var_builtin   : builtin;
  mutable var_tags : Tag.map;
}

(* Constants *)
and cst = {
  cst_index     : index; (** unique *)
  cst_ty        : term;
  cst_name      : string; (* TODO: use a more precise type *)
  cst_builtin   : builtin;
  mutable cst_tags : Tag.map;
}

and binder =
  | Pi of var list
  | Arrow of term list
  | Exists of var list
  | Forall of var list
  | Lambda of var list
  | Letin of (var * term) list

and descr =
  | Var of var
  | Cst of cst
  | App of term * term list
  | Binder of binder * term
  | Match of term * (pattern * term) list

and term = {
    ty : term;
    mutable descr : descr;
    mutable hash : hash;
    mutable tags : Tag.map;
}

(* Alias for dolmen_loop and others who allow to have different types
   for types, terms, and formulas. *)
type t = term
type ty = term
type formula = term

(* Type definition for ADTs and records *)
type adt_case = {
  cstr : cst;
  tester : cst;
  dstrs : cst option array;
}

type def =
  | Abstract
  | Adt of {
      ty : cst;
      record : bool;
      cases : adt_case array;
    }


(* Exceptions *)
(* ************************************************************************* *)

(* Kinds&Types *)
exception Kind_has_no_type
exception Type_already_defined of cst
exception Record_type_expected of cst
exception Bad_quantification of binder * t * t

(* Application Errors *)
exception Bad_arity of cst * term list
exception Expected_function_type of term
exception Wrong_type of { term : term; expected_ty: term; }

(* ADT exceptions*)
exception Field_missing of cst
exception Field_repeated of cst
exception Field_expected of cst
exception Constructor_expected of cst
exception Wrong_sum_type of { cst : cst; expected_ty: term; }
exception Wrong_record_type of { cst : cst; record_ty: cst; }

(* Unification *)
exception Impossible_unification of term * term


(* Tags *)
(* ************************************************************************* *)

module Tags = struct

  type 'a t = 'a tag
  type pos = Pretty.pos
  type name = Pretty.name

  let pos = Tag.create ()
  let name = Tag.create ()
  let rwrt = Tag.create ()

  let exact s = Pretty.Exact s
  let infix = Pretty.Infix
  let prefix = Pretty.Prefix

  let ac = Tag.create ()
  let named = Tag.create ()
  let triggers = Tag.create ()

end


(* Printing *)
(* ************************************************************************* *)

module Print = struct

  type 'a t = Format.formatter -> 'a -> unit

  let print_index = ref false

  let pos : Pretty.pos tag = Tags.pos
  let name : Pretty.name tag = Tags.name

  let return fmt_str out () = Format.fprintf out "%(%)" fmt_str

  let pp_index fmt index = Format.fprintf fmt "/%d" index

  let var fmt (v : var) =
    match Tag.last v.var_tags name with
    | Some (Pretty.Exact s | Pretty.Renamed s) -> Format.fprintf fmt "%s" s
    | None ->
      if !print_index then
        Format.fprintf fmt "%s%a" v.var_name
          (Fmt.styled (`Fg (`Hi `Black)) pp_index) v.var_index
      else
        Format.fprintf fmt "%s" v.var_name

  let cst fmt (v : cst) =
    match Tag.last v.cst_tags name with
    | Some (Pretty.Exact s | Pretty.Renamed s) -> Format.fprintf fmt "%s" s
    | None ->
      if !print_index then
        Format.fprintf fmt "%s%a" v.cst_name
          (Fmt.styled (`Fg (`Hi `Black)) pp_index) v.cst_index
      else
        Format.fprintf fmt "%s" v.cst_name

  let cst_pretty fmt (v : cst) =
    match Tag.last v.cst_tags pos with
    | None -> ()
    | Some Pretty.Infix -> Format.fprintf fmt "(%a)" cst v
    | Some Pretty.Prefix -> Format.fprintf fmt "[%a]" cst v

  let binder_sep fmt = function
    | Pi _
    | Exists _
    | Forall _  -> Format.fprintf fmt "."
    | Letin _   -> Format.fprintf fmt "in"
    | Lambda _
    | Arrow _   -> Format.fprintf fmt "->"

  let rec descr fmt = function
    | Var v -> var fmt v
    | Cst c -> cst fmt c
    | App (f, []) -> term fmt f
    | App ({ descr = Cst f; _ }, args) -> fo_app fmt f args
    | App (f, args) -> ho_app fmt f args
    | Binder (b, body) ->
      Format.fprintf fmt "@[<hv 2>%a%a@ %a@]" binder b binder_sep b term body
    | Match (scrutinee, branches) ->
      Format.fprintf fmt "@[<hv 2>match %a with@ %a@]"
        term scrutinee
        (Format.pp_print_list ~pp_sep:(return "@ ") branch) branches

  and fo_app fmt (f : cst) args =
    match Tag.last f.cst_tags pos with
    (* Infix operators must have at least 2 arguments to properly be printed
       as infix operators *)
    | Some Pretty.Infix when List.length args >= 2 ->
      let pp_sep fmt () = Format.fprintf fmt " %a@ " cst f in
      Format.fprintf fmt "(@[<hov>%a@])" (Format.pp_print_list ~pp_sep term) args
    (* Generic case *)
    | None | Some Pretty.Prefix | Some Pretty.Infix ->
      Format.fprintf fmt "@[<hov>%a(%a)@]"
        cst f (Format.pp_print_list ~pp_sep:(return ",@ ") term) args

  and ho_app fmt f args =
    Format.fprintf fmt "(%a)"
      (Format.pp_print_list ~pp_sep:(return " %@@ ") term) (f :: args)

  and binder fmt b =
    match b with
    | Pi l ->
      Format.fprintf fmt "Π @[<hov>%a@]"
        (Format.pp_print_list ~pp_sep:(return ",@ ") typed_var) l
    | Forall l ->
      Format.fprintf fmt "∀ @[<hov>%a@]"
        (Format.pp_print_list ~pp_sep:(return ",@ ") typed_var) l
    | Exists l ->
      Format.fprintf fmt "∃ @[<hov>%a@]"
        (Format.pp_print_list ~pp_sep:(return ",@ ") typed_var) l
    | Lambda l ->
      Format.fprintf fmt "fun %a"
        (Format.pp_print_list ~pp_sep:(return "@ ") typed_var) l
    | Letin l ->
      Format.fprintf fmt "let @[<hv>%a@]"
        (Format.pp_print_list ~pp_sep:(return ",@ ") binding) l
    | Arrow l ->
      Format.fprintf fmt "%a"
        (Format.pp_print_list ~pp_sep:(return " ->@ ") term) l

  and typed_var fmt v =
    Format.fprintf fmt "@[<hov 2>%a :@ %a@]" var v term v.var_ty

  and binding fmt (v, t) =
    Format.fprintf fmt "@[<hov 2>%a =@ %a@]" var v term t

  and branch fmt (pattern, body) =
    Format.fprintf fmt "@[<hov 2>| %a@ ->@ %a" term pattern term body

  and term fmt t =
    descr fmt t.descr

end

(* Helpers *)
(* ************************************************************************* *)

(* option iter *)
let option_iter f = function
  | None -> ()
  | Some x -> f x

(* List creation *)
let init_list n f =
  let rec aux acc i =
    if i > n then List.rev acc else aux (f i :: acc) (i + 1)
  in
  aux [] 1

(* constant list with the same elements *)
let replicate n x =
  let rec aux x acc n =
    if n <= 0 then acc
    else aux x (x :: acc) (n - 1)
  in
  aux x [] n

(* List split *)
let split_args expected args =
  let rec aux acc expected args =
    match expected, args with
    | [], _ -> List.rev acc, `Remaining_args args
    | _, [] -> List.rev acc, `Still_expect expected
    | x :: r, y :: rest -> aux ((x, y) :: acc) r rest
  in
  aux [] expected args

(* automatic cache *)
let with_cache ~cache f x =
  match Hashtbl.find cache x with
  | res -> res
  | exception Not_found ->
    let res = f x in
    Hashtbl.add cache x res;
    res

(* Views *)
(* ************************************************************************* *)

module View = struct

  (* ===== Term classification ===== *)
  (* =============================== *)

  module Classify = struct

    let view t : Dolmen_intf.View.Classify.t =
      match t with
      | { descr = Cst { cst_builtin = Builtin.Kind; _ }; _ }
        -> Kind
      | { descr = Cst { cst_builtin = Builtin.Type; _ }; _ }
        -> Type
      | { ty =
            { descr = Cst
                  { cst_builtin = Builtin.Type; _ }; _ }; _ }
        -> Ty
      | { ty =
            { ty =
                { descr = Cst
                      { cst_builtin = Builtin.Type; _ }; _ }; _ }; _ }
        -> Term
      | _ -> Other

  end

  (* ===== Simplified View for types ===== *)
  (* ===================================== *)

  module Ty = struct

    let rec sig_ vars args t =
      match t.descr with
      | Binder (Arrow l, body) ->
        sig_ vars (List.rev_append l args) body
      | _ ->
        begin match vars, args with
          | [], [] -> `Other t
          | _ -> `Sig (List.rev vars, List.rev args, t)
        end

    let rec poly_sig vars t =
      match t.descr with
      | Binder (Pi l, body) -> poly_sig (List.rev_append l vars) body
      | _ -> sig_ vars [] t

    let ty t =
      match t.descr with
      | Var v -> `Var v
      | Cst ({ cst_builtin; _ } as c) ->
        begin match cst_builtin with
          | Builtin.Int -> `Int
          | Builtin.Rat -> `Rat
          | Builtin.Real -> `Real
          | Builtin.Bitv i -> `Bitv i
          | Builtin.Float (e, s) -> `Float (e, s)
          | Builtin.String -> `String
          | Builtin.String_RegLan -> `String_reg_lang
          | b -> `App (b, c, [])
        end
      | App ({ descr = Cst { cst_builtin = Builtin.Array; _ }; _ }, [src; dst]) ->
        `Array (src, dst)
      | App ({ descr = Cst ({ cst_builtin = b; _ } as c); _ }, l) ->
        `App (b, c, l)
      | _ -> poly_sig [] t

  end

  (* ===== First-order view ===== *)
  (* ============================ *)

  module FO = struct

    open Dolmen_intf.View.FO

    exception Not_first_order_ty of ty
    exception Not_first_order_term of term

    let is_type t =
      match Classify.view t with
      | Type -> true
      | _ -> false

    let assert_is_type t =
      if not (is_type t) then raise (Not_first_order_ty t)

    let ty t : (_, _, _, _) ty_view =
      assert_is_type t;
      match t.descr with
      | Var v -> Var v
      | Cst ({ cst_builtin = b; _ } as c) -> App (b, c, [])
      | App ({ descr = Cst ({ cst_builtin = b; _ } as c); _ }, l) -> App (b, c, l)
      | _ -> raise (Not_first_order_ty t)

    let split_fo_args term l =
      let rec consume_tys acc = function
        | [] -> List.rev acc, []
        | t :: r ->
          if is_type t
          then consume_tys (t :: acc) r
          else consume_terms acc [t] r
      and consume_terms tys acc = function
        | [] -> List.rev tys, List.rev acc
        | t :: r ->
          if is_type t then raise (Not_first_order_term term) (* dependant application *);
          consume_terms tys (t :: acc) r
      in
      consume_tys [] l

    let rec app term t acc : (_, _, _, _, _, _) term_view =
      match t.descr with
      | Cst ({cst_builtin = b; _ } as c) ->
        begin match split_fo_args term acc with
          | tys, terms -> App (b, c, tys, terms)
          | exception Exit -> raise (Not_first_order_term term)
        end
      | App (f, l) ->
        app term f (l @ acc)
      | _ -> raise (Not_first_order_term term)

    let split_fo_vars term l =
      let rec consume_tys acc = function
        | [] -> List.rev acc, []
        | v :: r ->
          if is_type v.var_ty
          then consume_tys (v :: acc) r
          else consume_terms acc [v] r
      and consume_terms tys acc = function
        | [] -> List.rev tys, List.rev acc
        | v :: r ->
          if is_type v.var_ty then
            raise (Not_first_order_term term) (* dependant quantification *);
          consume_terms tys (v :: acc) r
      in
      consume_tys [] l

    let binder t b : (_, _, _) binder =
      match b with
      | Pi _
      | Lambda _
      | Arrow _ -> raise (Not_first_order_term t)
      | Exists l ->
        let tys, terms = split_fo_vars t l in
        Exists (tys, terms)
      | Forall l ->
        let tys, terms = split_fo_vars t l in
        Forall (tys, terms)
      | Letin l -> Letin l

    let term t : (_, _, _, _, _, _) term_view =
      match ty t.ty with
      | exception Not_first_order_ty _ ->
        raise (Not_first_order_term t)
      | _ ->
        begin match t.descr with
          | Var v -> Var v
          | Cst _
          | App _ -> app t t []
          | Binder (b, body) -> Binder (binder t b, body)
          | Match (scrutinee, branches) -> Match (scrutinee, branches)
        end

  end

  (* ===== Higher-order view ===== *)
  (* ============================= *)

  module HO = struct

    exception Not_higher_order of term

    let binder b : (_, _) Dolmen_intf.View.HO.binder =
      match (b : binder) with
      | Pi l -> Pi l
      | Arrow l -> Arrow l
      | Exists l -> Exists l
      | Forall l -> Forall l
      | Lambda l -> Lambda l
      | Letin l -> Letin l

    let term t : (_, _, _, _) Dolmen_intf.View.HO.t =
      match t.descr with
      | Var v -> Var v
      | Cst c -> Cst (c.cst_builtin, c)
      | App (f, args) -> App (f, args)
      | Binder (b, body) -> Binder (binder b, body)
      | Match (scrutinee, branches) -> Match (scrutinee, branches)

  end

end

(* Hashing *)
(* ************************************************************************* *)

module Hash = struct

  (* hash helpers *)
  let hash2 x y = Hashtbl.seeded_hash x y
  let hash3 x y z = hash2 x (hash2 y z)

  (* list hash *)
  let list f l =
    let rec aux acc = function
      | [] -> acc
      | x :: r -> aux (Hashtbl.seeded_hash acc (f x)) r
    in
    aux 0 l

  let var v = v.var_index
  let cst c = c.cst_index

  let rec binder = function
    | Pi l -> hash2 3 (list var l)
    | Arrow l -> hash2 5 (list term l)
    | Exists l -> hash2 7 (list var l)
    | Forall l -> hash2 11 (list var l)
    | Lambda l -> hash2 13 (list var l)
    | Letin l -> hash2 17 (list binding l)

  and binding (v, t) = hash2 (var v) (term t)

  and descr = function
    | Var v -> hash2 3 (var v)
    | Cst c -> hash2 5 (cst c)
    | App (f, args) -> hash3 7 (term f) (list term args)
    | Binder (b, body) -> hash3 11 (binder b) (term body)
    | Match (scrutinee, l) -> hash3 13 (term scrutinee) (list branch l)

  and branch (pattern, body) =
    hash2 (term pattern) (term body)

  and[@inline] term t =
    if t.hash <= 0 then t.hash <- descr t.descr;
    t.hash

end

(* Comparison *)
(* ************************************************************************* *)

module Discr = struct

  let[@inline] descr = function
    | Var _ -> 1
    | Cst _ -> 2
    | App _ -> 3
    | Binder _ -> 4
    | Match _ -> 5

  let[@inline] binder = function
    | Pi _ -> 1
    | Arrow _ -> 2
    | Exists _ -> 3
    | Forall _ -> 4
    | Lambda _ -> 5
    | Letin _ -> 6

end

module Compare = struct

  (* Shadow the poly compare just to be sure *)
  let compare (a: int) b = compare a b

  (* Useful shorthand for chaining comparisons *)
  let[@inline] (<?>) i (cmp, x, y) =
    match i with
    | 0 -> cmp x y
    | _ -> i

  (* lexicographic comparison *)
  let lexicographic cmp l l' =
    let rec aux l l' =
      match l, l' with
      | [], [] -> 0
      | _ :: _, [] -> 1
      | [], _ :: _ -> -1
      | x :: r, x' :: r' ->
        begin match cmp x x' with
          | 0 -> aux r r'
          | res -> res
        end
    in
    aux l l'

  let var v v' = compare v.var_index v'.var_index
  let cst c c' = compare c.cst_index c'.cst_index

  let[@inline] rec term u v =
    if u == v then 0 else begin
      let hu = Hash.term u and hv = Hash.term v in
      if hu <> hv then compare hu hv else descr u.descr v.descr
    end

  and descr u_descr v_descr =
    match u_descr, v_descr with
    | Var v1, Var v2 -> var v1 v2
    | Cst c1, Cst c2 -> cst c1 c2
    | App (f, args), App (f', args') ->
      term f f' <?> (lexicographic term, args, args')
    | Binder (b, body), Binder (b', body') ->
      binder b b' <?> (term, body, body')
    | Match (s, l), Match (s', l') ->
      term s s' <?> (lexicographic branch, l, l')
    | _, _ -> (Discr.descr u_descr) - (Discr.descr v_descr)

  and binder b b' =
    match b, b' with
    | Pi vars, Pi vars' -> lexicographic var vars vars'
    | Arrow l, Arrow l' -> lexicographic term l l'
    | Exists l, Exists l' -> lexicographic var l l'
    | Forall l, Forall l' -> lexicographic var l l'
    | Letin l, Letin l' -> lexicographic binding l l'
    | Lambda l, Lambda l' -> lexicographic var l l'
    | _, _ -> (Discr.binder b) - (Discr.binder b')

  and binding (v, t) (v', t') =
    var v v' <?> (term, t, t')

  and branch (p, b) (p', b') =
    term p p' <?> (term, b, b')

end

module Equal = struct

  let[@inline] var v v' =
    v == v' || Compare.var v v' = 0

  let[@inline] cst c c' =
    c == c' || Compare.cst c c' = 0

  let[@inline] term t t' =
    Compare.term t t' = 0

end

(* Variables *)
(* ************************************************************************* *)

module Var = struct

  type t = var

  (* Usual functions *)
  let hash = Hash.var
  let equal = Equal.var
  let compare = Compare.var

  let print = Print.var

  (* Sets and Maps *)
  module Aux = struct
    type t = var
    let hash = hash
    let equal = equal
    let compare = compare
  end

  module Set = Set.Make(Aux)
  module Map = Map.Make(Aux)
  module H = Hashtbl.Make(Aux)

  (* type *)
  let ty (v : t) = v.var_ty

  (* Tags *)
  let tag (id : t) k v = id.var_tags <- Tag.add id.var_tags k v

  let get_tag (id : t) k = Tag.get id.var_tags k

  let get_tag_last (id : t) k = Tag.last id.var_tags k

  (* Creating ids *)
  let id_counter = ref 0

  let make ?pos ?name ?(builtin=Builtin.Base) ?(tags=Tag.empty) var_name var_ty =
    incr id_counter;
    let res = {
      var_name; var_ty;
      var_tags = tags;
      var_builtin = builtin;
      var_index = !id_counter;
    } in
    (* Add pretty printing tags *)
    option_iter (tag res Print.pos) pos;
    option_iter (fun s -> tag res Print.name (Pretty.Exact s)) name;
    (* Return the id *)
    res

  let mk name ty = make name ty

end

(* Constants *)
(* ************************************************************************* *)

module Cst = struct

  type t = cst

  (* Usual functions *)
  let hash = Hash.cst
  let equal = Equal.cst
  let compare = Compare.cst

  let print = Print.cst

  (* Sets and Maps *)
  module Aux = struct
    type t = cst
    let hash = hash
    let equal = equal
    let compare = compare
  end

  module Set = Set.Make(Aux)
  module Map = Map.Make(Aux)
  module H = Hashtbl.Make(Aux)

  (* Tags *)
  let tag (id : t) k v = id.cst_tags <- Tag.add id.cst_tags k v

  let get_tag (id : t) k = Tag.get id.cst_tags k

  let get_tag_last (id : t) k = Tag.last id.cst_tags k


  (* Some useful functions *)

  let ty c = c.cst_ty


  (* Kind
     Because of the need to have an end-point for the type of terms,
     the constant and term for [Kind] are created manually with
     a recursive type definition. *)
  let rec kind = {
    cst_index = 0;
    cst_ty = kind_term;
    cst_name = "Kind";
    cst_builtin = Builtin.Kind;
    cst_tags = Tag.empty;
  }

  and kind_term = {
    ty = kind_term;
    descr = Cst kind;
    hash = -1;
    tags = Tag.empty;
  }

  (* Creating ids
     Note that the id [1] is already taken by the constant for kind,
     and since the make function increments before using the counter,
     we thus start at [1] (whereas variables start at [0]). *)
  let id_counter = ref 1

  let make ?pos ?name ?(builtin=Builtin.Base) ?(tags=Tag.empty) cst_name cst_ty =
    incr id_counter;
    let res = {
      cst_name; cst_ty;
      cst_tags = tags;
      cst_builtin = builtin;
      cst_index = !id_counter;
    } in
    (* Add pretty printing tags *)
    option_iter (tag res Print.pos) pos;
    option_iter (fun s -> tag res Print.name (Pretty.Exact s)) name;
    (* Return the id *)
    res

  let mk name ty = make name ty

  (* Type constant *)
  let type_ = make ~builtin:Builtin.Type "Type" kind_term

end

(* Some core functions *)
(* ************************************************************************* *)

module Core = struct

  (* ==== Simple terms ===== *)

  let mk ?(tags=Tag.empty) descr ty = { descr; ty; hash = -1; tags; }

  let of_var v = mk (Var v) v.var_ty

  let of_cst c = mk (Cst c) c.cst_ty

  (* ===== Some useful values ===== *)

  let kind = Cst.kind_term

  let type_ = of_cst Cst.type_

  (* ===== Inspection ===== *)

  let ty t =
    if t == kind then raise Kind_has_no_type else t.ty


  (* ===== Binders ===== *)

  let pi_sort b s1 s2 =
    match View.Classify.view s1, View.Classify.view s2 with
    | 

  let rec mk_bind b body =
    match b with
    (* empty list of bound vars *)
    | Pi []
    | Arrow []
    | Exists []
    | Forall []
    | Lambda []
    | Letin [] -> body

    (* Pi quantification (type quantification over types) *)
    | Pi l ->
      let res_ty = List.fold_right
          (fun v ty -> pi_sort b v.var_ty ty)

      begin match View.Classify.view body with
        | Ty -> ()
        | _ -> raise (Bad_quantification (b, body))
      end;
      List.iter (fun v ->
          match View.Classify.view v.var_ty with
          | Type -> ()
          | _ -> raise (Bad_quantification (b, body))
        ) l;
      mk (Binder (b, body)) type_

    (* Arrow function type *)
    | Arrow args ->
      begin match View.Classify.view body with
        | Type | Ty -> ()
        | _ -> raise (Bad_quantification (b, body))
      end;
      List.iter (fun t ->
          match View.Classify.view t with
          | Type | Ty -> ()
          | _ -> raise (Bad_quantification (b, body))
        ) args;
      mk (Binder (b, body)) type_

    (* The check that the body is a prop (for exists and forall)
       is delayed because prop is not yet created at this point. *)
    | Exists _
    | Forall _
    | Letin _ -> mk (Binder (b, body)) (ty body)

    (* for functions, the type is different *)
    | Lambda vars ->
      let fun_ty = fun_type vars (ty body) in
      mk (Binder (b, body)) fun_ty

  and fun_type vars ret_ty =
    let rec aux_ty acc = function
      | [] -> .

    and aux_type acc = function
      | [] -> .

    and aux = function
      | [] -> ret

      match Var.ty v with
      | { descr = Cst { cst_builtin = Builtin.Type; _ }; _ } ->
        mk_bind (Pi [v]) ret
      | v_ty ->
        mk_bind (Arrow [v_ty]) ret
    in


    List.fold_right aux vars ret_ty

  let pi l body = mk_bind (Pi l) body
  let arrow l body = mk_bind (Arrow l) body



  (* ===== Free variables ===== *)

  let rec free_vars acc t =
    match t.descr with
    | Var v ->
      Var.Set.add v (free_vars acc v.var_ty)
    | Cst _ ->
      acc
    | App (f, args) ->
      List.fold_left free_vars acc (f :: args)
    | Binder (Arrow l, body) ->
      List.fold_left free_vars acc (body :: l)
    | Binder ((Pi l | Exists l | Forall l | Lambda l), body) ->
      let fv = free_vars Var.Set.empty body in
      let fv = List.fold_right Var.Set.remove l fv in
      Var.Set.union fv acc
    | Binder (Letin l, body) ->
      let fv = free_vars Var.Set.empty body in
      let fv = List.fold_right (fun (v, t) acc ->
          let acc = free_vars acc t in
          let acc = Var.Set.remove v acc in
          let acc = free_vars acc v.var_ty in
          acc
        ) l fv in
      Var.Set.union fv acc
    | Match (scrutinee, branches) ->
      let acc = free_vars acc scrutinee in
      List.fold_left (fun fv (pat, body) ->
          let free = free_vars Var.Set.empty body in
          let bound = free_vars Var.Set.empty pat in
          Var.Set.union fv (Var.Set.diff free bound)
        ) acc branches

  let fv t =
    free_vars Var.Set.empty t


  (* ===== Wildcards ===== *)

  (* Set a wildcard/hole to a concrete type

     Wildcard can be set to point to another type by mutating the
     descr field of types (this is the only operation that mutates
     this field).
     In order to be correct, when we set a wildcard v to point at
     another wildcard w, we must remember that, so that when we set
     w to something, we also need to update v. *)

  let wildcard_tbl = ref Var.Map.empty

  let wildcard_get v =
    match Var.Map.find v !wildcard_tbl with
    | l -> l
    | exception Not_found -> []

  let wildcard_add v l =
    let l' = wildcard_get v in
    wildcard_tbl := Var.Map.add v (List.rev_append l l') !wildcard_tbl

  let set_wildcard v t =
    let set_descr t s = s.descr <- t.descr in
    let l = wildcard_get v in
    List.iter (set_descr t) l;
    match t.descr with
    | Var ({ var_builtin = Builtin.Wildcard; _ } as w) ->
      wildcard_add w l;
    | _ -> ()

  let wildcard () =
    let v = Var.make ~builtin:Builtin.Wildcard "_" type_ in
    let t = of_var v in
    wildcard_add v [t];
    t


  (* ===== Type definitions ===== *)

  let definition_tag : def Tag.t = Tag.create ()

  let definition c = Cst.get_tag_last c definition_tag

  let is_record c =
    match definition c with
    | Some Adt { record; _ } -> record
    | _ -> false

  let define c d =
    match definition c with
    | None -> Cst.tag c definition_tag d
    | Some _ -> raise (Type_already_defined c)

  (* Exhaustivity check
     TODO: implement this *)
  let check_exhaustivity _ty _pats = ()


  (* ===== Robinson unification ===== *)


  let rec follow subst t =
    match t with
    | { descr = Var v; _ } ->
      begin match Var.Map.find v subst with
        | t' -> follow subst t'
        | exception Not_found -> t
      end
    | t -> t

  let rec occurs subst l t =
    match t.descr with
    | Cst _ -> false
    | Var v ->
      List.exists (Var.equal v) l ||
      begin match Var.Map.find v subst with
        | exception Not_found -> false
        | e -> occurs subst (v :: l) e
      end
    | App (f, args) ->
      List.exists (occurs subst l) (f :: args)
    | Binder _
    | Match _ -> false

  let robinson_bind subst m v u =
    if occurs subst [v] u then
      raise (Impossible_unification (m, u))
    else
      Var.Map.add v u subst

  let rec robinson subst s t =
    let s = follow subst s in
    let t = follow subst t in
    match s, t with

    (* This robinson unification only has the goal of unifying the
       wildcard variables. The bound variables of a polymorphic function
       will be explicitly bound in the map by the applicaiton/instantiation
       process *)
    | ({ descr = Var ({ var_builtin = Builtin.Wildcard; _ } as v); _ } as m), u
    | u, ({ descr = Var ({ var_builtin = Builtin.Wildcard; _ } as v); _ } as m) ->
      if Equal.term m u then subst else robinson_bind subst m v u

    (* The rest of the cases are mainly here to go down in the AST *)
    | ({ descr = Var v; _}, { descr = Var v'; _ }) ->
      if Var.equal v v' then subst
      else raise (Impossible_unification (s, t))

    | ({ descr = Cst c; _ }, { descr = Cst c'; _ }) ->
      if Cst.equal c c' then subst
      else raise (Impossible_unification (s, t))

    | { descr = App (f, f_args); _ },
      { descr = App (g, g_args); _ } ->
      List.fold_left2 robinson (robinson subst f g) f_args g_args

    | { descr = Binder (Arrow l, body); _ },
      { descr = Binder (Arrow l', body'); _ } ->
      begin match List.fold_left2 robinson subst l l' with
        | subst -> robinson subst body body'
        | exception Invalid_argument _ -> raise (Impossible_unification (s, t))
      end

    | { descr = Binder (Pi l, body); _ },
      { descr = Binder (Pi l', body'); _ } ->
      let aux map v v' = Var.Map.add v (of_var v') map in
      begin match List.fold_left2 aux subst l l' with
        | subst -> robinson subst body body'
        | exception Invalid_argument _ -> raise (Impossible_unification (s, t))
      end

    (* Unification of binders/match are outside what is reasonable
       for the moment.
       TODO: allow let-bindings by accumulating them in the env *)
    | _, _ ->
      raise (Impossible_unification (s, t))


  (* ===== Substitution and application ===== *)

  let rec var_list_subst ~fix s acc = function
    | [] -> List.rev acc, s
    | v :: r ->
      let ty = subst_aux ~fix s v.var_ty in
      if not (Equal.term ty v.var_ty) then
        let nv = Var.mk v.var_name ty in
        var_list_subst ~fix
          (Var.Map.add v (of_var nv) s) (nv :: acc) r
      else
        var_list_subst ~fix (Var.Map.remove v s) (v :: acc) r

  and subst_aux ~fix s (t : term) =
    match t.descr with
    | Var v ->
      begin match Var.Map.find v s with
        | exception Not_found -> t
        | term ->
          if fix
          then subst_aux ~fix s term
          else term
      end
    | Cst _ -> t
    | App (f, args) ->
      let new_args = List.map (subst_aux ~fix s) args in
      if List.for_all2 (==) new_args args then t
      else apply f new_args
    | Binder (b, body) ->
      let b', s = binder_subst ~fix s b in
      mk_bind b' (subst_aux ~fix s body)
    | Match (scrutinee, branches) ->
      let scrutinee = subst_aux ~fix s scrutinee in
      let branches = List.map (branch_subst ~fix s) branches in
      pattern_match scrutinee branches

  and binder_subst ~fix s = function
    | Pi l ->
      let l', s' = var_list_subst ~fix s [] l in
      Pi l', s'
    | Arrow l ->
      let l' = List.map (subst_aux ~fix s) l in
      Arrow l', s
    | Exists l ->
      let l', s' = var_list_subst ~fix s [] l in
      Exists l', s'
    | Forall l ->
      let l', s' = var_list_subst ~fix s [] l in
      Forall l', s'
    | Lambda l ->
      let l', s' = var_list_subst ~fix s [] l in
      Lambda l', s'
    | Letin l ->
      let l', s' = binding_list_subst ~fix s [] l in
      Letin l', s'

  and binding_list_subst ~fix s acc = function
    | [] -> List.rev acc, s
    | (v, t) :: r ->
      let t = subst_aux ~fix s t in
      if Equal.term (ty t) v.var_ty then begin
        let s' = Var.Map.remove v s in
        let acc = (v, t) :: acc in
        binding_list_subst ~fix s' acc r
      end else begin
        let nv = Var.mk v.var_name (ty t) in
        let s' = Var.Map.add v (of_var nv) s in
        let acc = (nv, t) :: acc in
        binding_list_subst ~fix s' acc r
      end

  and branch_subst ~fix s (pattern, body) =
    let l = Var.Set.elements (fv pattern) in
    let _, s' = var_list_subst ~fix s [] l in
    (subst_aux ~fix s' pattern, subst_aux ~fix s' body)

  and subst ?(fix=true) s t =
    if Var.Map.is_empty s then t else subst_aux ~fix s t

  (* Application typechecking *)
  and instantiate s f args =
    match args with
    | [] ->
      Var.Map.iter set_wildcard s;
      subst s f.ty
    | _ ->
      begin match f.ty.descr with
        | Binder ((Pi l), body) ->
          let args, rest = split_args l args in
          let bind s (k, v) = Var.Map.add k v s in
          let s' = List.fold_left bind Var.Map.empty args in
          begin match rest with
            | `Remaining_args rest -> instantiate s' body rest
            | `Still_expect l' -> instantiate s' (pi l' body) []
          end
        | Binder ((Arrow l), body) ->
          let args, rest = split_args l args in
          let s' = List.fold_left (fun s (expected, term) ->
              try robinson s expected (ty term)
              with Impossible_unification _ -> _wrong_type s term expected
            ) s args
          in
          begin match rest with
            | `Remaining_args rest -> instantiate s' body rest
            | `Still_expect l' -> instantiate s' (arrow l' body) []
          end
        | _ -> raise (Expected_function_type f)
      end

  (* Application *)
  and apply f = function
    | [] -> f
    | args -> mk (App (f, args)) (instantiate Var.Map.empty f args)

  (* Pattern matching *)
  and pattern_match scrutinee branches =
    let scrutinee_ty = ty scrutinee in
    (* first,
       unify the type of the scrutinee and all patterns,
       and unify the type of all bodies *)
    let body_ty = wildcard () in
    let s = List.fold_left (fun acc (pattern, body) ->
        let acc =
          try robinson acc scrutinee_ty (ty pattern)
          with Impossible_unification _ -> _wrong_type acc pattern scrutinee_ty
        in
        let acc =
          try robinson acc body_ty (ty body)
          with Impossible_unification _ -> _wrong_type acc body body_ty
        in
        acc
      ) Var.Map.empty branches
    in
    (* Apply the substitution to the scrutinee, patterns and bodies *)
    let () = Var.Map.iter set_wildcard s in
    let scrutinee = subst s scrutinee in
    let branches = List.map (fun (pat, body) ->
        (subst s pat, subst s body)
      ) branches in
    (* Check exhaustivity *)
    let () = check_exhaustivity (ty scrutinee) (List.map fst branches) in
    (* Build the pattern matching *)
    mk (Match (scrutinee, branches)) body_ty

  (* helper for type errors *)
  and _wrong_type s t ty =
    raise (Wrong_type { term = subst s t;
                        expected_ty = subst s ty; })



end


(* Types *)
(* ************************************************************************* *)

module Ty = struct

  (* ===== Type aliases ===== *)

  type t = term

  type 'a tag = 'a Tag.t


  (* ===== Std functions =====*)

  let hash = Hash.term
  let equal = Equal.term
  let compare = Compare.term

  let print = Print.term


  (* ===== View ===== *)

  type view = (var, cst, builtin, ty) Dolmen_intf.View.Ty.t

  let view = View.Ty.ty


  (* ===== Tags ===== *)

  let tag (t : t) k v = t.tags <- Tag.add t.tags k v

  let get_tag (t : t) k = Tag.get t.tags k

  let get_tag_last (t : t) k = Tag.last t.tags k


  (* ===== Type definitions ===== *)

  let define = Core.define
  let definition = Core.definition


  (* ===== Free vars and substitution ===== *)

  let fv = Core.fv
  let subst = Core.subst


  (* ===== Function Aliases ===== *)

  let type_ = Core.type_

  let apply = Core.apply
  let of_var = Core.of_var
  let of_cst = Core.of_cst
  let wildcard = Core.wildcard

  let as_poly_sig t =
    match View.Ty.poly_sig [] t with
    | `Other ret -> [], [], ret
    | `Sig (vars, args, ret) -> vars, args, ret


  (* ===== First-order shortcut ===== *)

  let apply_fo c args =
    let f = of_cst c in
    try
      let res = apply f args in
      begin match View.Ty.poly_sig [] res with
        | `Other _ -> res
        | `Sig _ -> raise (Bad_arity (c, args)) (* under-application *)
      end
    with Expected_function_type _ ->
      raise (Bad_arity (c, args)) (* over-application *)


  (* ====== Binders ===== *)

  let pi = Core.pi
  let arrow = Core.arrow

  let poly_sig vars args ret =
    pi vars (arrow args ret)


  (* ===== Type variables & constants ===== *)

  let var name = Var.mk name Core.type_

  let cst name arity =
    Cst.mk name (arrow (replicate arity Core.type_) Core.type_)


  (* ===== Prop ===== *)

  let prop_cst = Cst.make ~builtin:Builtin.Prop "Prop" Core.type_
  let prop = apply_fo prop_cst []


  (* ===== Algebraic Datatypes ===== *)

  (* Helpers for adt definition *)
  let mk_cstr ty_c name i vars args ret =
    Cst.make name (poly_sig vars args ret)
      ~builtin:(Builtin.Constructor { adt = ty_c; case = i; })

  let mk_cstr_tester cstr =
    let name = Format.asprintf "is:%a" Print.cst cstr in
    let vars, _, adt = as_poly_sig cstr.cst_ty in
    Cst.make
      ~builtin:(Builtin.Tester { cstr })
      name (pi vars (arrow [adt] prop))

  (* ADT definition *)
  let define_adt_aux ~record ty_const vars l =
    let ty = apply (of_cst ty_const) (List.map of_var vars) in
    let cases = ref [] in
    let l' = List.mapi (fun i (cstr_name, args) ->
        let args_ty = List.map fst args in
        let cstr = mk_cstr ty_const cstr_name i vars args_ty ty in
        let tester = mk_cstr_tester cstr in
        let dstrs = Array.make (List.length args) None in
        let l' = List.mapi (fun j -> function
            | (arg_ty, None) -> (arg_ty, None)
            | (arg_ty, Some name) ->
              let dstr =
                Cst.make name (poly_sig vars [ty] arg_ty)
                  ~builtin:(Builtin.Destructor {
                      adt = ty_const; cstr;
                      case = i; field = j; })
              in
              dstrs.(j) <- Some dstr;
              (arg_ty, Some dstr)
          ) args in
        cases := { cstr; tester; dstrs; } :: !cases;
        cstr, l'
      ) l in
    assert (not record || List.length !cases = 1);
    Core.define ty_const (Adt { ty = ty_const; record;
                                cases = Array.of_list @@ List.rev !cases; });
    l'

  let define_adt = define_adt_aux ~record:false

  let define_record ty_const vars l =
    let name = ty_const.cst_name in
    let cstr_args = List.map (fun (field_name, ty) ->
        ty, Some field_name
      ) l in
    let l' = define_adt_aux ~record:true ty_const vars [name, cstr_args] in
    match l' with
    | [ _, l'' ] ->
      List.map (function
          | _, Some dstr -> dstr
          | _, None -> assert false
        ) l''
    | _ -> assert false


  (* ===== Type constants ===== *)

  module Cst = struct

    let make ?builtin name n =
      Cst.make ?builtin name
        (poly_sig [] (replicate n Core.type_) Core.type_)

    let prop = prop_cst
    let unit = make ~builtin:Builtin.Unit "unit" 0
    let base = make ~builtin:Builtin.Univ "$i" 0
    let int = make ~builtin:Builtin.Int "int" 0
    let rat = make ~builtin:Builtin.Rat "rat" 0
    let real = make ~builtin:Builtin.Real "real" 0
    let array = make ~builtin:Builtin.Array "array" 2
    let string = make ~builtin:Builtin.String "string" 0
    let string_reg_lang =
      make ~builtin:Builtin.String_RegLan "string_reglang" 0
    let bitv =
      with_cache ~cache:(Hashtbl.create 13) (fun i ->
          make ~builtin:(Builtin.Bitv i) (Format.asprintf "Bitv_%d" i) 0
        )
    let float =
      with_cache ~cache:(Hashtbl.create 13) (fun (e,s) ->
          make ~builtin:(Builtin.Float(e,s)) (Format.asprintf "FloatingPoint_%d_%d" e s) 0
        )
    let roundingMode = make ~builtin:Builtin.RoundingMode "RoundingMode" 0
  end

  (* Builtin types *)
  let unit = apply_fo Cst.unit []
  let base = apply_fo Cst.base []
  let int = apply_fo Cst.int []
  let rat = apply_fo Cst.rat []
  let real = apply_fo Cst.real []
  let string = apply_fo Cst.string []
  let string_reg_lang = apply_fo Cst.string_reg_lang []
  let array src dst = apply_fo Cst.array [src; dst]
  let bitv i = apply_fo (Cst.bitv i) []
  let float' es = apply_fo (Cst.float es) []
  let float e s = float' (e,s)
  let roundingMode = apply_fo Cst.roundingMode []
  let bool = prop (* alias for alt-ergo *)

end


(* ADT Constructors *)
(* ************************************************************************* *)

module Cstr = struct

  type t = cst

  (* Usual functions *)
  let hash = Cst.hash
  let equal = Cst.equal
  let compare = Cst.compare

  let print = Print.cst

  module Aux = struct
    type t = cst
    let hash = hash
    let equal = equal
    let compare = compare
  end

  module Set = Set.Make(Aux)
  module Map = Map.Make(Aux)
  module H = Hashtbl.Make(Aux)

  (* Tags *)
  let tag = Cst.tag
  let get_tag = Cst.get_tag
  let get_tag_last = Cst.get_tag_last

  (* Application *)
  let apply cstr args = Core.apply (Core.of_cst cstr) args

  (* Arity for constructors,
     Given that the return value of a constructor will
     not be a function type (since it will be the ADT type
     constructor applied to some arguments), the arity of
     a constructor is well-defined. *)
  let arity cstr =
    let vars, args, _ = Ty.as_poly_sig cstr.cst_ty in
    List.length vars, List.length args

  (* Get the tester for a given constructor *)
  let tester c =
    match c.cst_builtin with
    | Builtin.Constructor { adt; case; } ->
      begin match Core.definition adt with
        | Some Adt { cases; _ } -> cases.(case).tester
        | _ -> assert false
      end
    | _ -> raise (Constructor_expected c)

  (* ADT constructor tester *)
  let test c t =
    let test_term = tester c in
    let vars, _, _ = Ty.as_poly_sig test_term.cst_ty in
    let ty_args = init_list (List.length vars) (fun _ -> Core.wildcard ()) in
    Core.apply (Core.of_cst test_term) (ty_args @ [t])

  let pattern_arity (c : t) ret tys =
    try
      let vars, args, adt = Ty.as_poly_sig c.cst_ty in
      let bind s k v = Var.Map.add k v s in
      let s = List.fold_left2 bind Var.Map.empty vars tys in
      let s = Core.robinson s adt ret in
      Var.Map.iter Core.set_wildcard s;
      List.map (Core.subst s) args
    with
    | Impossible_unification _ ->
      raise (Wrong_sum_type { cst = c; expected_ty = ret; })
    | Invalid_argument _ ->
      raise (Bad_arity (c, tys))

  let void =
    match Ty.define_adt Ty.Cst.unit [] ["void", []] with
    | [void, _] -> void
    | _ -> assert false

end

(* ADT/record fields *)
(* ************************************************************************* *)

module Field = struct

  (* Record fields are represented as their destructors, i.e. constants *)
  type t = cst

  (* Usual functions *)
  let hash = Cst.hash
  let equal = Cst.equal
  let compare = Cst.compare

  let print = Print.cst

  module Aux = struct
    type t = cst
    let hash = hash
    let equal = equal
    let compare = compare
  end

  module Set = Set.Make(Aux)
  module Map = Map.Make(Aux)
  module H = Hashtbl.Make(Aux)

  (* Record field application *)
  let apply (f : cst) t =
    let vars, _, _ = Ty.as_poly_sig f.cst_ty in
    let tys = init_list (List.length vars) (fun _ -> Core.wildcard ()) in
    Core.apply (Core.of_cst f) (tys @ [t])

  (* Record field getter *)
  let find ty_c i =
    match Core.definition ty_c with
    | Some Adt { record = true; cases = [| { dstrs; _ } |]; _ } ->
      begin match dstrs.(i) with
        | Some c -> c
        | None -> assert false
      end
    | _ ->
      raise (Record_type_expected ty_c)

  (* Record creation *)
  let index ty_c f =
    match f.cst_builtin with
    | Builtin.Destructor { adt = ty_d; case = i; field = j; _ } ->
      if Cst.equal ty_c ty_d then begin
        assert (i = 0);
        j
      end else
        raise (Wrong_record_type { cst = f; record_ty = ty_c; })
    | _ ->
      raise (Field_expected f)

end

(* Terms *)
(* ************************************************************************* *)

module Term = struct

  type t = term

  type 'a tag = 'a Tag.t

  (* ===== Std functions =====*)

  let hash = Hash.term
  let equal = Equal.term
  let compare = Compare.term

  let print = Print.term


  (* ====== Helpers ===== *)

  let check_ty term ~ty:expected_ty =
    if not (equal expected_ty term.ty) then
      raise (Wrong_type { term; expected_ty; })


  (* ===== Tags ===== *)

  let tag (t : t) k v = t.tags <- Tag.add t.tags k v

  let get_tag (t : t) k = Tag.get t.tags k

  let get_tag_last (t : t) k = Tag.last t.tags k


  (* ===== Free variables and subtitution ===== *)

  let fv = Core.fv
  let subst = Core.subst

  (* ===== Terms and types ===== *)

  let ty = Core.ty

  (* typing annotations *)
  let ensure t ty =
    match Core.robinson Var.Map.empty ty t.ty with
    | s ->
      Var.Map.iter Core.set_wildcard s;
      t
    | exception Impossible_unification _ ->
      raise (Wrong_type { term = t; expected_ty = ty; })


  (* ===== Constants ===== *)

  let cst name vars args ret =
      Cst.mk name (Ty.poly_sig vars args ret)


  (* ===== Term creation ===== *)

  let of_var = Core.of_var
  let of_cst = Core.of_cst
  let apply = Core.apply
  let pattern_match = Core.pattern_match

  let lambda vars body =
    Core.mk_bind (Lambda vars) body

  let ex vars body =
    check_ty body ~ty:Ty.prop;
    Core.mk_bind (Exists vars) body

  let all vars body =
    check_ty body ~ty:Ty.prop;
    Core.mk_bind (Forall vars) body

  let letin l body =
    List.iter (fun ((v : Var.t), t) ->
        if not (Ty.equal v.var_ty (Core.ty t))
        then raise (Wrong_type { term = t; expected_ty = v.var_ty; })
      ) l;
    Core.mk_bind (Letin l) body


  (* ===== First-order shortcut ===== *)

  let apply_fo c tys args =
    let f = of_cst c in
    let args = tys @ args in
    try
      let res = apply f args in
      begin match View.Ty.poly_sig [] res.ty with
        | `Other _ -> res
        | `Sig _ -> raise (Bad_arity (c, args)) (* under-application *)
      end
    with Expected_function_type _ ->
      raise (Bad_arity (c, args)) (* over-application *)


  (* ===== Record creation ===== *)

  let build_record_fields ty_c l =
    let n =
      match Core.definition ty_c with
      | Some Adt { record = true; cases = [| { dstrs; _ } |]; _ } ->
        Array.length dstrs
      | _ -> raise (Record_type_expected ty_c)
    in
    let fields = Array.make n None in
    List.iter (fun (field, value) ->
        let i = Field.index ty_c field in
        match fields.(i) with
        | Some _ -> raise (Field_repeated field)
        | None -> fields.(i) <- Some value
      ) l;
    fields

  let mk_record missing = function
    | [] -> raise (Invalid_argument "Dolmen.Expr.record")
    | ((f, _) :: _) as l ->
      begin match f.cst_builtin with
        | Builtin.Destructor { adt = ty_c; cstr = c; _ } when Core.is_record ty_c ->
          let fields = build_record_fields ty_c l in
          (* Check that all fields are indeed present, and create the list
             of term arguments *)
          let t_args = Array.to_list @@ Array.mapi (fun i o ->
              match o with
              | None -> missing ty_c i
              | Some v -> v
            ) fields in
          (* Create type wildcard to be unified during application. *)
          let vars, _, _ = Ty.as_poly_sig c.cst_ty in
          let ty_args = init_list (List.length vars) (fun _ -> Core.wildcard ()) in
          apply_fo c ty_args t_args
        | _ ->
          raise (Field_expected f)
      end

  let record l =
    mk_record (fun ty_c i -> raise (Field_missing (Field.find ty_c i))) l

  let record_with t = function
    | [] -> t
    | l ->
      let aux ty_c i =
        let f = Field.find ty_c i in
        Field.apply f t
      in
      mk_record aux l


  (* ===== Term Constants Namespace ===== *)

  module Cst = struct

    (* Some helpers *)
    let make_fo ?pos ?name ?builtin s vars args ret =
      Cst.make ?pos ?name ?builtin s (Ty.poly_sig vars args ret)

    let alpha () =
      let var = Var.mk "α" Core.type_ in
      var, of_var var

    let beta () =
      let var = Var.mk "β" Core.type_ in
      var, of_var var

    (* Some constants *)
    let _true =
      make_fo ~name:"⊤" ~builtin:Builtin.True "True" [] [] Ty.prop

    let _false =
      make_fo ~name:"⊥" ~builtin:Builtin.False "False" [] [] Ty.prop

    let eq =
      let a, a_ty = alpha () in
      make_fo
        ~pos:Pretty.Infix ~name:"=" ~builtin:Builtin.Equal
        "Equal" [a] [a_ty; a_ty] Ty.prop

    let eqs =
      let a, a_ty = alpha () in
      with_cache ~cache:(Hashtbl.create 13) (fun i ->
          make_fo "Equals" [a] (replicate i a_ty) Ty.prop
            ~pos:Pretty.Infix ~name:"=" ~builtin:Builtin.Equal
        )

    let distinct =
      let a, a_ty = alpha () in
      with_cache ~cache:(Hashtbl.create 13) (fun i ->
          make_fo "Distinct" [a] (replicate i a_ty) Ty.prop
            ~name:"distinct" ~builtin:Builtin.Distinct
        )

    let neg = make_fo "Neg" [] [Ty.prop] Ty.prop
        ~pos:Pretty.Prefix ~name:"¬" ~builtin:Builtin.Neg

    let _and =
      with_cache ~cache:(Hashtbl.create 13) (fun i ->
          make_fo "And" [] (replicate i Ty.prop) Ty.prop
            ~pos:Pretty.Infix ~name:"∧" ~builtin:Builtin.And
        )

    let _or =
      with_cache ~cache:(Hashtbl.create 13) (fun i ->
          make_fo "Or" [] (replicate i Ty.prop) Ty.prop
            ~pos:Pretty.Infix ~name:"∨" ~builtin:Builtin.Or
        )

    let nand = make_fo "Nand" [] [Ty.prop; Ty.prop] Ty.prop
        ~pos:Pretty.Infix ~name:"⊼" ~builtin:Builtin.Nand

    let nor = make_fo "or" [] [Ty.prop; Ty.prop] Ty.prop
        ~pos:Pretty.Infix ~name:"V" ~builtin:Builtin.Nor

    let xor = make_fo "Xor" [] [Ty.prop; Ty.prop] Ty.prop
        ~pos:Pretty.Infix ~name:"⊻" ~builtin:Builtin.Xor

    let imply = make_fo "Imply" [] [Ty.prop; Ty.prop] Ty.prop
        ~pos:Pretty.Infix ~name:"⇒" ~builtin:Builtin.Imply

    let equiv = make_fo "Equiv" [] [Ty.prop; Ty.prop] Ty.prop
        ~pos:Pretty.Infix ~name:"⇔" ~builtin:Builtin.Equiv

    let ite =
      let a, a_ty = alpha () in
      make_fo ~name:"ite" ~builtin:Builtin.Ite
        "Ite" [a] [Ty.prop; a_ty; a_ty] a_ty

    let select =
      let a, a_ty = alpha () in
      let b, b_ty = beta () in
      make_fo ~name:"select" ~builtin:Builtin.Select
        "Select" [a; b] [Ty.array a_ty b_ty; a_ty] b_ty

    let store =
      let a, a_ty = alpha () in
      let b, b_ty = beta () in
      let arr = Ty.array a_ty b_ty in
      make_fo ~name:"store" ~builtin:Builtin.Store
        "Store" [a; b] [arr; a_ty; b_ty] arr

    let coerce =
      let a, a_ty = alpha () in
      let b, b_ty = beta () in
      make_fo ~builtin:Builtin.Coercion "coerce"
        [a; b] [a_ty] b_ty

    module Int = struct

      let int =
        with_cache ~cache:(Hashtbl.create 113) (fun s ->
            make_fo ~builtin:(Builtin.Integer s) s [] [] Ty.int
          )

      let minus = make_fo "Minus" [] [Ty.int] Ty.int
          ~pos:Pretty.Prefix ~name:"-" ~builtin:Builtin.Minus

      let add = make_fo "Add" [] [Ty.int; Ty.int] Ty.int
          ~pos:Pretty.Infix ~name:"+" ~builtin:Builtin.Add

      let sub = make_fo "Sub" [] [Ty.int; Ty.int] Ty.int
          ~pos:Pretty.Infix ~name:"-" ~builtin:Builtin.Sub

      let mul = make_fo "Mul" [] [Ty.int; Ty.int] Ty.int
          ~pos:Pretty.Infix ~name:"*" ~builtin:Builtin.Mul

      let div_e = make_fo "Div_e" [] [Ty.int; Ty.int] Ty.int
          ~pos:Pretty.Infix ~name:"/" ~builtin:Builtin.Div_e
      let div_t = make_fo "Div_t" [] [Ty.int; Ty.int] Ty.int
          ~pos:Pretty.Infix ~name:"/t" ~builtin:Builtin.Div_t
      let div_f = make_fo "Div_f" [] [Ty.int; Ty.int] Ty.int
          ~pos:Pretty.Infix ~name:"/f" ~builtin:Builtin.Div_f

      let rem_e = make_fo "Modulo" [] [Ty.int; Ty.int] Ty.int
          ~pos:Pretty.Infix ~name:"%" ~builtin:Builtin.Modulo_e
      let rem_t = make_fo "Modulo" [] [Ty.int; Ty.int] Ty.int
          ~pos:Pretty.Infix ~name:"%e" ~builtin:Builtin.Modulo_t
      let rem_f = make_fo "Modulo" [] [Ty.int; Ty.int] Ty.int
          ~pos:Pretty.Infix ~name:"%f" ~builtin:Builtin.Modulo_f

      let abs = make_fo "Abs" [] [Ty.int] Ty.int
          ~name:"abs" ~builtin:Builtin.Abs

      let lt = make_fo "LessThan" [] [Ty.int; Ty.int] Ty.prop
          ~pos:Pretty.Infix ~name:"<" ~builtin:Builtin.Lt

      let le = make_fo "LessOrEqual" [] [Ty.int; Ty.int] Ty.prop
          ~pos:Pretty.Infix ~name:"<=" ~builtin:Builtin.Leq

      let gt = make_fo "GreaterThan" [] [Ty.int; Ty.int] Ty.prop
          ~pos:Pretty.Infix ~name:">" ~builtin:Builtin.Gt

      let ge = make_fo "GreaterOrEqual" [] [Ty.int; Ty.int] Ty.prop
          ~pos:Pretty.Infix ~name:">=" ~builtin:Builtin.Geq

      let floor = make_fo "Floor" [] [Ty.int] Ty.int
          ~name:"floor" ~builtin:Builtin.Floor

      let ceiling = make_fo "Ceiling" [] [Ty.int] Ty.int
          ~name:"ceiling" ~builtin:Builtin.Ceiling

      let truncate = make_fo "Truncate" [] [Ty.int] Ty.int
          ~name:"truncate" ~builtin:Builtin.Truncate

      let round = make_fo "Round" [] [Ty.int] Ty.int
          ~name:"round" ~builtin:Builtin.Round

      let is_int = make_fo "Is_int" [] [Ty.int] Ty.prop
          ~name:"is_int" ~builtin:Builtin.Is_int

      let is_rat = make_fo "Is_rat" [] [Ty.int] Ty.prop
          ~name:"is_rat" ~builtin:Builtin.Is_rat

      let divisible = make_fo "Divisible" [] [Ty.int; Ty.int] Ty.prop
          ~name:"divisible" ~builtin:Builtin.Divisible

    end

    module Rat = struct

      let rat =
        with_cache ~cache:(Hashtbl.create 113) (fun s ->
            make_fo ~builtin:(Builtin.Rational s) s [] [] Ty.rat
          )

      let minus = make_fo "Minus" [] [Ty.rat] Ty.rat
          ~pos:Pretty.Prefix ~name:"-" ~builtin:Builtin.Minus

      let add = make_fo "Add" [] [Ty.rat; Ty.rat] Ty.rat
          ~pos:Pretty.Infix ~name:"+" ~builtin:Builtin.Add

      let sub = make_fo "Sub" [] [Ty.rat; Ty.rat] Ty.rat
          ~pos:Pretty.Infix ~name:"-" ~builtin:Builtin.Sub

      let mul = make_fo "Mul" [] [Ty.rat; Ty.rat] Ty.rat
          ~pos:Pretty.Infix ~name:"*" ~builtin:Builtin.Mul

      let div = make_fo "Div" [] [Ty.rat; Ty.rat] Ty.rat
          ~pos:Pretty.Infix ~name:"/" ~builtin:Builtin.Div
      let div_e = make_fo "Div_e" [] [Ty.rat; Ty.rat] Ty.rat
          ~pos:Pretty.Infix ~name:"/e" ~builtin:Builtin.Div_e
      let div_t = make_fo "Div_t" [] [Ty.rat; Ty.rat] Ty.rat
          ~pos:Pretty.Infix ~name:"/t" ~builtin:Builtin.Div_t
      let div_f = make_fo "Div_f" [] [Ty.rat; Ty.rat] Ty.rat
          ~pos:Pretty.Infix ~name:"/f" ~builtin:Builtin.Div_f

      let rem_e = make_fo "Modulo" [] [Ty.rat; Ty.rat] Ty.rat
          ~pos:Pretty.Infix ~name:"%" ~builtin:Builtin.Modulo_e
      let rem_t = make_fo "Modulo" [] [Ty.rat; Ty.rat] Ty.rat
          ~pos:Pretty.Infix ~name:"%" ~builtin:Builtin.Modulo_t
      let rem_f = make_fo "Modulo" [] [Ty.rat; Ty.rat] Ty.rat
          ~pos:Pretty.Infix ~name:"%" ~builtin:Builtin.Modulo_f

      let lt = make_fo "LessThan" [] [Ty.rat; Ty.rat] Ty.prop
          ~pos:Pretty.Infix ~name:"<" ~builtin:Builtin.Lt
      let le = make_fo "LessOrEqual" [] [Ty.rat; Ty.rat] Ty.prop
          ~pos:Pretty.Infix ~name:"<=" ~builtin:Builtin.Leq
      let gt = make_fo "GreaterThan" [] [Ty.rat; Ty.rat] Ty.prop
          ~pos:Pretty.Infix ~name:">" ~builtin:Builtin.Gt
      let ge = make_fo "GreaterOrEqual" [] [Ty.rat; Ty.rat] Ty.prop
          ~pos:Pretty.Infix ~name:">=" ~builtin:Builtin.Geq

      let floor = make_fo "Floor" [] [Ty.rat] Ty.rat
          ~name:"floor" ~builtin:Builtin.Floor

      let ceiling = make_fo "Ceiling" [] [Ty.rat] Ty.rat
          ~name:"ceiling" ~builtin:Builtin.Ceiling

      let truncate = make_fo "Truncate" [] [Ty.rat] Ty.rat
          ~name:"truncate" ~builtin:Builtin.Truncate

      let round = make_fo "Round" [] [Ty.rat] Ty.rat
          ~name:"round" ~builtin:Builtin.Round

      let is_int = make_fo "Is_int" [] [Ty.rat] Ty.prop
          ~name:"is_int" ~builtin:Builtin.Is_int

      let is_rat = make_fo "Is_rat" [] [Ty.rat] Ty.prop
          ~name:"is_rat" ~builtin:Builtin.Is_rat

    end

    module Real = struct

      let real =
        with_cache ~cache:(Hashtbl.create 113) (fun s ->
            make_fo ~builtin:(Builtin.Decimal s) s [] [] Ty.real
          )

      let minus = make_fo "Minus" [] [Ty.real] Ty.real
          ~pos:Pretty.Prefix ~name:"-" ~builtin:Builtin.Minus

      let add = make_fo "Add" [] [Ty.real; Ty.real] Ty.real
          ~pos:Pretty.Infix ~name:"+" ~builtin:Builtin.Add

      let sub = make_fo "Sub" [] [Ty.real; Ty.real] Ty.real
          ~pos:Pretty.Infix ~name:"-" ~builtin:Builtin.Sub

      let mul = make_fo "Mul" [] [Ty.real; Ty.real] Ty.real
          ~pos:Pretty.Infix ~name:"*" ~builtin:Builtin.Mul

      let div = make_fo "Div" [] [Ty.real; Ty.real] Ty.real
          ~pos:Pretty.Infix ~name:"/" ~builtin:Builtin.Div

      let div_e = make_fo "Div_e" [] [Ty.real; Ty.real] Ty.real
          ~pos:Pretty.Infix ~name:"/" ~builtin:Builtin.Div_e
      let div_t = make_fo "Div_t" [] [Ty.real; Ty.real] Ty.real
          ~pos:Pretty.Infix ~name:"/t" ~builtin:Builtin.Div_t
      let div_f = make_fo "Div_f" [] [Ty.real; Ty.real] Ty.real
          ~pos:Pretty.Infix ~name:"/f" ~builtin:Builtin.Div_f

      let rem_e = make_fo "Modulo" [] [Ty.real; Ty.real] Ty.real
          ~pos:Pretty.Infix ~name:"%" ~builtin:Builtin.Modulo_e
      let rem_t = make_fo "Modulo" [] [Ty.real; Ty.real] Ty.real
          ~pos:Pretty.Infix ~name:"%" ~builtin:Builtin.Modulo_t
      let rem_f = make_fo "Modulo" [] [Ty.real; Ty.real] Ty.real
          ~pos:Pretty.Infix ~name:"%" ~builtin:Builtin.Modulo_f

      let lt = make_fo "LessThan" [] [Ty.real; Ty.real] Ty.prop
          ~pos:Pretty.Infix ~name:"<" ~builtin:Builtin.Lt
      let le = make_fo "LessOrEqual" [] [Ty.real; Ty.real] Ty.prop
          ~pos:Pretty.Infix ~name:"<=" ~builtin:Builtin.Leq
      let gt = make_fo "GreaterThan" [] [Ty.real; Ty.real] Ty.prop
          ~pos:Pretty.Infix ~name:">" ~builtin:Builtin.Gt
      let ge = make_fo "GreaterOrEqual" [] [Ty.real; Ty.real] Ty.prop
          ~pos:Pretty.Infix ~name:">=" ~builtin:Builtin.Geq

      let floor = make_fo "Floor" [] [Ty.real] Ty.real
          ~name:"floor" ~builtin:Builtin.Floor

      let ceiling = make_fo "Ceiling" [] [Ty.real] Ty.real
          ~name:"ceiling" ~builtin:Builtin.Ceiling

      let truncate = make_fo "Truncate" [] [Ty.real] Ty.real
          ~name:"truncate" ~builtin:Builtin.Truncate

      let round = make_fo "Round" [] [Ty.real] Ty.real
          ~name:"round" ~builtin:Builtin.Round

      let is_int = make_fo "Is_int" [] [Ty.real] Ty.prop
          ~name:"is_int" ~builtin:Builtin.Is_int

      let is_rat = make_fo "Is_rat" [] [Ty.real] Ty.prop
          ~name:"is_rat" ~builtin:Builtin.Is_rat

    end

    module Bitv = struct

      let bitv s =
        make_fo ~builtin:(Builtin.Bitvec s)
          (Format.asprintf "bv#%s#" s) [] [] (Ty.bitv (String.length s))

      let concat =
        with_cache ~cache:(Hashtbl.create 13) (fun (i, j) ->
            make_fo ~builtin:Builtin.Bitv_concat "bitv_concat"
              [] [Ty.bitv i; Ty.bitv j] (Ty.bitv (i + j))
          )

      let extract =
        with_cache ~cache:(Hashtbl.create 13) (fun (i, j, n) ->
            make_fo ~builtin:(Builtin.Bitv_extract (i, j))
              (Format.asprintf "bitv_extract_%d_%d" i j) []
              [Ty.bitv n] (Ty.bitv (i - j + 1))
          )

      let repeat =
        with_cache ~cache:(Hashtbl.create 13) (fun (k, n) ->
            make_fo ~builtin:Builtin.Bitv_repeat
              (Format.asprintf "bitv_repeat_%d" k)
              [] [Ty.bitv n] (Ty.bitv (n * k))
          )

      let zero_extend =
        with_cache ~cache:(Hashtbl.create 13) (fun (k, n) ->
            make_fo ~builtin:Builtin.Bitv_zero_extend
              (Format.asprintf "zero_extend_%d" k)
              [] [Ty.bitv n] (Ty.bitv (n + k))
          )

      let sign_extend =
        with_cache ~cache:(Hashtbl.create 13) (fun (k, n) ->
            make_fo ~builtin:Builtin.Bitv_sign_extend
              (Format.asprintf "sign_extend_%d" k)
              [] [Ty.bitv n] (Ty.bitv (n + k))
          )

      let rotate_right =
        with_cache ~cache:(Hashtbl.create 13) (fun (k, n) ->
            make_fo ~builtin:(Builtin.Bitv_rotate_right k)
              (Format.asprintf "rotate_right_%d" k)
              [] [Ty.bitv n] (Ty.bitv n)
          )

      let rotate_left =
        with_cache ~cache:(Hashtbl.create 13) (fun (k, n) ->
            make_fo ~builtin:(Builtin.Bitv_rotate_left k)
              (Format.asprintf "rotate_left_%d" k)
              [] [Ty.bitv n] (Ty.bitv n)
          )

      let not =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            make_fo ~builtin:Builtin.Bitv_not
              "bvnot" [] [Ty.bitv n] (Ty.bitv n)
          )

      let and_ =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            make_fo ~builtin:Builtin.Bitv_and
              "bvand" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let or_ =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            make_fo ~builtin:Builtin.Bitv_or
              "bvor" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let nand =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            make_fo ~builtin:Builtin.Bitv_nand
              "bvnand" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let nor =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            make_fo ~builtin:Builtin.Bitv_nor
              "bvnor" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let xor =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            make_fo ~builtin:Builtin.Bitv_xor
              "bvxor" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let xnor =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            make_fo ~builtin:Builtin.Bitv_xnor
              "bvxnor" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let comp =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            make_fo ~builtin:Builtin.Bitv_comp
              "bvcomp" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv 1)
          )

      let neg =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            make_fo ~builtin:Builtin.Bitv_neg
              "bvneg" [] [Ty.bitv n] (Ty.bitv n)
          )

      let add =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            make_fo ~builtin:Builtin.Bitv_add
              "bvadd" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let sub =
      with_cache ~cache:(Hashtbl.create 13) (fun n ->
            make_fo ~builtin:Builtin.Bitv_sub
              "bvsub" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
        )

      let mul =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            make_fo ~builtin:Builtin.Bitv_mul
              "bvmul" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let udiv =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            make_fo ~builtin:Builtin.Bitv_udiv
              "bvudiv" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let urem =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            make_fo ~builtin:Builtin.Bitv_urem
              "bvurem" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let sdiv =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            make_fo ~builtin:Builtin.Bitv_sdiv
              "bvsdiv" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let srem =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            make_fo ~builtin:Builtin.Bitv_srem
              "bvsrem" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let smod =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            make_fo ~builtin:Builtin.Bitv_smod
              "bvsmod" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let shl =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            make_fo ~builtin:Builtin.Bitv_shl
              "bvshl" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let lshr =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            make_fo ~builtin:Builtin.Bitv_lshr
              "bvlshr" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let ashr =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            make_fo ~builtin:Builtin.Bitv_ashr
              "bvashr" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let ult =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            make_fo ~builtin:Builtin.Bitv_ult
              "bvult" [] [Ty.bitv n; Ty.bitv n] Ty.prop
          )

      let ule =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            make_fo ~builtin:Builtin.Bitv_ule
              "bvule" [] [Ty.bitv n; Ty.bitv n] Ty.prop
          )

      let ugt =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            make_fo ~builtin:Builtin.Bitv_ugt
              "bvugt" [] [Ty.bitv n; Ty.bitv n] Ty.prop
          )

      let uge =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            make_fo ~builtin:Builtin.Bitv_uge
              "bvsge" [] [Ty.bitv n; Ty.bitv n] Ty.prop
          )

      let slt =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            make_fo ~builtin:Builtin.Bitv_slt
              "bvslt" [] [Ty.bitv n; Ty.bitv n] Ty.prop
          )

      let sle =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            make_fo ~builtin:Builtin.Bitv_sle
              "bvsle" [] [Ty.bitv n; Ty.bitv n] Ty.prop
          )

      let sgt =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            make_fo ~builtin:Builtin.Bitv_sgt
              "bvsgt" [] [Ty.bitv n; Ty.bitv n] Ty.prop
          )

      let sge =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            make_fo ~builtin:Builtin.Bitv_sge
              "bvsge" [] [Ty.bitv n; Ty.bitv n] Ty.prop
          )

    end

    module Float = struct

      let fp =
        with_cache ~cache:(Hashtbl.create 13) (fun (e, s) ->
            make_fo ~builtin:(Builtin.Fp(e, s)) "fp" []
              [Ty.bitv 1; Ty.bitv e; Ty.bitv (s-1)] (Ty.float e s)
          )

      let roundNearestTiesToEven =
        make_fo ~builtin:Builtin.RoundNearestTiesToEven
          "RoundNearestTiesToEven" [] [] Ty.roundingMode

      let roundNearestTiesToAway =
        make_fo ~builtin:Builtin.RoundNearestTiesToAway
          "RoundNearestTiesToAway" [] [] Ty.roundingMode

      let roundTowardPositive =
        make_fo ~builtin:Builtin.RoundTowardPositive
          "RoundTowardPositive" [] [] Ty.roundingMode

      let roundTowardNegative =
        make_fo ~builtin:Builtin.RoundTowardNegative
          "RoundTowardNegative" [] [] Ty.roundingMode

      let roundTowardZero =
        make_fo ~builtin:Builtin.RoundTowardZero
          "RoundTowardZero" [] [] Ty.roundingMode

      (** Generic function for creating functions primarily on the same floating
         point format with optionally a rounding mode and a particular result
         type *)
      let fp_gen_fun ~args ?rm ?res name builtin =
        with_cache ~cache:(Hashtbl.create 13) (fun es ->
            let fp = Ty.float' es in
            let args = List.init args (fun _ -> fp) in
            let args = match rm with None -> args | Some () -> Ty.roundingMode::args in
            let res =
              match res with
              | Some res -> res
              | None -> fp
            in
            make_fo ~builtin:(builtin es) name [] args res
          )

      let plus_infinity = fp_gen_fun ~args:0 "plus_infinity"
          (fun (e,s) -> Builtin.Plus_infinity (e,s))
      let minus_infinity = fp_gen_fun ~args:0 "minus_infinity"
          (fun (e,s) -> Builtin.Minus_infinity (e,s))
      let plus_zero = fp_gen_fun ~args:0 "plus_zero"
          (fun (e,s) -> Builtin.Plus_zero (e,s))
      let minus_zero = fp_gen_fun ~args:0 "minus_zero"
          (fun (e,s) -> Builtin.Minus_zero (e,s))
      let nan = fp_gen_fun ~args:0 "nan"
          (fun (e,s) -> Builtin.NaN (e,s))
      let abs = fp_gen_fun ~args:1 "fp.abs"
          (fun (e,s) -> Builtin.Fp_abs (e,s))
      let neg = fp_gen_fun ~args:1 "fp.neg"
          (fun (e,s) -> Builtin.Fp_neg (e,s))
      let add = fp_gen_fun ~args:2 ~rm:() "fp.add"
          (fun (e,s) -> Builtin.Fp_add (e,s))
      let sub = fp_gen_fun ~args:2 ~rm:() "fp.sub"
          (fun (e,s) -> Builtin.Fp_sub (e,s))
      let mul = fp_gen_fun ~args:2 ~rm:() "fp.mul"
          (fun (e,s) -> Builtin.Fp_mul (e,s))
      let div = fp_gen_fun ~args:2 ~rm:() "fp.div"
          (fun (e,s) -> Builtin.Fp_div (e,s))
      let fma = fp_gen_fun ~args:3 ~rm:() "fp.fma"
          (fun (e,s) -> Builtin.Fp_fma (e,s))
      let sqrt = fp_gen_fun ~args:1 ~rm:() "fp.sqrt"
          (fun (e,s) -> Builtin.Fp_sqrt (e,s))
      let rem = fp_gen_fun ~args:2 "fp.rem"
          (fun (e,s) -> Builtin.Fp_rem (e,s))
      let roundToIntegral = fp_gen_fun ~args:1 ~rm:() "fp.roundToIntegral"
          (fun (e,s) -> Builtin.Fp_roundToIntegral (e,s))
      let min = fp_gen_fun ~args:2 "fp.min"
          (fun (e,s) -> Builtin.Fp_min (e,s))
      let max = fp_gen_fun ~args:2 "fp.max"
          (fun (e,s) -> Builtin.Fp_max (e,s))
      let leq = fp_gen_fun ~args:2 ~res:Ty.prop "fp.leq"
          (fun (e,s) -> Builtin.Fp_leq (e,s))
      let lt = fp_gen_fun ~args:2 ~res:Ty.prop "fp.lt"
          (fun (e,s) -> Builtin.Fp_lt (e,s))
      let geq = fp_gen_fun ~args:2 ~res:Ty.prop "fp.geq"
          (fun (e,s) -> Builtin.Fp_geq (e,s))
      let gt = fp_gen_fun ~args:2 ~res:Ty.prop "fp.gt"
          (fun (e,s) -> Builtin.Fp_gt (e,s))
      let eq = fp_gen_fun ~args:2 ~res:Ty.prop "fp.eq"
          (fun (e,s) -> Builtin.Fp_eq (e,s))
      let isNormal = fp_gen_fun ~args:1 ~res:Ty.prop "fp.isnormal"
          (fun (e,s) -> Builtin.Fp_isNormal (e,s))
      let isSubnormal = fp_gen_fun ~args:1 ~res:Ty.prop "fp.issubnormal"
          (fun (e,s) -> Builtin.Fp_isSubnormal (e,s))
      let isZero = fp_gen_fun ~args:1 ~res:Ty.prop "fp.iszero"
          (fun (e,s) -> Builtin.Fp_isZero (e,s))
      let isInfinite = fp_gen_fun ~args:1 ~res:Ty.prop "fp.isinfinite"
          (fun (e,s) -> Builtin.Fp_isInfinite (e,s))
      let isNaN = fp_gen_fun ~args:1 ~res:Ty.prop "fp.isnan"
          (fun (e,s) -> Builtin.Fp_isNaN (e,s))
      let isNegative = fp_gen_fun ~args:1 ~res:Ty.prop "fp.isnegative"
          (fun (e,s) -> Builtin.Fp_isNegative (e,s))
      let isPositive = fp_gen_fun ~args:1 ~res:Ty.prop "fp.ispositive"
          (fun (e,s) -> Builtin.Fp_isPositive (e,s))
      let to_real = fp_gen_fun ~args:1 ~res:Ty.real "fp.to_real"
          (fun (e,s) -> Builtin.To_real (e,s))

      let ieee_format_to_fp =
        with_cache ~cache:(Hashtbl.create 13) (fun ((e,s) as es) ->
            make_fo ~builtin:(Builtin.Ieee_format_to_fp (e,s))
              "to_fp" [] [Ty.bitv (e+s)] (Ty.float' es)
          )
      let to_fp =
        with_cache ~cache:(Hashtbl.create 13) (fun (e1,s1,e2,s2) ->
            make_fo ~builtin:(Builtin.Fp_to_fp (e1,s1,e2,s2))
              "to_fp" [] [Ty.roundingMode;Ty.float e1 s1] (Ty.float e2 s2)
          )
      let real_to_fp =
        with_cache ~cache:(Hashtbl.create 13) (fun ((e,s) as es) ->
            make_fo ~builtin:(Builtin.Real_to_fp (e,s))
              "to_fp" [] [Ty.roundingMode;Ty.real] (Ty.float' es)
          )
      let sbv_to_fp =
        with_cache ~cache:(Hashtbl.create 13) (fun (bv,e,s) ->
            make_fo ~builtin:(Builtin.Sbv_to_fp (bv,e,s))
              "to_fp" [] [Ty.roundingMode;Ty.bitv bv] (Ty.float e s)
          )
      let ubv_to_fp =
        with_cache ~cache:(Hashtbl.create 13) (fun (bv,e,s) ->
            make_fo ~builtin:(Builtin.Ubv_to_fp (bv,e,s))
              "to_fp" [] [Ty.roundingMode;Ty.bitv bv] (Ty.float e s)
          )
      let to_ubv =
        with_cache ~cache:(Hashtbl.create 13) (fun (e,s,bv) ->
            make_fo ~builtin:(Builtin.To_ubv (bv,e,s))
              "fp.to_ubv" [] [Ty.roundingMode;Ty.float e s] (Ty.bitv bv)
          )
      let to_sbv =
        with_cache ~cache:(Hashtbl.create 13) (fun (e,s,bv) ->
            make_fo ~builtin:(Builtin.To_sbv (bv,e,s))
              "fp.to_sbv" [] [Ty.roundingMode;Ty.float e s] (Ty.bitv bv)
          )

    end

    module String = struct

      let string =
        with_cache ~cache:(Hashtbl.create 13) (fun s ->
            make_fo ~builtin:(Builtin.Str s)
              (Format.asprintf {|"%s"|} s) [] [] Ty.string
          )

      let length =
        make_fo ~builtin:Builtin.Str_length "length"
          [] [Ty.string] Ty.int
      let at =
        make_fo ~builtin:Builtin.Str_at "at"
          [] [Ty.string; Ty.int] Ty.string
      let to_code =
        make_fo ~builtin:Builtin.Str_to_code "to_code"
          [] [Ty.string] Ty.int
      let of_code =
        make_fo ~builtin:Builtin.Str_of_code "of_code"
          [] [Ty.int] Ty.string
      let is_digit =
        make_fo ~builtin:Builtin.Str_is_digit "is_digit"
          [] [Ty.string] Ty.prop
      let to_int =
        make_fo ~builtin:Builtin.Str_to_int "to_int"
          [] [Ty.string] Ty.int
      let of_int =
        make_fo ~builtin:Builtin.Str_of_int "of_int"
          [] [Ty.int] Ty.string
      let concat =
        make_fo ~builtin:Builtin.Str_concat ~pos:Pretty.Infix "++"
          [] [Ty.string; Ty.string] Ty.string
      let sub =
        make_fo ~builtin:Builtin.Str_sub "sub"
          [] [Ty.string; Ty.int; Ty.int] Ty.string
      let index_of =
        make_fo ~builtin:Builtin.Str_index_of "index_of"
          [] [Ty.string; Ty.string; Ty.int] Ty.int
      let replace =
        make_fo ~builtin:Builtin.Str_replace "replace"
          [] [Ty.string; Ty.string; Ty.string] Ty.string
      let replace_all =
        make_fo ~builtin:Builtin.Str_replace_all "replace_all"
          [] [Ty.string; Ty.string; Ty.string] Ty.string
      let replace_re =
        make_fo ~builtin:Builtin.Str_replace_re "replace_re"
          [] [Ty.string; Ty.string_reg_lang; Ty.string] Ty.string
      let replace_re_all =
        make_fo ~builtin:Builtin.Str_replace_re_all "replace_re_all"
          [] [Ty.string; Ty.string_reg_lang; Ty.string] Ty.string
      let is_prefix =
        make_fo ~builtin:Builtin.Str_is_prefix "is_prefix"
          [] [Ty.string; Ty.string] Ty.prop
      let is_suffix =
        make_fo ~builtin:Builtin.Str_is_suffix "is_suffix"
          [] [Ty.string; Ty.string] Ty.prop
      let contains =
        make_fo ~builtin:Builtin.Str_contains "contains"
          [] [Ty.string; Ty.string] Ty.prop
      let lt =
        make_fo ~builtin:Builtin.Str_lexicographic_strict
          ~pos:Pretty.Infix "lt"
          [] [Ty.string; Ty.string] Ty.prop
      let leq =
        make_fo ~builtin:Builtin.Str_lexicographic_large
          ~pos:Pretty.Infix "leq"
          [] [Ty.string; Ty.string] Ty.prop
      let in_re =
        make_fo ~builtin:Builtin.Str_in_re "in_re"
          [] [Ty.string; Ty.string_reg_lang] Ty.prop

      module Reg_Lang = struct

        let empty =
          make_fo ~builtin:Builtin.Re_empty "empty"
            [] [] Ty.string_reg_lang
        let all =
          make_fo ~builtin:Builtin.Re_all "all"
            [] [] Ty.string_reg_lang
        let allchar =
          make_fo ~builtin:Builtin.Re_allchar "allchar"
            [] [] Ty.string_reg_lang
        let of_string =
          make_fo ~builtin:Builtin.Re_of_string "of_string"
            [] [Ty.string] Ty.string_reg_lang
        let range =
          make_fo ~builtin:Builtin.Re_range "range"
            [] [Ty.string; Ty.string] Ty.string_reg_lang
        let concat =
          make_fo ~builtin:Builtin.Re_concat ~pos:Pretty.Infix "++"
            [] [Ty.string_reg_lang; Ty.string_reg_lang] Ty.string_reg_lang
        let union =
          make_fo ~builtin:Builtin.Re_union ~pos:Pretty.Infix "∪"
            [] [Ty.string_reg_lang; Ty.string_reg_lang] Ty.string_reg_lang
        let inter =
          make_fo ~builtin:Builtin.Re_inter ~pos:Pretty.Infix "∩"
            [] [Ty.string_reg_lang; Ty.string_reg_lang] Ty.string_reg_lang
        let diff =
          make_fo ~builtin:Builtin.Re_diff ~pos:Pretty.Infix "-"
            [] [Ty.string_reg_lang; Ty.string_reg_lang] Ty.string_reg_lang
        let star =
          make_fo ~builtin:Builtin.Re_star ~pos:Pretty.Prefix "*"
            [] [Ty.string_reg_lang] Ty.string_reg_lang
        let cross =
          make_fo ~builtin:Builtin.Re_cross ~pos:Pretty.Prefix "+"
            [] [Ty.string_reg_lang] Ty.string_reg_lang
        let complement =
          make_fo ~builtin:Builtin.Re_complement "complement"
            [] [Ty.string_reg_lang] Ty.string_reg_lang
        let option =
          make_fo ~builtin:Builtin.Re_option "option"
            [] [Ty.string_reg_lang] Ty.string_reg_lang
        let power =
          with_cache ~cache:(Hashtbl.create 13) (fun n ->
              make_fo ~builtin:(Builtin.Re_power n)
                (Format.asprintf "power_%d" n)
                [] [Ty.string_reg_lang] Ty.string_reg_lang
            )
        let loop =
          with_cache ~cache:(Hashtbl.create 13) (fun (n1, n2) ->
              make_fo ~builtin:(Builtin.Re_loop (n1, n2))
                (Format.asprintf "loop_%d_%d" n1 n2)
                [] [Ty.string_reg_lang] Ty.string_reg_lang
            )

      end

    end

  end

  (* if-then-else *)
  let ite cond t_then t_else =
    let ty = Core.ty t_then in
    apply_fo Cst.ite [ty] [cond; t_then; t_else]

  (* coercion *)
  let coerce dst_ty t =
    let src_ty = Core.ty t in
    apply_fo Cst.coerce [src_ty; dst_ty] [t]

  (* Common constructions *)
  let void = apply_fo Cstr.void [] []

  let _true = apply_fo Cst._true [] []
  let _false = apply_fo Cst._false [] []

  let eq a b = apply_fo Cst.eq [Core.ty a] [a; b]

  let eqs = function
    | [] -> raise (Bad_arity (Cst.eqs 2, []))
    | (h :: _) as l ->
      apply_fo (Cst.eqs (List.length l)) [Core.ty h] l

  let distinct = function
    | [] -> raise (Bad_arity (Cst.distinct 2, []))
    | (h :: _) as l ->
      apply_fo (Cst.distinct (List.length l)) [Core.ty h] l

  let neg x = apply_fo Cst.neg [] [x]

  let _and l = apply_fo (Cst._and (List.length l)) [] l

  let _or l = apply_fo (Cst._or (List.length l)) [] l

  let nand p q = apply_fo Cst.nand [] [p; q]

  let nor p q = apply_fo Cst.nor [] [p; q]

  let xor p q = apply_fo Cst.xor [] [p; q]

  let imply p q = apply_fo Cst.imply [] [p; q]

  let equiv p q = apply_fo Cst.equiv [] [p; q]

  let int s = apply_fo (Cst.Int.int s) [] []
  let rat s = apply_fo (Cst.Rat.rat s) [] []
  let real s = apply_fo (Cst.Real.real s) [] []

  (* arithmetic *)
  module Int = struct
    let mk = int
    let minus t = apply_fo Cst.Int.minus [] [t]
    let add a b = apply_fo Cst.Int.add [] [a; b]
    let sub a b = apply_fo Cst.Int.sub [] [a; b]
    let mul a b = apply_fo Cst.Int.mul [] [a; b]
    let div a b = apply_fo Cst.Int.div_e [] [a; b]
    let rem a b = apply_fo Cst.Int.rem_e [] [a; b]
    let div_e a b = apply_fo Cst.Int.div_e [] [a; b]
    let div_t a b = apply_fo Cst.Int.div_t [] [a; b]
    let div_f a b = apply_fo Cst.Int.div_f [] [a; b]
    let rem_e a b = apply_fo Cst.Int.rem_e [] [a; b]
    let rem_t a b = apply_fo Cst.Int.rem_t [] [a; b]
    let rem_f a b = apply_fo Cst.Int.rem_f [] [a; b]
    let abs a = apply_fo Cst.Int.abs [] [a]
    let lt a b = apply_fo Cst.Int.lt [] [a; b]
    let le a b = apply_fo Cst.Int.le [] [a; b]
    let gt a b = apply_fo Cst.Int.gt [] [a; b]
    let ge a b = apply_fo Cst.Int.ge [] [a; b]
    let floor a = apply_fo Cst.Int.floor [] [a]
    let ceiling a = apply_fo Cst.Int.ceiling [] [a]
    let truncate a = apply_fo Cst.Int.truncate [] [a]
    let round a = apply_fo Cst.Int.round [] [a]
    let is_int a = apply_fo Cst.Int.is_int [] [a]
    let is_rat a = apply_fo Cst.Int.is_rat [] [a]
    let to_int t = coerce Ty.int t
    let to_rat t = coerce Ty.rat t
    let to_real t = coerce Ty.real t
    let divisible s t = apply_fo Cst.Int.divisible [] [int s; t]
  end

  module Rat = struct
    (* let mk = rat *)
    let minus t = apply_fo Cst.Rat.minus [] [t]
    let add a b = apply_fo Cst.Rat.add [] [a; b]
    let sub a b = apply_fo Cst.Rat.sub [] [a; b]
    let mul a b = apply_fo Cst.Rat.mul [] [a; b]
    let div a b = apply_fo Cst.Rat.div [] [a; b]
    let div_e a b = apply_fo Cst.Rat.div_e [] [a; b]
    let div_t a b = apply_fo Cst.Rat.div_t [] [a; b]
    let div_f a b = apply_fo Cst.Rat.div_f [] [a; b]
    let rem_e a b = apply_fo Cst.Rat.rem_e [] [a; b]
    let rem_t a b = apply_fo Cst.Rat.rem_t [] [a; b]
    let rem_f a b = apply_fo Cst.Rat.rem_f [] [a; b]
    let lt a b = apply_fo Cst.Rat.lt [] [a; b]
    let le a b = apply_fo Cst.Rat.le [] [a; b]
    let gt a b = apply_fo Cst.Rat.gt [] [a; b]
    let ge a b = apply_fo Cst.Rat.ge [] [a; b]
    let floor a = apply_fo Cst.Rat.floor [] [a]
    let ceiling a = apply_fo Cst.Rat.ceiling [] [a]
    let truncate a = apply_fo Cst.Rat.truncate [] [a]
    let round a = apply_fo Cst.Rat.round [] [a]
    let is_int a = apply_fo Cst.Rat.is_int [] [a]
    let is_rat a = apply_fo Cst.Rat.is_rat [] [a]
    let to_int t = coerce Ty.int t
    let to_rat t = coerce Ty.rat t
    let to_real t = coerce Ty.real t
  end

  module Real = struct
    let mk = real
    let minus t = apply_fo Cst.Real.minus [] [t]
    let add a b = apply_fo Cst.Real.add [] [a; b]
    let sub a b = apply_fo Cst.Real.sub [] [a; b]
    let mul a b = apply_fo Cst.Real.mul [] [a; b]
    let div a b = apply_fo Cst.Real.div [] [a; b]
    let div_e a b = apply_fo Cst.Real.div_e [] [a; b]
    let div_t a b = apply_fo Cst.Real.div_t [] [a; b]
    let div_f a b = apply_fo Cst.Real.div_f [] [a; b]
    let rem_e a b = apply_fo Cst.Real.rem_e [] [a; b]
    let rem_t a b = apply_fo Cst.Real.rem_t [] [a; b]
    let rem_f a b = apply_fo Cst.Real.rem_f [] [a; b]
    let lt a b = apply_fo Cst.Real.lt [] [a; b]
    let le a b = apply_fo Cst.Real.le [] [a; b]
    let gt a b = apply_fo Cst.Real.gt [] [a; b]
    let ge a b = apply_fo Cst.Real.ge [] [a; b]
    let floor a = apply_fo Cst.Real.floor [] [a]
    let ceiling a = apply_fo Cst.Real.ceiling [] [a]
    let truncate a = apply_fo Cst.Real.truncate [] [a]
    let round a = apply_fo Cst.Real.round [] [a]
    let is_int a = apply_fo Cst.Real.is_int [] [a]
    let is_rat a = apply_fo Cst.Real.is_rat [] [a]
    let to_int t = coerce Ty.int t
    let to_rat t = coerce Ty.rat t
    let to_real t = coerce Ty.real t
  end

  (* Arrays *)
  let match_array_type t =
    match View.Ty.ty (Core.ty t) with
    | `Array (src, dst) -> (src, dst)
    | _ ->
      let src = Var.mk "_" Core.type_ in
      let dst = Var.mk "_" Core.type_ in
      let pat = Ty.array (Ty.of_var src) (Ty.of_var dst) in
      raise (Wrong_type { term = t; expected_ty = pat; })

  let select t idx =
    let src, dst = match_array_type t in
    apply_fo Cst.select [src; dst] [t; idx]

  let store t idx value =
    let src, dst = match_array_type t in
    apply_fo Cst.store [src; dst] [t; idx; value]

  (* Bitvectors *)
  module Bitv = struct
    let match_bitv_type t =
      match View.Ty.ty (Core.ty t) with
      | `Bitv i -> i
      | _ -> raise (Wrong_type { term = t; expected_ty = Ty.bitv 0; })

    let mk s = apply_fo (Cst.Bitv.bitv s) [] []

    let concat u v =
      let i = match_bitv_type u in
      let j = match_bitv_type v in
      apply_fo (Cst.Bitv.concat (i, j)) [] [u; v]

    let extract i j t =
      let n = match_bitv_type t in
      (* TODO: check that i and j are correct index for a bitv(n) *)
      apply_fo (Cst.Bitv.extract (i, j, n)) [] [t]

    let repeat k t =
      let n = match_bitv_type t in
      apply_fo (Cst.Bitv.repeat (k, n)) [] [t]

    let zero_extend k t =
      let n = match_bitv_type t in
      apply_fo (Cst.Bitv.zero_extend (k, n)) [] [t]

    let sign_extend k t =
      let n = match_bitv_type t in
      apply_fo (Cst.Bitv.sign_extend (k, n)) [] [t]

    let rotate_right k t =
      let n = match_bitv_type t in
      apply_fo (Cst.Bitv.rotate_right (k, n)) [] [t]

    let rotate_left k t =
      let n = match_bitv_type t in
      apply_fo (Cst.Bitv.rotate_left (k, n)) [] [t]

    let not t =
      let n = match_bitv_type t in
      apply_fo (Cst.Bitv.not n) [] [t]

    let and_ u v =
      let n = match_bitv_type u in
      apply_fo (Cst.Bitv.and_ n) [] [u; v]

    let or_ u v =
      let n = match_bitv_type u in
      apply_fo (Cst.Bitv.or_ n) [] [u; v]

    let nand u v =
      let n = match_bitv_type u in
      apply_fo (Cst.Bitv.nand n) [] [u; v]

    let nor u v =
      let n = match_bitv_type u in
      apply_fo (Cst.Bitv.nor n) [] [u; v]

    let xor u v =
      let n = match_bitv_type u in
      apply_fo (Cst.Bitv.xor n) [] [u; v]

    let xnor u v =
      let n = match_bitv_type u in
      apply_fo (Cst.Bitv.xnor n) [] [u; v]

    let comp u v =
      let n = match_bitv_type u in
      apply_fo (Cst.Bitv.comp n) [] [u; v]

    let neg t =
      let n = match_bitv_type t in
      apply_fo (Cst.Bitv.neg n) [] [t]

    let add u v =
      let n = match_bitv_type u in
      apply_fo (Cst.Bitv.add n) [] [u; v]

    let sub u v =
      let n = match_bitv_type u in
      apply_fo (Cst.Bitv.sub n) [] [u; v]

    let mul u v =
      let n = match_bitv_type u in
      apply_fo (Cst.Bitv.mul n) [] [u; v]

    let udiv u v =
      let n = match_bitv_type u in
      apply_fo (Cst.Bitv.udiv n) [] [u; v]

    let urem u v =
      let n = match_bitv_type u in
      apply_fo (Cst.Bitv.urem n) [] [u; v]

    let sdiv u v =
      let n = match_bitv_type u in
      apply_fo (Cst.Bitv.sdiv n) [] [u; v]

    let srem u v =
      let n = match_bitv_type u in
      apply_fo (Cst.Bitv.srem n) [] [u; v]

    let smod u v =
      let n = match_bitv_type u in
      apply_fo (Cst.Bitv.smod n) [] [u; v]

    let shl u v =
      let n = match_bitv_type u in
      apply_fo (Cst.Bitv.shl n) [] [u; v]

    let lshr u v =
      let n = match_bitv_type u in
      apply_fo (Cst.Bitv.lshr n) [] [u; v]

    let ashr u v =
      let n = match_bitv_type u in
      apply_fo (Cst.Bitv.ashr n) [] [u; v]

    let ult u v =
      let n = match_bitv_type u in
      apply_fo (Cst.Bitv.ult n) [] [u; v]

    let ule u v =
      let n = match_bitv_type u in
      apply_fo (Cst.Bitv.ule n) [] [u; v]

    let ugt u v =
      let n = match_bitv_type u in
      apply_fo (Cst.Bitv.ugt n) [] [u; v]

    let uge u v =
      let n = match_bitv_type u in
      apply_fo (Cst.Bitv.uge n) [] [u; v]

    let slt u v =
      let n = match_bitv_type u in
      apply_fo (Cst.Bitv.slt n) [] [u; v]

    let sle u v =
      let n = match_bitv_type u in
      apply_fo (Cst.Bitv.sle n) [] [u; v]

    let sgt u v =
      let n = match_bitv_type u in
      apply_fo (Cst.Bitv.sgt n) [] [u; v]

    let sge u v =
      let n = match_bitv_type u in
      apply_fo (Cst.Bitv.sge n) [] [u; v]

  end

  module Float = struct
    (* Floats *)
    let match_float_type t =
      match View.Ty.ty (Core.ty t) with
      | `Float (e, s) -> (e, s)
      | _ -> raise (Wrong_type { term = t; expected_ty = Ty.float 0 0; })

    let fp sign exp significand =
      let e = Bitv.match_bitv_type exp in
      let s = Bitv.match_bitv_type significand in
      apply_fo (Cst.Float.fp (e, s+1)) [] [sign; exp; significand]

    let roundNearestTiesToEven = apply_fo Cst.Float.roundNearestTiesToEven [] []
    let roundNearestTiesToAway = apply_fo Cst.Float.roundNearestTiesToAway [] []
    let roundTowardPositive = apply_fo Cst.Float.roundTowardPositive [] []
    let roundTowardNegative = apply_fo Cst.Float.roundTowardNegative [] []
    let roundTowardZero = apply_fo Cst.Float.roundTowardZero [] []

    let plus_infinity e s = apply_fo (Cst.Float.plus_infinity (e,s)) [] []
    let minus_infinity e s = apply_fo (Cst.Float.minus_infinity (e,s)) [] []
    let plus_zero e s = apply_fo (Cst.Float.plus_zero (e,s)) [] []
    let minus_zero e s = apply_fo (Cst.Float.minus_zero (e,s)) [] []
    let nan e s = apply_fo (Cst.Float.nan (e,s)) [] []
    let abs x =
      let es = match_float_type x in
      apply_fo (Cst.Float.abs es) [] [x]
    let neg x =
      let es = match_float_type x in
      apply_fo (Cst.Float.neg es) [] [x]
    let add rm x y =
      let es = match_float_type x in
      apply_fo (Cst.Float.add es) [] [rm;x;y]
    let sub rm x y =
      let es = match_float_type x in
      apply_fo (Cst.Float.sub es) [] [rm;x;y]
    let mul rm x y =
      let es = match_float_type x in
      apply_fo (Cst.Float.mul es) [] [rm;x;y]
    let div rm x y =
      let es = match_float_type x in
      apply_fo (Cst.Float.div es) [] [rm;x;y]
    let fma rm x y z =
      let es = match_float_type x in
      apply_fo (Cst.Float.fma es) [] [rm;x;y;z]
    let sqrt rm x =
      let es = match_float_type x in
      apply_fo (Cst.Float.sqrt es) [] [rm;x]
    let rem x y =
      let es = match_float_type x in
      apply_fo (Cst.Float.rem es) [] [x;y]
    let roundToIntegral rm x =
      let es = match_float_type x in
      apply_fo (Cst.Float.roundToIntegral es) [] [rm;x]
    let min x y =
      let es = match_float_type x in
      apply_fo (Cst.Float.min es) [] [x;y]
    let max x y =
      let es = match_float_type x in
      apply_fo (Cst.Float.max es) [] [x;y]
    let leq x y =
      let es = match_float_type x in
      apply_fo (Cst.Float.leq es) [] [x;y]
    let lt x y =
      let es = match_float_type x in
      apply_fo (Cst.Float.lt es) [] [x;y]
    let geq x y =
      let es = match_float_type x in
      apply_fo (Cst.Float.geq es) [] [x;y]
    let gt x y =
      let es = match_float_type x in
      apply_fo (Cst.Float.gt es) [] [x;y]
    let eq x y =
      let es = match_float_type x in
      apply_fo (Cst.Float.eq es) [] [x;y]
    let isNormal x =
      let es = match_float_type x in
      apply_fo (Cst.Float.isNormal es) [] [x]
    let isSubnormal x =
      let es = match_float_type x in
      apply_fo (Cst.Float.isSubnormal es) [] [x]
    let isZero x =
      let es = match_float_type x in
      apply_fo (Cst.Float.isZero es) [] [x]
    let isInfinite x =
      let es = match_float_type x in
      apply_fo (Cst.Float.isInfinite es) [] [x]
    let isNaN x =
      let es = match_float_type x in
      apply_fo (Cst.Float.isNaN es) [] [x]
    let isNegative x =
      let es = match_float_type x in
      apply_fo (Cst.Float.isNegative es) [] [x]
    let isPositive x =
      let es = match_float_type x in
      apply_fo (Cst.Float.isPositive es) [] [x]
    let to_real x =
      let es = match_float_type x in
      apply_fo (Cst.Float.to_real es) [] [x]
    let ieee_format_to_fp e s bv =
      apply_fo (Cst.Float.ieee_format_to_fp (e,s)) [] [bv]
    let to_fp e2 s2 rm x =
      let (e1,s1) = match_float_type x in
      apply_fo (Cst.Float.to_fp (e1,s1,e2,s2)) [] [rm;x]
    let real_to_fp e s rm r =
      apply_fo (Cst.Float.real_to_fp (e,s)) [] [rm;r]
    let sbv_to_fp e s rm bv =
      let n = Bitv.match_bitv_type bv in
      apply_fo (Cst.Float.sbv_to_fp (n,e,s)) [] [rm;bv]
    let ubv_to_fp e s rm bv =
      let n = Bitv.match_bitv_type bv in
      apply_fo (Cst.Float.ubv_to_fp (n,e,s)) [] [rm;bv]
    let to_ubv m rm x =
      let (e,s) = match_float_type x in
      apply_fo (Cst.Float.to_ubv (e,s,m)) [] [rm;x]
    let to_sbv m rm x =
      let (e,s) = match_float_type x in
      apply_fo (Cst.Float.to_sbv (e,s,m)) [] [rm;x]
  end

  module String = struct

    let of_ustring s = apply_fo (Cst.String.string s) [] []
    let length s = apply_fo Cst.String.length [] [s]
    let at s i = apply_fo Cst.String.at [] [s; i]
    let is_digit s = apply_fo Cst.String.is_digit [] [s]
    let to_code s = apply_fo Cst.String.to_code [] [s]
    let of_code i = apply_fo Cst.String.of_code [] [i]
    let to_int s = apply_fo Cst.String.to_int [] [s]
    let of_int i = apply_fo Cst.String.of_int [] [i]
    let concat s s' = apply_fo Cst.String.concat [] [s;s']
    let sub s i n = apply_fo Cst.String.sub [] [s; i; n]
    let index_of s s' i = apply_fo Cst.String.index_of [] [s; s'; i]
    let replace s pat by = apply_fo Cst.String.replace [] [s; pat; by]
    let replace_all s pat by = apply_fo Cst.String.replace_all [] [s; pat; by]
    let replace_re s pat by = apply_fo Cst.String.replace_re [] [s; pat; by]
    let replace_re_all s pat by = apply_fo Cst.String.replace_re_all [] [s; pat; by]
    let is_prefix s s' = apply_fo Cst.String.is_prefix [] [s; s']
    let is_suffix s s' = apply_fo Cst.String.is_suffix [] [s; s']
    let contains s s' = apply_fo Cst.String.contains [] [s; s']
    let lt s s' = apply_fo Cst.String.lt [] [s; s']
    let leq s s' = apply_fo Cst.String.leq [] [s; s']
    let in_re s re = apply_fo Cst.String.in_re [] [s; re]

    module RegLan = struct
      let empty = apply_fo Cst.String.Reg_Lang.empty [] []
      let all = apply_fo Cst.String.Reg_Lang.all [] []
      let allchar = apply_fo Cst.String.Reg_Lang.allchar [] []
      let of_string s = apply_fo Cst.String.Reg_Lang.of_string [] [s]
      let range s s' = apply_fo Cst.String.Reg_Lang.range [] [s; s']
      let concat re re' = apply_fo Cst.String.Reg_Lang.concat [] [re; re']
      let union re re' = apply_fo Cst.String.Reg_Lang.union [] [re; re']
      let inter re re' = apply_fo Cst.String.Reg_Lang.inter [] [re; re']
      let diff re re' = apply_fo Cst.String.Reg_Lang.diff [] [re; re']
      let star re = apply_fo Cst.String.Reg_Lang.star [] [re]
      let cross re = apply_fo Cst.String.Reg_Lang.cross [] [re]
      let complement re = apply_fo Cst.String.Reg_Lang.complement [] [re]
      let option re = apply_fo Cst.String.Reg_Lang.option [] [re]
      let power n re = apply_fo (Cst.String.Reg_Lang.power n) [] [re]
      let loop n1 n2 re = apply_fo (Cst.String.Reg_Lang.loop (n1, n2)) [] [re]
    end

  end

end

