
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Type definitions *)
(* ************************************************************************* *)

(* private aliases *)
type hash = int
type index = int
type 'a tag = 'a Tag.t

(* Extensible variant type for builtin operations *)
type builtin = <
  ty : ty;
  ty_var : ty_var;
  ty_cst : ty_cst;
  term : term;
  term_var : term_var;
  term_cst : term_cst;
> Builtin.t

(* Identifiers *)
and 'ty id = {
  id_ty         : 'ty;
  index         : index; (** unique index *)
  path          : Path.t;
  builtin       : builtin;
  mutable tags  : Tag.map;
}

and type_ = Type

(* Type for first order types *)
and type_fun = {
  arity : int;
  mutable alias : type_alias;
}

and type_alias =
  | No_alias
  | Alias of {
      alias_vars : ty_var list;
      alias_body : ty;
    }

(* Representation of polymorphic types
   Rank-1 polymorphism is enforced at runtime *)
and ty_var = type_ id

and ty_cst = type_fun id

and ty_descr =
  | TyVar of ty_var
  | TyApp of ty_cst * ty list
  | Arrow of ty list * ty
  | Pi of ty_var list * ty

and ty = {
  mutable ty_hash : hash; (* lazy hash *)
  mutable ty_tags : Tag.map;
  mutable ty_descr : ty_descr;
  mutable ty_head : ty;
}

(* Terms and formulas *)
and term_var = ty id

and term_cst = ty id

and pattern = term

and term_descr =
  | Var of term_var
  | Cst of term_cst
  | App of term * ty list * term list
  | Binder of binder * term
  | Match of term * (pattern * term) list

and binder =
  | Let_seq  of (term_var * term) list
  | Let_par of (term_var * term) list
  | Lambda of ty_var list * term_var list
  | Exists of ty_var list * term_var list
  | Forall of ty_var list * term_var list

and term = {
  term_ty : ty;
  term_descr : term_descr;
  mutable term_hash : hash;
  mutable term_tags : Tag.map;
}

(* Alias for dolmen_loop and others who allow to have different types
   for terms and formulas. *)
and formula = term

(* Type definitions *)
type ty_def_adt_case = {
  cstr : term_cst;
  tester : term_cst;
  dstrs : term_cst option array;
}

type ty_def =
  | Abstract
  | Adt of {
      ty : ty_cst;
      record : bool;
      cases : ty_def_adt_case array;
    }


(* Exceptions *)
(* ************************************************************************* *)

exception Already_aliased of ty_cst
exception Type_already_defined of ty_cst
exception Record_type_expected of ty_cst
exception Wildcard_already_set of ty_var


(* Tags *)
(* ************************************************************************* *)

module Tags = struct

  type 'a t = 'a tag
  type pos = Pretty.pos
  type name = Pretty.name

  let pos = Tag.create ()
  let name = Tag.create ()
  let rwrt = Tag.create ()
  let ac = Tag.create ()
  let predicate = Tag.create ()

  let exact s = Pretty.Exact s
  let infix = Pretty.Infix
  let prefix = Pretty.Prefix

  let named = Tag.create ()
  let triggers = Tag.create ()
  let filters = Tag.create ()

  let bound = Tag.create ()

end


(* Printing *)
(* ************************************************************************* *)

module Print = struct

  type 'a t = Format.formatter -> 'a -> unit

  let print_index = ref false

  let pos : Pretty.pos tag = Tags.pos
  let name : Pretty.name tag = Tags.name

  let return fmt_str out () = Format.fprintf out "%(%)" fmt_str


  (* Id printing *)

  let pp_index fmt (v : _ id) =
    let aux fmt v = Format.fprintf fmt "/%d" v.index in
    Fmt.styled (`Fg (`Hi `Black)) aux fmt v

  let id fmt (v : _ id) =
    match Tag.get v.tags name with
    | Some (Pretty.Exact s | Pretty.Renamed s) ->
      Format.fprintf fmt "%s" s
    | None ->
      if !print_index then
        Format.fprintf fmt "%a%a" Path.print v.path pp_index v
      else
        Format.fprintf fmt "%a" Path.print v.path

  let id_pretty fmt (v : _ id) =
    match Tag.get v.tags pos with
    | None -> ()
    | Some Pretty.Infix -> Format.fprintf fmt "(%a)" id v
    | Some Pretty.Prefix -> Format.fprintf fmt "[%a]" id v

  let id_type print fmt (v : _ id) =
    Format.fprintf fmt "@[<hov 2>%a%a :@ %a@]" id v id_pretty v print v.id_ty


  (* Type printing *)

  let type_ fmt Type =
    Format.fprintf fmt "Type"

  let type_fun fmt { arity; alias = _; } =
    let rec aux fmt = function
      | 0 -> type_ fmt Type
      | n -> Format.fprintf fmt "%a ->@ %a" type_ Type aux (n - 1)
    in
    aux fmt arity

  let ty_var fmt var = id_type type_ fmt var
  let ty_cst fmt cst = id_type type_fun fmt cst

  let rec ty_descr fmt (descr : ty_descr) =
    match descr with
    | TyVar v -> id fmt v
    | Arrow (args, ret) ->
      Format.fprintf fmt "@[<hov 2>%a ->@ %a@]"
        (Format.pp_print_list ~pp_sep:(return " ->@ ") ty) args subty ret
    | TyApp (f, []) -> id fmt f
    | TyApp (f, l) ->
      begin match Tag.get f.tags pos with
        | Some Pretty.Prefix ->
          Format.fprintf fmt "@[<hov 2>%a %a@]"
            id f (Format.pp_print_list ~pp_sep:(return "") subty) l
        | Some Pretty.Infix when List.length l >= 2 ->
          let pp_sep fmt () = Format.fprintf fmt " %a@ " id f in
          Format.fprintf fmt "@[<hov 2>%a@]" (Format.pp_print_list ~pp_sep subty) l
        | None | Some Pretty.Infix ->
          Format.fprintf fmt "@[<hov 2>%a(%a)@]"
            id f (Format.pp_print_list ~pp_sep:(return ",@ ") subty) l
      end
    | Pi ([], body) -> ty fmt body
    | Pi (poly_vars, body) ->
      Format.fprintf fmt "@[<hov 2>∀ @[<hov>%a@] .@ %a@]"
        (Format.pp_print_list ~pp_sep:(return ",@ ") ty_var) poly_vars ty body

  and subty fmt t =
    match t.ty_descr with
    | TyVar _
    | TyApp (_, []) -> ty fmt t
    | _ -> Format.fprintf fmt "( %a )" ty t

  and ty fmt t =
    ty_descr fmt t.ty_descr

  let term_var fmt var = id_type ty fmt var
  let term_cst fmt cst = id_type ty fmt cst

  (* Term printing *)

  let rec term_descr fmt = function
    | Var v -> id fmt v
    | Cst c -> id fmt c
    | App (f, [], []) -> term fmt f
    | App (f, tys, args) -> term_app fmt f tys args
    | Binder (b, body) ->
      Format.fprintf fmt "@[<hv 2>%a%a@ %a@]" binder b binder_sep b term body
    | Match (scrutinee, branches) ->
      Format.fprintf fmt "@[<hv 2>match %a with@ %a@]"
        term scrutinee
        (Format.pp_print_list ~pp_sep:(return "@ ") branch) branches

  and term_app fmt f tys args =
    match f.term_descr with
    | Cst g ->
      begin match Tag.get g.tags pos with
        | Some Pretty.Prefix -> generic_app fmt f [] args
        | Some Pretty.Infix when List.length args >= 2 ->
          let pp_sep fmt () = Format.fprintf fmt " %a@ " id g in
          Format.fprintf fmt "@[<hov>%a@]" (Format.pp_print_list ~pp_sep subterm) args
        | None | Some Pretty.Infix -> generic_app fmt f tys args
      end
    | _ -> generic_app fmt f tys args

  and generic_app fmt f tys args =
    match tys, args with
    | _, [] ->
      Format.fprintf fmt "@[<hov>%a@ %a@]"
        subterm f (Format.pp_print_list ~pp_sep:(return "@ ") ty) tys
    | [], _ ->
      Format.fprintf fmt "@[<hov>%a@ %a@]"
        subterm f (Format.pp_print_list ~pp_sep:(return "@ ") subterm) args
    | _ ->
      Format.fprintf fmt "@[<hov>%a@ %a@ %a@]" subterm f
        (Format.pp_print_list ~pp_sep:(return "@ ") subty) tys
        (Format.pp_print_list ~pp_sep:(return "@ ") subterm) args

  and binder_sep fmt = function
    | Lambda _ -> Format.fprintf fmt "=>"
    | Let_seq _
    | Let_par _ -> Format.fprintf fmt "in"
    | Exists _
    | Forall _ -> Format.fprintf fmt "."

  and binder fmt b =
    match b with
    | Let_seq l ->
      Format.fprintf fmt "let @[<hv>%a@]"
        (Format.pp_print_list ~pp_sep:(return ",@ ") binding) l
    | Let_par l ->
      Format.fprintf fmt "let @[<hv>%a@]"
        (Format.pp_print_list ~pp_sep:(return " and@ ") binding) l

    | Lambda (vars, args) ->
      Format.fprintf fmt "λ @[<hov>%a .@ %a@]"
        (Format.pp_print_list ~pp_sep:(return ",@ ") ty_var) vars
        (Format.pp_print_list ~pp_sep:(return "@ ") term_var) args

    | Exists (l, []) ->
      Format.fprintf fmt "∃ @[<hov>%a@]"
        (Format.pp_print_list ~pp_sep:(return ",@ ") ty_var) l
    | Exists ([], l) ->
      Format.fprintf fmt "∃ @[<hov>%a@]"
        (Format.pp_print_list ~pp_sep:(return ",@ ") term_var) l
    | Exists (tys, ts) ->
      Format.fprintf fmt "∃ @[<hov>%a,@ %a@]"
        (Format.pp_print_list ~pp_sep:(return ",@ ") ty_var) tys
        (Format.pp_print_list ~pp_sep:(return ",@ ") term_var) ts

    | Forall (l, []) ->
      Format.fprintf fmt "∀ @[<hov>%a@]"
        (Format.pp_print_list ~pp_sep:(return ",@ ") ty_var) l
    | Forall ([], l) ->
      Format.fprintf fmt "∀ @[<hov>%a@]"
        (Format.pp_print_list ~pp_sep:(return ",@ ") term_var) l
    | Forall (tys, ts) ->
      Format.fprintf fmt "∀ @[<hov>%a,@ %a@]"
        (Format.pp_print_list ~pp_sep:(return ",@ ") ty_var) tys
        (Format.pp_print_list ~pp_sep:(return ",@ ") term_var) ts

  and binding fmt (v, t) =
    Format.fprintf fmt "@[<hov 2>%a =@ %a@]" id v term t

  and branch fmt (pattern, body) =
    Format.fprintf fmt "@[<hov 2>| %a@ ->@ %a" term pattern term body

  and subterm fmt t =
    match t.term_descr with
    | Var _ | Cst _ -> term fmt t
    | App (t', [], []) -> subterm fmt t'
    | _ -> Format.fprintf fmt "(%a)" term t

  and term fmt t =
    term_descr fmt t.term_descr

  let formula = term


  (* Type def printing *)

  let ty_def_adt_case fmt { cstr; tester; dstrs; } =
    Format.fprintf fmt
      "| @[<v>%a@ %a@ %a@]"
      term_cst cstr term_cst tester
      Fmt.(array (option term_cst)) dstrs

  let ty_def fmt = function
    | Abstract -> Format.fprintf fmt "abstract"
    | Adt { ty = _; record = _; cases; } ->
      Format.fprintf fmt "@[<v>%a@]"
        Fmt.(array ty_def_adt_case) cases

  (* Misc *)
  let iter ~sep pp fmt k =
    let first = ref true in
    k (fun x ->
        if !first then first := false else sep fmt ();
        pp fmt x
      )
end


(* Helpers *)
(* ************************************************************************* *)

(* Useful shorthand for chaining comparisons *)
let (<?>) = Misc.(<?>)
let lexicographic = Misc.lexicographic

(* option iter *)
let option_map f = function
  | None -> None
  | Some x -> Some (f x)

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

(* list split *)
let list_take l n =
  let rec aux acc l n =
    match l with
    | _ when n <= 0 -> List.rev acc, l
    | [] -> failwith "Expr.list_take: not enough arguments"
    | x :: r -> aux (x :: acc) r (n - 1)
  in
  aux [] l n

(* List.concat_map *)
let list_concat_map f l =
  let rec aux acc f = function
    | [] -> List.rev acc
    | x :: r ->
      let l = f x in
      aux (List.rev_append l acc) f r
  in
  aux [] f l

(* List.find_map *)
let rec list_find_map f = function
  | [] -> None
  | x :: r ->
    begin match f x with
      | (Some _) as res -> res
      | None -> list_find_map f r
    end

(* automatic cache *)
let with_cache ~cache f x =
  match Hashtbl.find cache x with
  | res -> res
  | exception Not_found ->
    let res = f x in
    Hashtbl.add cache x res;
    res


(* Ids *)
(* ************************************************************************* *)

module Id = struct

  type 'a t = 'a id

  let print = Print.id

  (* Usual functions *)
  let hash (v : _ t) = v.index

  let compare v v' = compare v.index v'.index

  let equal v v' = compare v v' = 0

  (* Tags *)
  let get_tag (id : _ id) k = Tag.get id.tags k
  let get_tag_last (id : _ id) k = Tag.get_last id.tags k
  let get_tag_list (id : _ id) k = Tag.get_list id.tags k

  let set_tag (id : _ id) k v = id.tags <- Tag.set id.tags k v
  let add_tag (id : _ id) k v = id.tags <- Tag.add id.tags k v
  let add_tag_opt (id : _ id) k o = id.tags <- Tag.add_opt id.tags k o
  let add_tag_list (id : _ id) k l = id.tags <- Tag.add_list id.tags k l

  let unset_tag (id : _ id) k = id.tags <- Tag.unset id.tags k

  (* Creating ids *)
  let id_counter = ref 0

  let mk
      ?pos ?name
      ?(tags=Tag.empty)
      ?(builtin=Builtin.Base)
      id_path id_ty =
    incr id_counter;
    let tags = Tag.set_opt tags Print.pos pos in
    let tags = Tag.set_opt tags Print.name
        (option_map (fun s -> Pretty.Exact s) name)
    in
    { path = id_path; id_ty; builtin; tags; index = !id_counter; }

end

(* Maps from integers *)
(* ************************************************************************* *)

module M = Map.Make(Int)


(* Sets of variables *)
(* ************************************************************************* *)

module FV = struct

  type elt =
    | Ty of ty_var
    | Term of term_var

  type t = elt M.t

  let tok v = v.index
  let token = function
    | Ty v -> tok v
    | Term v -> tok v

  (* let mem v s = M.mem (tok v) s *)
  (* let get v s = M.find (tok v) s *)
  let add x (s : t) = M.add (token x) x s
  let del x (s : t) = M.remove (tok x) s

  let empty = M.empty

  let remove s l =
    List.fold_left (fun acc v -> del v acc) s l

  let diff s s' =
    M.merge (fun _ o o' ->
        match o, o' with
        | None, None -> None
        | None, Some _ -> None
        | (Some _ as res), None -> res
        | Some _, Some _ -> None
      ) s s'

  let merge s s' =
    M.merge (fun _ o o' ->
        match o, o' with
        | None, None -> None
        | _, ((Some _) as res)
        | ((Some _) as res), _ -> res
      ) s s'

  let to_list s =
    let aux _ elt (tys, ts) = match elt with
      | Ty v -> (v :: tys, ts)
      | Term v -> (tys, v :: ts)
    in
    M.fold aux s ([], [])

end

(* Substitutions *)
(* ************************************************************************* *)

module Subst = struct

  type ('a, 'b) t = ('a * 'b) M.t

  (* Usual functions *)
  let empty = M.empty

  let is_empty = M.is_empty

  let wrap key = function
    | None -> None
    | Some x -> Some (key, x)

  let merge f = M.merge (fun _ opt1 opt2 ->
      match opt1, opt2 with
      | None, None -> assert false
      | Some (key, value), None ->
        wrap key @@ f key (Some value) None
      | None, Some (key, value) ->
        wrap key @@ f key None (Some value)
      | Some (key, value1), Some (_key, value2) ->
        wrap key @@ f key (Some value1) (Some value2)
    )

  let iter f = M.iter (fun _ (key, value) -> f key value)

  let map f = M.map (fun (key, value) -> (key, f value))

  let fold f = M.fold (fun _ (key, value) acc -> f key value acc)

  let bindings s = M.fold (fun _ (key, value) acc -> (key, value) :: acc) s []

  let filter p = M.filter (fun _ (key, value) -> p key value)

  (* Comparisons *)
  let equal f = M.equal (fun (_, value1) (_, value2) -> f value1 value2)
  let compare f = M.compare (fun (_, value1) (_, value2) -> f value1 value2)
  let hash h s = M.fold (fun i (_, value) acc -> Hashtbl.hash (acc, i, h value)) s 1

  let choose m = snd (M.choose m)

  (* Iterators *)
  let exists pred s =
    try
      iter (fun m s -> if pred m s then raise Exit) s;
      false
    with Exit ->
      true

  let for_all pred s =
    try
      iter (fun m s -> if not (pred m s) then raise Exit) s;
      true
    with Exit ->
      false

  let print print_key print_value fmt map =
    let aux fmt (_, (key, value)) =
      Format.fprintf fmt "@[<hov 2>%a ↦@ %a@]" print_key key print_value value
    in
    Format.fprintf fmt "@[<hv>%a@]"
      Print.(iter ~sep:(return ";@ ") aux) (fun k -> M.iter (fun x y -> k(x,y)) map)

  let debug print_key print_value fmt map =
    let aux fmt (i, (key, value)) =
      Format.fprintf fmt "@[<hov 2>%d: %a ↦@ %a@]"
        i print_key key print_value value
    in
    Format.fprintf fmt "@[<hv>%a@]"
      Print.(iter ~sep:(return ";@ ") aux) (fun k -> M.iter (fun x y -> k(x,y)) map)

  (* Specific substitutions signature *)
  module type S = sig
    type 'a key
    val get : 'a key -> ('a key, 'b) t -> 'b
    val mem : 'a key -> ('a key, 'b) t -> bool
    val bind : ('a key, 'b) t -> 'a key -> 'b -> ('a key, 'b) t
    val remove : 'a key -> ('a key, 'b) t -> ('a key, 'b) t
  end

  (* Variable substitutions *)
  module Var = struct
    type 'a key = 'a id
    let tok v = v.index
    let get v s = snd (M.find (tok v) s)
    let mem v s = M.mem (tok v) s
    let bind s v t = M.add (tok v) (v, t) s
    let remove v s = M.remove (tok v) s
  end
end


(* Types *)
(* ************************************************************************* *)

module Ty = struct

  (* Std type aliases *)

  type t = ty

  type subst = (ty_var, ty) Subst.t

  type 'a tag = 'a Tag.t

  exception Bad_arity of ty_cst * ty list
  exception Prenex_polymorphism of ty

  (* printing *)
  let print = Print.ty

  (* Set a wildcard/hole to a concrete type

     Wildcard can be set to point to another type by mutating the
     descr field of types (this is the only operation that mutates
     this field).
     In order to be correct, when we set a wildcard v to point at
     another wildcard w, we must remember that, so that when we set
     w to something, we also need to update v. *)

  let wildcard_refs = Tag.create ()
  let wildcard_hook = Tag.create ()

  let wildcard_add_refs v l =
    Id.add_tag_list v wildcard_refs l

  let set_wildcard v (t: t) =
    match v.builtin with
    | Builtin.Wildcard { ty; } ->
      begin match !ty with
        | Some _ ->
          raise (Wildcard_already_set v)
        | None ->
          let set_descr (t : t) (s: t) =
            s.ty_descr <- t.ty_descr;
            s.ty_hash <- -1
          in
          ty := Some t;
          let l = Id.get_tag_list v wildcard_refs in
          List.iter (set_descr t) l;
          begin match t.ty_descr with
            | TyVar ({ builtin = Builtin.Wildcard _; _ } as w) ->
              wildcard_add_refs w l
            | _ -> ()
          end;
          List.iter (fun f -> f v t)
            (Id.get_tag_list v wildcard_hook)
      end
    | _ -> ()

  let wildcard_var () =
    let path = Path.local "_" in
    Id.mk ~builtin:(Builtin.Wildcard { ty = ref None; }) path Type


  (* Prenex/Rank-1 polymorphism check *)
  let rec check_prenex t =
    match (expand_head t).ty_descr with
    | Pi _ -> raise (Prenex_polymorphism t)
    | _ -> ()

  and check_prenex_list l =
    List.iter check_prenex l

  and subst_bind subst v u =
    check_prenex u;
    Subst.Var.bind subst v u

  (* type creation *)
  and dummy = {
    ty_hash = 0; (* must be non-negative *)
    ty_head = dummy;
    ty_tags = Tag.empty;
    ty_descr = Arrow ([], dummy);
  }

  and mk ty_descr = {
    ty_descr;
    ty_hash = -1;
    ty_tags = Tag.empty;
    ty_head = dummy;
  }

  and of_var v =
    match v with
    | { builtin = Builtin.Wildcard { ty; }; _ } ->
      begin match !ty with
        | None ->
          let t = mk (TyVar v) in
          wildcard_add_refs v [t];
          t
        | Some t -> t
      end
    | _ -> mk (TyVar v)

  and apply f args =
    if List.length args <> f.id_ty.arity then
      raise (Bad_arity (f, args))
    else begin
      check_prenex_list args;
      mk (TyApp (f, args))
    end

  and arrow args ret =
    check_prenex_list (ret :: args);
    match args with
    | [] -> ret
    | _ -> mk (Arrow (args, ret))

  and pi vars body =
    check_prenex body;
    match vars with
    | [] -> body
    | _ -> mk (Pi (vars, body))


  (* Substitutions *)
  and subst_aux ~fix var_map (t : t) =
    match t.ty_descr with
    | TyVar v ->
      begin match Subst.Var.get v var_map with
        | exception Not_found -> t
        | ty -> if fix then subst_aux ~fix var_map ty else ty
      end
    | TyApp (f, args) ->
      let new_args = List.map (subst_aux ~fix var_map) args in
      if List.for_all2 (==) args new_args then t
      else apply f new_args
    | Arrow (args, ret) ->
      let new_args = List.map (subst_aux ~fix var_map) args in
      let new_ret = subst_aux ~fix var_map ret in
      if ret == new_ret && List.for_all2 (==) args new_args then t
      else arrow new_args new_ret
    | Pi (vars, body) ->
      let var_map =
        List.fold_left (fun map v ->
            Subst.Var.remove v map
          ) var_map vars
      in
      let new_body = subst_aux ~fix var_map body in
      if body == new_body then t
      else pi vars new_body

  and subst ?(fix=true) var_map t =
    if Subst.is_empty var_map then t
    else subst_aux ~fix var_map t

  (* type aliases *)

  and alias_to c alias_vars alias_body =
    let cst_ty = c.id_ty in
    match cst_ty.alias with
    | No_alias -> cst_ty.alias <- Alias { alias_vars; alias_body; }
    | Alias _ -> raise (Already_aliased c)

  and expand_head t =
    if t.ty_head != dummy then t.ty_head
    else match t.ty_descr with
      | TyApp (f, args) ->
        begin match f.id_ty.alias with
          | No_alias -> t.ty_head <- t; t
          | Alias { alias_vars; alias_body; } ->
            assert (List.compare_lengths alias_vars args = 0);
            let map = List.fold_left2 Subst.Var.bind Subst.empty alias_vars args in
            let res = expand_head (subst map alias_body) in
            t.ty_head <- res;
            res
        end
      | _ -> t.ty_head <- t; t

  (* hash function *)
  let rec hash_aux (t : t) =
    match t.ty_descr with
    | TyVar v ->
      Misc.hash2 3 (Id.hash v)
    | TyApp (f, args) ->
      Misc.hash3 5 (Id.hash f) (Misc.hash_list hash args)
    | Arrow (args, ret) ->
      Misc.hash3 7 (Misc.hash_list hash args) (hash ret)
    | Pi (vars, body) ->
      Misc.hash3 11 (Misc.hash_list Id.hash vars) (hash body)

  and hash (t : t) =
    if t.ty_hash >= 0 then t.ty_hash
    else begin
      let t' = expand_head t in
      let res = hash_aux t' in
      t'.ty_hash <- res;
      t.ty_hash <- res;
      res
    end

  (* comparison *)
  let discr (t: t) =
    match (expand_head t).ty_descr with
    | TyVar _ -> 1
    | TyApp _ -> 2
    | Arrow _ -> 3
    | Pi _ -> 4

  let rec compare (u : t) (v : t) =
    if u == v || u.ty_descr == v.ty_descr then 0 else begin
      let hu = hash u and hv = hash v in
      if hu <> hv then hu - hv (* safe since both are positive *)
      else match (expand_head u).ty_descr, (expand_head v).ty_descr with
        | TyVar v, TyVar v' -> Id.compare v v'
        | TyApp (f, args), TyApp (f', args') ->
          Id.compare f f'
          <?> (lexicographic compare, args, args')
        | Arrow (args, ret), Arrow (args', ret') ->
          lexicographic compare args args'
          <?> (compare, ret, ret')
        | Pi (vars, body), Pi (vars', body') ->
          List.compare_lengths vars vars'
          <?> (compare_bound, (vars, body), (vars', body'))
        | _, _ -> Stdlib.compare (discr u) (discr v)
    end

  and compare_bound (vars, body) (vars', body') =
    (* Since we only have prenex/rank-1 polymorphism, this can only happen
       once by comparison. *)
    let map =
      List.fold_left2 Subst.Var.bind Subst.empty
        vars (List.map of_var vars')
    in
    let body = subst ~fix:false map body in
    compare body body'

  let equal u v = compare u v = 0

  (* Types definitions *)

  type def = ty_def

  let definition_tag : def Tag.t = Tag.create ()

  let definition c = Id.get_tag c definition_tag

  let is_record c =
    match definition c with
    | Some Adt { record; _ } -> record
    | _ -> false

  let define c d =
    match definition c with
    | None -> Id.set_tag c definition_tag d
    | Some _ -> raise (Type_already_defined c)

  (* Tags *)
  let get_tag (t : t) k = Tag.get t.ty_tags k
  let get_tag_last (t : t) k = Tag.get_last t.ty_tags k
  let get_tag_list (t : t) k = Tag.get_list t.ty_tags k

  let set_tag (t : t) k l = t.ty_tags <- Tag.set t.ty_tags k l
  let add_tag (t : t) k v = t.ty_tags <- Tag.add t.ty_tags k v
  let add_tag_opt (t : t) k o = t.ty_tags <- Tag.add_opt t.ty_tags k o
  let add_tag_list (t : t) k l = t.ty_tags <- Tag.add_list t.ty_tags k l

  let unset_tag (t: t) k = t.ty_tags <- Tag.unset t.ty_tags k

  (* Module for namespacing *)
  module Var = struct
    type t = ty_var
    let hash = Id.hash
    let print = Id.print
    let equal = Id.equal
    let compare = Id.compare
    let get_tag = Id.get_tag
    let get_tag_last = Id.get_tag_last
    let get_tag_list = Id.get_tag_list
    let set_tag = Id.set_tag
    let add_tag = Id.add_tag
    let add_tag_opt = Id.add_tag_opt
    let add_tag_list = Id.add_tag_list
    let unset_tag = Id.unset_tag

    let mk name = Id.mk (Path.local name) Type
    let wildcard () = wildcard_var ()
    let is_wildcard = function
      | { builtin = Builtin.Wildcard _; _ } -> true
      | _ -> false
  end

  let add_wildcard_hook ~hook v =
    if Var.is_wildcard v then Var.add_tag v wildcard_hook hook

  module Const = struct
    type t = ty_cst
    let hash = Id.hash
    let print = Id.print
    let equal = Id.equal
    let compare = Id.compare
    let get_tag = Id.get_tag
    let get_tag_last = Id.get_tag_last
    let get_tag_list = Id.get_tag_list
    let set_tag = Id.set_tag
    let add_tag = Id.add_tag
    let add_tag_opt = Id.add_tag_opt
    let add_tag_list = Id.add_tag_list
    let unset_tag = Id.unset_tag

    let arity (c : t) = c.id_ty.arity
    let mk path n =
      Id.mk path { arity = n; alias = No_alias; }
    let mk' ~builtin name n =
      Id.mk ~builtin (Path.global name) { arity = n; alias = No_alias; }

    let prop = mk' ~builtin:Builtin.Prop "Prop" 0
    let unit = mk' ~builtin:Builtin.Unit "unit" 0
    let base = mk' ~builtin:Builtin.Univ "$i" 0
    let int =  mk' ~builtin:Builtin.Int "int" 0
    let rat =  mk' ~builtin:Builtin.Rat "rat" 0
    let real = mk' ~builtin:Builtin.Real "real" 0
    let string = mk' ~builtin:Builtin.String "string" 0
    let string_reg_lang = mk' ~builtin:Builtin.String_RegLan "string_reglang" 0
    let array = mk' ~builtin:Builtin.Array "array" 2
    let bitv =
      with_cache ~cache:(Hashtbl.create 13) (fun i ->
          mk' ~builtin:(Builtin.Bitv i) (Format.asprintf "Bitv_%d" i) 0
        )
    let float =
      with_cache ~cache:(Hashtbl.create 13) (fun (e,s) ->
          mk' ~builtin:(Builtin.Float(e,s)) (Format.asprintf "FloatingPoint_%d_%d" e s) 0
        )
    let roundingMode = mk' ~builtin:Builtin.RoundingMode "RoundingMode" 0
  end

  (* Builtin types *)
  let prop = apply Const.prop []
  let unit = apply Const.unit []
  let base = apply Const.base []
  let int = apply Const.int []
  let rat = apply Const.rat []
  let real = apply Const.real []
  let string = apply Const.string []
  let string_reg_lang = apply Const.string_reg_lang []
  let array src dst = apply Const.array [src; dst]
  let bitv i = apply (Const.bitv i) []
  let float' es = apply (Const.float es) []
  let float e s = float' (e,s)
  let roundingMode = apply Const.roundingMode []

  (* alias for alt-ergo *)
  let bool = prop

  (* *)
  let split_pi t =
    let rec aux acc ty =
      let ty' = expand_head ty in
      match ty'.ty_descr with
      | Pi (vars, body) -> aux (vars :: acc) body
      | _ ->
        let vars = List.concat (List.rev acc) in
        vars, ty'
    in
    aux [] t

  let split_arrow t =
    let rec aux acc t =
      let t' = expand_head t in
      match t'.ty_descr with
      | Arrow (args, ret) -> aux (args :: acc) ret
      | TyVar _ | TyApp _ ->
        let args = List.concat (List.rev acc) in
        args, t'
      | Pi _ -> raise (Prenex_polymorphism t)
    in
    aux [] t

  let poly_sig t =
    let vars, t = split_pi t in
    let args, ret = split_arrow t in
    vars, args, ret

  let pi_arity t =
    let l, _ = split_pi t in
    List.length l

  let t_arity t =
    let _, l, _ = poly_sig t in
    List.length l

  (* Matching *)
  exception Impossible_matching of ty * ty

  let rec pmatch subst (pat : ty) (t : ty) =
    match (expand_head pat), (expand_head t) with
    | { ty_descr = TyVar v; _ }, _ ->
      begin match Subst.Var.get v subst with
        | t' ->
          if equal t t' then subst
          else raise (Impossible_matching (pat, t))
        | exception Not_found ->
          subst_bind subst v t
      end
    | { ty_descr = TyApp (f, f_args); _ },
      { ty_descr = TyApp (g, g_args); _ } ->
      if Id.equal f g then
        List.fold_left2 pmatch subst f_args g_args
      else
        raise (Impossible_matching (pat, t))
    | { ty_descr = Arrow (pat_args, pat_ret); _ },
      { ty_descr = Arrow (t_args, t_ret); _ } ->
      pmatch (pmatch_list pat t subst pat_args t_args) pat_ret t_ret
    | _ -> raise (Impossible_matching (pat, t))

  and pmatch_list pat t subst pat_args t_args =
    match pat_args, t_args with
    | [], [] -> subst
    | x :: l, y :: r -> pmatch_list pat t (pmatch subst x y) l r
    | [], _ :: _
    | _ :: _, [] -> raise (Impossible_matching (pat, t))

  let match_ pats tys =
    let rec aux subst pats tys =
      match pats, tys with
      | [], [] -> subst
      | pat :: pats, ty :: tys -> aux (pmatch subst pat ty) pats tys
      | [], _ :: _
      | _ :: _, [] -> raise Exit
    in
    try
      let res = aux Subst.empty pats tys in
      Subst.iter (fun var _ ->
          match var.builtin with
          | Builtin.Wildcard _ -> assert false
          | _ -> ()
        ) res;
      Some res
    with Exit | Impossible_matching _ ->
      None

  let instance_of poly ty =
    let vars, pat = split_pi poly in
    match match_ [pat] [ty] with
    | None -> None
    | Some subst ->
      let l = List.map (fun v -> Subst.Var.get v subst) vars in
      Some l

  (* Unification *)
  exception Impossible_unification of t * t

  let rec follow subst (t : t) =
    match t with
    | { ty_descr = TyVar v; _ } ->
      begin match Subst.Var.get v subst with
        | t' -> follow subst t'
        | exception Not_found -> t
      end
    | t -> t

  let rec occurs u subst l (t : t) =
    (* no need to call expand_head here, since robinson_bind,
       and thus this function also, always receive types that
       have already been expanded. *)
    match t.ty_descr with
    | TyVar v ->
      List.exists (Id.equal v) l ||
      begin match Subst.Var.get v subst with
        | exception Not_found -> false
        | e -> occurs u subst (v :: l) e
      end
    | TyApp (_, tys) ->
      List.exists (occurs u subst l) tys
    | Arrow (args, ret) ->
      List.exists (occurs u subst l) args || occurs u subst l ret
    | Pi _ -> raise (Prenex_polymorphism t)

  let robinson_bind subst m v u =
    match u.ty_descr with
    | Pi _ -> raise (Prenex_polymorphism u)
    | _ ->
      if occurs u subst [v] u then
        raise (Impossible_unification (m, u))
      else
        Subst.Var.bind subst v u

  let rec robinson subst s t =
    let s = expand_head (follow subst s) in
    let t = expand_head (follow subst t) in
    match s, t with
    | ({ ty_descr = TyVar ({ builtin = Builtin.Wildcard _; _ } as v); _ } as m), u
    | u, ({ ty_descr = TyVar ({ builtin = Builtin.Wildcard _; _ } as v); _ } as m) ->
      if equal m u then subst else robinson_bind subst m v u
    | ({ ty_descr = TyVar v; _}, { ty_descr = TyVar v'; _ }) ->
      if Id.equal v v' then subst
      else raise (Impossible_unification (s, t))
    | { ty_descr = TyApp (f, f_args); _ },
      { ty_descr = TyApp (g, g_args); _ } ->
      if Id.equal f g then
        List.fold_left2 robinson subst f_args g_args
      else
        raise (Impossible_unification (s, t))
    | { ty_descr = Arrow (f_args, f_ret); _ },
      { ty_descr = Arrow (g_args, g_ret); _ } ->
      robinson_arrow subst f_args f_ret g_args g_ret
    | ({ ty_descr = Pi _; _ } as ty), _
    | _, ({ ty_descr = Pi _; _ } as ty) -> raise (Prenex_polymorphism ty)
    | _, _ -> raise (Impossible_unification (s, t))

  and robinson_arrow subst f_args f_ret g_args g_ret =
    match f_args, g_args with
    | [], [] -> robinson subst f_ret g_ret
    | [], _ -> robinson subst f_ret (arrow g_args g_ret)
    | _, [] -> robinson subst (arrow f_args f_ret) g_ret
    | f :: f_r, g :: g_r ->
      robinson_arrow (robinson subst f g) f_r f_ret g_r g_ret

  (* typing annotations *)
  let unify t t' =
    match robinson Subst.empty t t' with
    | s ->
      Subst.iter set_wildcard s;
      Some (subst s t)
    | exception Impossible_unification _ ->
      None

  (* free variables *)
  let rec free_vars acc (t : t) =
    match t.ty_descr with
    | TyVar v -> FV.add (FV.Ty v) acc
    | TyApp (_, l) -> List.fold_left free_vars acc l
    | Arrow (args, ret) -> List.fold_left free_vars (free_vars acc ret) args
    | Pi (vars, body) ->
      let fv = free_vars FV.empty body in
      let fv = FV.remove fv vars in
      FV.merge fv acc

  let fv t =
    let s = free_vars FV.empty t in
    let l, _ = FV.to_list s in
    l

  (* Freshening of bound type variables *)
  let freshen_var v = Id.mk v.path Type

  let rec freshen_aux subst (t: t) =
    match t.ty_descr with
    | TyVar v ->
      begin match Subst.Var.get v subst with
        | v' -> of_var v'
        | exception Not_found -> t
      end
    | TyApp (c, args) ->
      let args = List.map (freshen_aux subst) args in
      apply c args
    | Arrow (params, ret) ->
      let params = List.map (freshen_aux subst) params in
      let ret = freshen_aux subst ret in
      arrow params ret
    | Pi (vars, body) ->
      let subst, rev_vars =
        List.fold_left (fun (subst, rev_vars) var ->
            let new_var = freshen_var var in
            let subst = Subst.Var.bind subst var new_var in
            subst, (new_var :: rev_vars)
          ) (subst, []) vars
      in
      let body = freshen_aux subst body in
      pi (List.rev rev_vars) body

  let freshen t = freshen_aux Subst.empty t

  (* Access to type descr *)
  let descr (t: t) = (expand_head t).ty_descr

  (* View *)
  type view = [
    | `Prop
    | `Int
    | `Rat
    | `Real
    | `Array of ty * ty
    | `Bitv of int
    | `Float of int * int
    | `String
    | `String_reg_lang
    (* Generic cases *)
    | `Var of ty_var
    | `Wildcard of ty_var
    | `App of [
        | `Generic of ty_cst
        | `Builtin of builtin
      ] * ty list
    | `Arrow of ty list * ty
    | `Pi of ty_var list * ty
  ]

  let view (t : ty) : view =
    match descr t with
    | TyVar v ->
      begin match v with
        | { builtin = Builtin.Wildcard _;_ } -> `Wildcard v
        | _ -> `Var v
      end
    | Pi _ ->
      let vars, body = split_pi t in
      `Pi (vars, body)
    | Arrow _ ->
      let args, ret = split_arrow t in
      `Arrow (args, ret)
    | TyApp (({ builtin; _ } as c), l) ->
      begin match builtin with
        | Builtin.Prop -> `Prop
        | Builtin.Int -> `Int
        | Builtin.Rat -> `Rat
        | Builtin.Real -> `Real
        | Builtin.Bitv i -> `Bitv i
        | Builtin.Float (e, s) -> `Float (e, s)
        | Builtin.Array -> begin match l with
            | [src; dst] -> `Array (src, dst)
            | _ -> assert false (* not possible *)
          end
        | Builtin.String -> `String
        | Builtin.String_RegLan -> `String_reg_lang
        | Builtin.Base -> `App (`Generic c, l)
        | _ -> `App (`Builtin builtin, l)
      end

end

(* Terms *)
(* ************************************************************************* *)

module Term = struct

  type t = term
  type ty = Ty.t
  type ty_var = Ty.Var.t
  type ty_const = Ty.Const.t

  type subst = (term_var, term) Subst.t

  type 'a tag = 'a Tag.t

  (* Exceptions *)

  exception Wrong_type of t * ty
  exception Wrong_sum_type of term_cst * ty
  exception Wrong_record_type of term_cst * ty_cst

  exception Field_repeated of term_cst
  exception Field_missing of term_cst
  exception Field_expected of term_cst

  exception Pattern_expected of t
  exception Empty_pattern_matching
  exception Partial_pattern_match of t list
  exception Constructor_expected of term_cst

  exception Over_application of t list
  exception Bad_poly_arity of ty_var list * ty list


  (* *)

  (* Print *)
  let print = Print.term

  (* Tags *)
  let get_tag (t : t) k = Tag.get t.term_tags k
  let get_tag_last (t : t) k = Tag.get_last t.term_tags k
  let get_tag_list (t : t) k = Tag.get_list t.term_tags k

  let set_tag (t : t) k l = t.term_tags <- Tag.set t.term_tags k l
  let add_tag (t : t) k v = t.term_tags <- Tag.add t.term_tags k v
  let add_tag_opt (t : t) k o = t.term_tags <- Tag.add_opt t.term_tags k o
  let add_tag_list (t : t) k l = t.term_tags <- Tag.add_list t.term_tags k l

  let unset_tag (t: t) k = t.term_tags <- Tag.unset t.term_tags k

  (* Term creation *)
  let mk ?(tags=Tag.empty) term_descr term_ty =
    { term_descr; term_ty; term_hash = -1; term_tags = tags; }

  let of_var v = mk (Var v) v.id_ty
  let of_cst c = mk (Cst c) c.id_ty

  (* Hash *)
  let rec hash_aux t =
    match t.term_descr with
    | Var v ->
      Misc.hash2 3 (Id.hash v)
    | Cst c ->
      Misc.hash2 5 (Id.hash c)
    | App (f, tys, args) ->
      Misc.hash4 7 (hash f) (Misc.hash_list Ty.hash tys) (Misc.hash_list hash args)
    | Binder (b, body) ->
      Misc.hash3 11 (hash_binder b) (hash body)
    | Match (scrutinee, branches) ->
      Misc.hash3 13 (hash scrutinee) (hash_branches branches)

  and hash t =
    if t.term_hash >= 0 then t.term_hash
    else begin
      let res = hash_aux t in
      t.term_hash <- res;
      res
    end

  and hash_binder = function
    | Let_seq l ->
      let aux (v, t) = Misc.hash2 (Id.hash v) (hash t) in
      Misc.hash2 3 (Misc.hash_list aux l)
    | Let_par l ->
      let aux (v, t) = Misc.hash2 (Id.hash v) (hash t) in
      Misc.hash2 5 (Misc.hash_list aux l)
    | Lambda (tys, ts) ->
      Misc.hash3 7 (Misc.hash_list Id.hash tys) (Misc.hash_list Id.hash ts)
    | Exists (tys, ts) ->
      Misc.hash3 11 (Misc.hash_list Id.hash tys) (Misc.hash_list Id.hash ts)
    | Forall (tys, ts) ->
      Misc.hash3 13 (Misc.hash_list Id.hash tys) (Misc.hash_list Id.hash ts)

  and hash_branch (pattern, body) =
    Misc.hash2 (hash pattern) (hash body)

  and hash_branches l =
    Misc.hash_list hash_branch l

  (* Comparison *)
  let discr t =
    match t.term_descr with
    | Var _ -> 1
    | Cst _ -> 2
    | App _ -> 3
    | Binder _ -> 4
    | Match _ -> 5

  let binder_discr = function
    | Let_seq _ -> 1
    | Let_par _ -> 2
    | Lambda _ -> 3
    | Exists _ -> 4
    | Forall _ -> 5

  let rec compare u v =
    if u == v then 0 else begin
      let hu = hash u and hv = hash v in
      if hu <> hv then hu - hv
      else match u.term_descr, v.term_descr with
        | Var v1, Var v2 -> Id.compare v1 v2
        | Cst c1, Cst c2 -> Id.compare c1 c2
        | App (f1, tys1, args1), App (f2, tys2, args2) ->
          compare f1 f2
          <?> (lexicographic Ty.compare, tys1, tys2)
          <?> (lexicographic compare, args1, args2)
        | Binder (b, body), Binder (b', body') ->
          compare_binder b b'
          <?> (compare, body, body')
        | Match (s, l), Match (s', l') ->
          compare s s'
          <?> (lexicographic compare_branch, l, l')
        | _, _ -> (discr u) - (discr v)
    end

  and compare_binder b b' =
    match b, b' with
    | Let_seq l, Let_seq l' ->
      let aux (v, t) (v', t') = Id.compare v v' <?> (compare, t, t') in
      lexicographic aux l l'
    | Let_par l, Let_par l' ->
      let aux (v, t) (v', t') = Id.compare v v' <?> (compare, t, t') in
      lexicographic aux l l'
    | Lambda (tys, ts), Lambda (tys', ts') ->
      lexicographic Id.compare tys tys'
      <?> (lexicographic Id.compare, ts, ts')
    | Exists (tys, ts), Exists (tys', ts') ->
      lexicographic Id.compare tys tys'
      <?> (lexicographic Id.compare, ts, ts')
    | Forall (tys, ts), Forall (tys', ts') ->
      lexicographic Id.compare tys tys'
      <?> (lexicographic Id.compare, ts, ts')
    | _, _ -> (binder_discr b) - (binder_discr b')

  and compare_branch (p, b) (p', b') =
    compare p p' <?> (compare, b, b')

  let equal u v = compare u v = 0

  (* Inspection *)
  let ty { term_ty; _ } = term_ty

  (* free variables *)
  let rec free_vars acc (t : t) =
    match t.term_descr with
    | Var v -> FV.add (FV.Term v) (Ty.free_vars acc v.id_ty)
    | Cst _ -> acc
    | App (f, tys, ts) ->
      List.fold_left free_vars (
        List.fold_left Ty.free_vars (
          free_vars acc f
        ) tys
      ) ts
    | Binder ((Lambda (tys, ts) | Exists (tys, ts) | Forall (tys, ts)), body) ->
      let fv = free_vars FV.empty body in
      let fv = FV.remove fv tys in
      let fv = FV.remove fv ts in
      FV.merge fv acc
    | Binder (Let_seq l, body) ->
      let fv = free_vars FV.empty body in
      let fv = List.fold_right (fun (v, t) acc ->
          let acc = free_vars acc t in
          let acc = FV.del v acc in
          let acc = Ty.free_vars acc v.id_ty in
          acc
        ) l fv in
      FV.merge fv acc
    | Binder (Let_par l, body) ->
      let fv = free_vars FV.empty body in
      let fv = List.fold_right (fun (_, t) acc ->
          free_vars acc t
        ) l fv
      in
      let fv = List.fold_right (fun (v, _) acc ->
          let acc = FV.del v acc in
          Ty.free_vars acc v.id_ty
        ) l fv
      in
      FV.merge fv acc
    | Match (scrutinee, branches) ->
      let acc = free_vars acc scrutinee in
      List.fold_left (fun fv (pat, body) ->
          let free = free_vars FV.empty body in
          let bound = free_vars FV.empty pat in
          FV.merge fv (FV.diff free bound)
        ) acc branches

  let fv t =
    let s = free_vars FV.empty t in
    FV.to_list s

  (* Helpers for adt definition *)
  let mk_cstr ty_c path i vars args ret =
    let ty = Ty.pi vars (Ty.arrow args ret) in
    Id.mk path ty ~builtin:(Builtin.Constructor { adt = ty_c; case = i; })

  let mk_cstr_tester ty_c cstr i =
    let path = Path.rename (fun s -> "is:" ^ s) cstr.path in
    let vars, _, ret = Ty.poly_sig cstr.id_ty in
    let ty = Ty.pi vars (Ty.arrow [ret] Ty.prop) in
    Id.mk ~builtin:(Builtin.Tester { adt = ty_c; cstr; case = i; }) path ty

  (* ADT definition *)
  let define_adt_aux ~record ty_const vars l =
    let ty =  Ty.apply ty_const (List.map Ty.of_var vars) in
    let cases = ref [] in
    let l' = List.mapi (fun i (cstr_path, args) ->
        let args_ty = List.map fst args in
        let cstr = mk_cstr ty_const cstr_path i vars args_ty ty in
        let tester = mk_cstr_tester ty_const cstr i in
        let dstrs = Array.make (List.length args) None in
        let l' = List.mapi (fun j -> function
            | (arg_ty, None) -> (arg_ty, None)
            | (arg_ty, Some name) ->
              let dstr_ty = Ty.pi vars (Ty.arrow [ty] arg_ty) in
              let dstr =
                Id.mk name dstr_ty
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
    let def =
      Adt { ty = ty_const; record;
            cases = Array.of_list @@ List.rev !cases; }
    in
    Ty.define ty_const def;
    def, l'

  let define_adt = define_adt_aux ~record:false

  let define_record ty_const vars l =
    let path = ty_const.path in
    let cstr_args = List.map (fun (field_name, ty) ->
        ty, Some field_name
      ) l in
    let def, l' = define_adt_aux ~record:true ty_const vars [path, cstr_args] in
    match l' with
    | [ _, l'' ] ->
      let fields =
        List.map (function
            | _, Some dstr -> dstr
            | _, None -> assert false
          ) l''
      in
      def, fields
    | _ -> assert false


  module Pattern_matching = struct
    (* Pattern matching analysis

       We perform an analysis on pattern matching to determine two things:
       - which patterns are redundant, if any
       - which patterns are missing from the match, if any

       The analysis is taken from the following research paper:
       Warnings for pattern matching - LUC MARANGET
       http://moscova.inria.fr/~maranget/papers/warn/warn.pdf
    *)

    type pattern_head =
      | Wildcard
      | Cstr of {
          case : int;
          adt : ty_cst;
          cstr : term_cst;
          args : term list;
        }

    type pat = {
      term : term;
      head : pattern_head;
    }

    let pp_pat fmt { term; _ } =
      Format.fprintf fmt "%a" Print.term term

    type matrix = pat list list

    let pp_row fmt r =
      let pp_sep fmt () = Format.fprintf fmt "\t@ " in
      Format.fprintf fmt "(@[<h>%a@])" (Format.pp_print_list ~pp_sep pp_pat) r

    let _pp_matrix fmt m =
      let pp_sep fmt () = Format.fprintf fmt "@ " in
      Format.fprintf fmt "(@[<v>%a@])" (Format.pp_print_list ~pp_sep pp_row) m

    let view_pat t =
      match t.term_descr with
      | Var _ -> Wildcard
      | Cst ( { builtin = Builtin.Constructor { adt; case; }; _ } as cstr ) ->
        Cstr { cstr; adt; case; args = []; }
      | App ({ term_descr = Cst ({
          builtin = Builtin.Constructor { adt; case; }; _ } as cstr); _ }, _, args) ->
        Cstr { cstr; adt; case; args; }
      | Cst _ | App _ | Binder _ | Match _ ->
        raise (Pattern_expected t)

    let mk_pat term =
      { term; head = view_pat term; }

    let specialise_row arity c = function
      | [] -> assert false
      | p_i_1 :: p_r ->
        begin match p_i_1.head with
          | Wildcard ->
            [init_list arity (fun _ -> p_i_1) @ p_r]
          | Cstr { cstr = c'; args = r; _ } ->
            if Id.equal c c' then [List.map mk_pat r @ p_r] else []
        end

    let specialise_vector arity c q =
      match specialise_row arity c q with
      | [q'] -> q'
      | [] | _ :: _ :: _-> assert false (* internal error *)

    let specialise_matrix arity c p =
      list_concat_map (specialise_row arity c) p

    let default_matrix (p : matrix) : matrix =
      list_concat_map (function
          | [] -> assert false
          | p_i_1 :: p_i_r ->
            begin match p_i_1.head with
              | Wildcard -> [p_i_r]
              | Cstr _ -> []
            end) p

    let all_constructors_appear_in_first_col (p : matrix) =
      let r = ref None in
      let cstrs = ref [| |] in
      let expand_for adt =
        match !r with
        | Some a -> a
        | None ->
          begin match Ty.definition adt with
            | Some Adt { cases; _ } ->
              let a = Array.map (fun _ -> false) cases in
              cstrs := cases;
              r := Some a;
              a
            | _ -> assert false
          end
      in
      let set adt i =
        let a = expand_for adt in
        a.(i) <- true
      in
      List.iter (function
          | [] -> ()
          | p_1 :: _ ->
            begin match p_1.head with
              | Wildcard -> ()
              | Cstr { case; adt; _ } ->
                set adt case
            end
        ) p;
      match !r with
      | None -> `Missing []
      | Some a ->
        let all_occurs = ref true in
        let missing = ref [] in
        let all = ref [] in
        Array.iter2 (fun (case : ty_def_adt_case) occurs ->
            if occurs
            then (all := case.cstr :: !all)
            else (all_occurs := false; missing := case.cstr :: !missing)
          ) !cstrs a;
        if !all_occurs
        then `All_present !all
        else `Missing !missing

    let rec u_rec (p : matrix) = function
      | [] ->
        begin match p with
          | [] -> true
          | _ :: _ -> false
        end
      | (q_1 :: q_r) as q ->
        begin match q_1.head with
          | Cstr { cstr; args; _ } ->
            let arity = List.length args in
            let p' = specialise_matrix arity cstr p in
            let q' = specialise_vector arity cstr q in
            u_rec p' q'
          | Wildcard ->
            begin match all_constructors_appear_in_first_col p with
              | `All_present cstrs ->
                List.for_all (fun cstr ->
                    let arity = Ty.t_arity cstr.id_ty in
                    let p' = specialise_matrix arity cstr p in
                    let q' = specialise_vector arity cstr q in
                    u_rec p' q'
                  ) cstrs
              | `Missing _ ->
                let p' = default_matrix p in
                let q' = q_r in
                u_rec p' q'
            end
        end

    let wildcard_ty () =
      Ty.of_var @@ Ty.wildcard_var ()

    let wildcard_term () =
      of_var (Id.mk (Path.local "_") (wildcard_ty ()))

    let rec i ~apply (p : matrix) n =
      match p, n with
      | [], _ -> Some (init_list n (fun _ -> wildcard_term ()))
      | _ :: _, 0 -> None
      | _, _ ->
        begin match all_constructors_appear_in_first_col p with
          | `All_present cstrs ->
            list_find_map (fun cstr ->
                let vars, params, _ = Ty.poly_sig cstr.id_ty in
                let arity = List.length params in
                let p' = specialise_matrix arity cstr p in
                match i ~apply p' (arity + n - 1) with
                | None -> None
                | Some res ->
                  let args, rest = list_take res arity in
                  let tys = List.map (fun _ -> wildcard_ty ()) vars in
                  let t = apply (of_cst cstr) tys args in
                  Some (t :: rest)
              ) cstrs
          | `Missing cstrs ->
            let p' = default_matrix p in
            begin match i ~apply p' (n - 1) with
              | None -> None
              | Some res ->
                let head =
                  match cstrs with
                  | [] -> wildcard_term ()
                  | cstr :: _ ->
                    let vars, params, _ = Ty.poly_sig cstr.id_ty in
                    let tys = List.map (fun _ -> wildcard_ty ()) vars in
                    let args = List.map (fun _ -> wildcard_term ()) params in
                    apply (of_cst cstr) tys args
                in
                Some (head :: res)
            end
        end

    let analyze ~apply patterns =
      let redundant, matrix =
        List.fold_left (fun (redundant, matrix) pattern ->
            let pat = mk_pat pattern in
            let redundant =
              if u_rec matrix [pat]
              then redundant
              else pattern :: redundant
            in
            let matrix = matrix @ [[pat]] in
            (redundant, matrix)
          ) ([], []) patterns
      in
      let missing =
        match i ~apply matrix 1 with
        | None -> []
        | Some ([_] as res) -> res
        | Some _ -> assert false
      in
      redundant, missing

  end



  (* Variables *)
  module Var = struct
    type t = term_var
    let hash = Id.hash
    let print = Id.print
    let equal = Id.equal
    let compare = Id.compare
    let get_tag = Id.get_tag
    let get_tag_last = Id.get_tag_last
    let get_tag_list = Id.get_tag_list
    let set_tag = Id.set_tag
    let add_tag = Id.add_tag
    let add_tag_opt = Id.add_tag_opt
    let add_tag_list = Id.add_tag_list
    let unset_tag = Id.unset_tag

    let ty ({ id_ty; _ } : t) = id_ty
    let create path ty = Id.mk path ty
    let mk name ty = Id.mk (Path.local name) ty
  end

  (* Constants *)
  module Const = struct
    type t = term_cst
    let hash = Id.hash
    let print = Id.print
    let equal = Id.equal
    let compare = Id.compare
    let get_tag = Id.get_tag
    let get_tag_last = Id.get_tag_last
    let get_tag_list = Id.get_tag_list
    let set_tag = Id.set_tag
    let add_tag = Id.add_tag
    let add_tag_opt = Id.add_tag_opt
    let add_tag_list = Id.add_tag_list
    let unset_tag = Id.unset_tag

    let ty ({ id_ty; _ } : t) = id_ty

    let mk path ty =
      Id.mk path ty

    let mk' ?pos ?name ?builtin ?tags cname vars args ret =
      let ty = Ty.pi vars (Ty.arrow args ret) in
      Id.mk ?pos ?name ?builtin ?tags (Path.global cname) ty

    let indexed
        ?pos ?name ?builtin ?tags
        cname fun_vars fun_arg fun_ret =
      with_cache ~cache:(Hashtbl.create 13) (fun i ->
          let fun_args = replicate i fun_arg in
          mk' ?pos ?name ?builtin ?tags cname fun_vars fun_args fun_ret
        )

    (* Some constants *)
    let _true =
      Id.mk ~name:"⊤" ~builtin:Builtin.True (Path.global "true") Ty.prop

    let _false =
      Id.mk ~name:"⊥" ~builtin:Builtin.False (Path.global "false") Ty.prop

    let eqs =
      let a = Ty.Var.mk "alpha" in
      let a_ty = Ty.of_var a in
      indexed
        ~pos:Pretty.Infix ~name:"=" ~builtin:Builtin.Equal
        "eqs" [a] a_ty Ty.prop

    let eq = eqs 2

    let distinct =
      let a = Ty.Var.mk "alpha" in
      let a_ty = Ty.of_var a in
      indexed ~builtin:Builtin.Distinct "Distinct" [a] a_ty Ty.prop

    let neq = distinct 2

    let neg = mk'
        ~pos:Pretty.Prefix ~name:"¬" ~builtin:Builtin.Neg
        "Neg" [] [Ty.prop] Ty.prop

    let _and = indexed
        ~pos:Pretty.Infix ~name:"∧" ~builtin:Builtin.And
        "And" [] Ty.prop Ty.prop

    let and_ = _and 2

    let _or = indexed
        ~pos:Pretty.Infix ~name:"∨" ~builtin:Builtin.Or
        "Or" [] Ty.prop Ty.prop

    let or_ = _or 2

    let nand = mk'
        ~pos:Pretty.Infix ~name:"⊼" ~builtin:Builtin.Nand
        "Nand" [] [Ty.prop; Ty.prop] Ty.prop

    let nor = mk'
        ~pos:Pretty.Infix ~name:"V" ~builtin:Builtin.Nor
        "or" [] [Ty.prop; Ty.prop] Ty.prop

    let xor = mk'
        ~pos:Pretty.Infix ~name:"⊻" ~builtin:Builtin.Xor
        "Xor" [] [Ty.prop; Ty.prop] Ty.prop

    let imply = mk'
        ~pos:Pretty.Infix ~name:"⇒" ~builtin:Builtin.Imply
        "Imply" [] [Ty.prop; Ty.prop] Ty.prop

    let implied = mk'
        ~pos:Pretty.Infix ~name:"⇐" ~builtin:Builtin.Implied
        "Implied" [] [Ty.prop; Ty.prop] Ty.prop

    let equiv = mk'
        ~pos:Pretty.Infix ~name:"⇔" ~builtin:Builtin.Equiv
        "Equiv" [] [Ty.prop; Ty.prop] Ty.prop

    let ite =
      let a = Ty.Var.mk "alpha" in
      let a_ty = Ty.of_var a in
      mk'
        ~name:"ite" ~builtin:Builtin.Ite
        "Ite" [a] [Ty.prop; a_ty; a_ty] a_ty

    let pi =
      let a = Ty.Var.mk "alpha" in
      let a_ty = Ty.of_var a in
      mk' ~name:"Π" ~builtin:Builtin.Pi
        "Pi" [a] [Ty.(arrow [a_ty] prop)] Ty.prop

    let sigma =
      let a = Ty.Var.mk "alpha" in
      let a_ty = Ty.of_var a in
      mk' ~name:"Σ" ~builtin:Builtin.Sigma
        "Sigma" [a] [Ty.(arrow [a_ty] prop)] Ty.prop

    let coerce =
      let a = Ty.Var.mk "alpha" in
      let b = Ty.Var.mk "beta" in
      mk' ~builtin:Builtin.Coercion "coerce"
        [a; b] [Ty.of_var a] (Ty.of_var b)

    let in_interval (b1, b2) = mk'
        ~name:"in_interval" ~builtin:(Builtin.In_interval (b1, b2))
        "InInterval" [] [Ty.int; Ty.int; Ty.int] Ty.prop

    let maps_to =
      let a = Ty.Var.mk "alpha" in
      let b = Ty.Var.mk "beta" in
      mk'
        ~name:"maps_to" ~builtin:Builtin.Maps_to
        "MapsTo" [a; b] [Ty.of_var a; Ty.of_var b] Ty.prop

    module Int = struct

      let int =
        with_cache ~cache:(Hashtbl.create 113) (fun s ->
            mk' ~builtin:(Builtin.Integer s) s [] [] Ty.int
          )

      let minus = mk'
          ~pos:Pretty.Prefix ~name:"-" ~builtin:(Builtin.Minus `Int)
          "Minus" [] [Ty.int] Ty.int

      let add = mk'
          ~pos:Pretty.Infix ~name:"+" ~builtin:(Builtin.Add `Int)
          "Add" [] [Ty.int; Ty.int] Ty.int

      let sub = mk'
          ~pos:Pretty.Infix ~name:"-" ~builtin:(Builtin.Sub `Int)
           "Sub" [] [Ty.int; Ty.int] Ty.int

      let mul = mk'
          ~pos:Pretty.Infix ~name:"*" ~builtin:(Builtin.Mul `Int)
           "Mul" [] [Ty.int; Ty.int] Ty.int

      let pow = mk'
          ~pos:Pretty.Infix ~name:"**" ~builtin:(Builtin.Pow `Int)
           "Pow" [] [Ty.int; Ty.int] Ty.int

      let div_e = mk'
          ~pos:Pretty.Prefix ~name:"div" ~builtin:(Builtin.Div_e `Int)
           "Div_e" [] [Ty.int; Ty.int] Ty.int
      let div_t = mk'
          ~pos:Pretty.Infix ~name:"/t" ~builtin:(Builtin.Div_t `Int)
          "Div_t" [] [Ty.int; Ty.int] Ty.int
      let div_f = mk'
          ~pos:Pretty.Infix ~name:"/f" ~builtin:(Builtin.Div_f `Int)
           "Div_f" [] [Ty.int; Ty.int] Ty.int

      let rem_e = mk'
          ~pos:Pretty.Infix ~name:"%" ~builtin:(Builtin.Modulo_e `Int)
           "Modulo_e" [] [Ty.int; Ty.int] Ty.int
      let rem_t = mk'
          ~pos:Pretty.Infix ~name:"%t" ~builtin:(Builtin.Modulo_t `Int)
           "Modulo_t" [] [Ty.int; Ty.int] Ty.int
      let rem_f = mk'
          ~pos:Pretty.Infix ~name:"%f" ~builtin:(Builtin.Modulo_f `Int)
           "Modulo_f" [] [Ty.int; Ty.int] Ty.int

      let abs = mk'
          ~name:"abs" ~builtin:Builtin.Abs
          "Abs" [] [Ty.int] Ty.int

      let lt = mk'
          ~pos:Pretty.Infix ~name:"<" ~builtin:Builtin.(Lt `Int)
           "LessThan" [] [Ty.int; Ty.int] Ty.prop

      let le = mk'
          ~pos:Pretty.Infix ~name:"<=" ~builtin:Builtin.(Leq `Int)
           "LessOrEqual" [] [Ty.int; Ty.int] Ty.prop

      let gt = mk'
          ~pos:Pretty.Infix ~name:">" ~builtin:Builtin.(Gt `Int)
           "GreaterThan" [] [Ty.int; Ty.int] Ty.prop

      let ge = mk'
          ~pos:Pretty.Infix ~name:">=" ~builtin:Builtin.(Geq `Int)
           "GreaterOrEqual" [] [Ty.int; Ty.int] Ty.prop

      let floor = mk'
          ~name:"floor" ~builtin:Builtin.(Floor `Int)
           "Floor" [] [Ty.int] Ty.int

      let ceiling = mk'
          ~name:"ceiling" ~builtin:Builtin.(Ceiling `Int)
           "Ceiling" [] [Ty.int] Ty.int

      let truncate = mk'
          ~name:"truncate" ~builtin:Builtin.(Truncate `Int)
           "Truncate" [] [Ty.int] Ty.int

      let round = mk'
          ~name:"round" ~builtin:Builtin.(Round `Int)
           "Round" [] [Ty.int] Ty.int

      let is_int = mk'
          ~name:"is_int" ~builtin:Builtin.(Is_int `Int)
           "Is_int" [] [Ty.int] Ty.prop

      let is_rat = mk'
          ~name:"is_rat" ~builtin:Builtin.(Is_rat `Int)
           "Is_rat" [] [Ty.int] Ty.prop

      let divisible = mk'
          ~builtin:Builtin.Divisible "Divisible"
          [] [Ty.int; Ty.int] Ty.prop

    end

    module Rat = struct

      let rat =
        with_cache ~cache:(Hashtbl.create 113) (fun s ->
            mk' ~builtin:(Builtin.Rational s) s [] [] Ty.rat
          )

      let minus = mk'
          ~pos:Pretty.Prefix ~name:"-" ~builtin:(Builtin.Minus `Rat)
           "Minus" [] [Ty.rat] Ty.rat

      let add = mk'
          ~pos:Pretty.Infix ~name:"+" ~builtin:(Builtin.Add `Rat)
           "Add" [] [Ty.rat; Ty.rat] Ty.rat

      let sub = mk'
          ~pos:Pretty.Infix ~name:"-" ~builtin:(Builtin.Sub `Rat)
           "Sub" [] [Ty.rat; Ty.rat] Ty.rat

      let mul = mk'
          ~pos:Pretty.Infix ~name:"*" ~builtin:(Builtin.Mul `Rat)
           "Mul" [] [Ty.rat; Ty.rat] Ty.rat

      let div = mk'
          ~pos:Pretty.Infix ~name:"/" ~builtin:(Builtin.Div `Rat)
           "Div" [] [Ty.rat; Ty.rat] Ty.rat
      let div_e = mk'
          ~pos:Pretty.Infix ~name:"/e" ~builtin:(Builtin.Div_e `Rat)
          "Div_e" [] [Ty.rat; Ty.rat] Ty.rat
      let div_t = mk'
          ~pos:Pretty.Infix ~name:"/t" ~builtin:(Builtin.Div_t `Rat)
           "Div_t" [] [Ty.rat; Ty.rat] Ty.rat
      let div_f = mk'
          ~pos:Pretty.Infix ~name:"/f" ~builtin:(Builtin.Div_f `Rat)
           "Div_f" [] [Ty.rat; Ty.rat] Ty.rat

      let rem_e = mk'
          ~pos:Pretty.Infix ~name:"%" ~builtin:(Builtin.Modulo_e `Rat)
           "Modulo" [] [Ty.rat; Ty.rat] Ty.rat
      let rem_t = mk'
          ~pos:Pretty.Infix ~name:"%t" ~builtin:(Builtin.Modulo_t `Rat)
           "Modulo_t" [] [Ty.rat; Ty.rat] Ty.rat
      let rem_f = mk'
          ~pos:Pretty.Infix ~name:"%f" ~builtin:(Builtin.Modulo_f `Rat)
           "Modulo_f" [] [Ty.rat; Ty.rat] Ty.rat

      let lt = mk'
          ~pos:Pretty.Infix ~name:"<" ~builtin:(Builtin.Lt `Rat)
           "LessThan" [] [Ty.rat; Ty.rat] Ty.prop

      let le = mk'
          ~pos:Pretty.Infix ~name:"<=" ~builtin:(Builtin.Leq `Rat)
           "LessOrEqual" [] [Ty.rat; Ty.rat] Ty.prop

      let gt = mk'
          ~pos:Pretty.Infix ~name:">" ~builtin:(Builtin.Gt `Rat)
           "GreaterThan" [] [Ty.rat; Ty.rat] Ty.prop

      let ge = mk'
          ~pos:Pretty.Infix ~name:">=" ~builtin:(Builtin.Geq `Rat)
           "GreaterOrEqual" [] [Ty.rat; Ty.rat] Ty.prop

      let floor = mk'
          ~name:"floor" ~builtin:(Builtin.Floor `Rat)
           "Floor" [] [Ty.rat] Ty.rat

      let ceiling = mk'
          ~name:"ceiling" ~builtin:(Builtin.Ceiling `Rat)
           "Ceiling" [] [Ty.rat] Ty.rat

      let truncate = mk'
          ~name:"truncate" ~builtin:(Builtin.Truncate `Rat)
           "Truncate" [] [Ty.rat] Ty.rat

      let round = mk'
          ~name:"round" ~builtin:(Builtin.Round `Rat)
           "Round" [] [Ty.rat] Ty.rat

      let is_int = mk'
          ~name:"is_int" ~builtin:(Builtin.Is_int `Rat)
           "Is_int" [] [Ty.rat] Ty.prop

      let is_rat = mk'
          ~name:"is_rat" ~builtin:(Builtin.Is_rat `Rat)
           "Is_rat" [] [Ty.rat] Ty.prop
    end

    module Real = struct

      let real =
        with_cache ~cache:(Hashtbl.create 113) (fun s ->
            mk' ~builtin:(Builtin.Decimal s) s [] [] Ty.real
          )

      let minus = mk'
          ~pos:Pretty.Prefix ~name:"-" ~builtin:(Builtin.Minus `Real)
           "Minus" [] [Ty.real] Ty.real

      let add = mk'
          ~pos:Pretty.Infix ~name:"+" ~builtin:(Builtin.Add `Real)
           "Add" [] [Ty.real; Ty.real] Ty.real

      let sub = mk'
          ~pos:Pretty.Infix ~name:"-" ~builtin:(Builtin.Sub `Real)
           "Sub" [] [Ty.real; Ty.real] Ty.real

      let mul = mk'
          ~pos:Pretty.Infix ~name:"*" ~builtin:(Builtin.Mul `Real)
           "Mul" [] [Ty.real; Ty.real] Ty.real

      let pow = mk'
          ~pos:Pretty.Infix ~name:"**" ~builtin:(Builtin.Pow `Real)
           "Pow" [] [Ty.real; Ty.real] Ty.real

      let div = mk'
          ~pos:Pretty.Infix ~name:"/" ~builtin:(Builtin.Div `Real)
           "Div" [] [Ty.real; Ty.real] Ty.real

      let div_e = mk'
          ~pos:Pretty.Infix ~name:"/" ~builtin:(Builtin.Div_e `Real)
           "Div_e" [] [Ty.real; Ty.real] Ty.real
      let div_t = mk'
          ~pos:Pretty.Infix ~name:"/t" ~builtin:(Builtin.Div_t `Real)
          "Div_t" [] [Ty.real; Ty.real] Ty.real
      let div_f = mk'
          ~pos:Pretty.Infix ~name:"/f" ~builtin:(Builtin.Div_f `Real)
           "Div_f" [] [Ty.real; Ty.real] Ty.real

      let rem_e = mk'
          ~pos:Pretty.Infix ~name:"%" ~builtin:(Builtin.Modulo_e `Real)
           "Modulo" [] [Ty.real; Ty.real] Ty.real
      let rem_t = mk'
          ~pos:Pretty.Infix ~name:"%t" ~builtin:(Builtin.Modulo_t `Real)
          "Modulo_t" [] [Ty.real; Ty.real] Ty.real
      let rem_f = mk'
          ~pos:Pretty.Infix ~name:"%f" ~builtin:(Builtin.Modulo_f `Real)
           "Modulo_f" [] [Ty.real; Ty.real] Ty.real

      let lt = mk'
          ~pos:Pretty.Infix ~name:"<" ~builtin:(Builtin.Lt `Real)
           "LessThan" [] [Ty.real; Ty.real] Ty.prop

      let le = mk'
          ~pos:Pretty.Infix ~name:"<=" ~builtin:(Builtin.Leq `Real)
           "LessOrEqual" [] [Ty.real; Ty.real] Ty.prop

      let gt = mk'
          ~pos:Pretty.Infix ~name:">" ~builtin:(Builtin.Gt `Real)
           "GreaterThan" [] [Ty.real; Ty.real] Ty.prop

      let ge = mk'
          ~pos:Pretty.Infix ~name:">=" ~builtin:(Builtin.Geq `Real)
           "GreaterOrEqual" [] [Ty.real; Ty.real] Ty.prop

      let floor = mk'
          ~name:"floor" ~builtin:(Builtin.Floor `Real)
           "Floor" [] [Ty.real] Ty.real

      let floor_to_int = mk'
          ~name:"floor_to_int" ~builtin:(Builtin.Floor_to_int `Real)
           "Floor" [] [Ty.real] Ty.int

      let ceiling = mk'
          ~name:"ceiling" ~builtin:(Builtin.Ceiling `Real)
           "Ceiling" [] [Ty.real] Ty.real

      let truncate = mk'
          ~name:"truncate" ~builtin:(Builtin.Truncate `Real)
           "Truncate" [] [Ty.real] Ty.real

      let round = mk'
          ~name:"round" ~builtin:(Builtin.Round `Real)
           "Round" [] [Ty.real] Ty.real

      let is_int = mk'
          ~name:"is_int" ~builtin:(Builtin.Is_int `Real)
           "Is_int" [] [Ty.real] Ty.prop

      let is_rat = mk'
          ~name:"is_rat" ~builtin:(Builtin.Is_rat `Real)
           "Is_rat" [] [Ty.real] Ty.prop
    end

    module Array = struct

      let const =
        let a = Ty.Var.mk "alpha" in
        let a_ty = Ty.of_var a in
        let b = Ty.Var.mk "beta" in
        let b_ty = Ty.of_var b in
        mk'
          ~name:"const" ~builtin:Builtin.Const
          "Const" [a; b] [b_ty] (Ty.array a_ty b_ty)

      let select =
        let a = Ty.Var.mk "alpha" in
        let a_ty = Ty.of_var a in
        let b = Ty.Var.mk "beta" in
        let b_ty = Ty.of_var b in
        mk'
          ~name:"select" ~builtin:Builtin.Select
          "Select" [a; b] [Ty.array a_ty b_ty; a_ty] b_ty

      let store =
        let a = Ty.Var.mk "alpha" in
        let a_ty = Ty.of_var a in
        let b = Ty.Var.mk "beta" in
        let b_ty = Ty.of_var b in
        let arr = Ty.array a_ty b_ty in
        mk'
          ~name:"store" ~builtin:Builtin.Store
          "Store" [a; b] [arr; a_ty; b_ty] arr

    end

    module Bitv = struct

      let bitv s =
        mk' ~builtin:(Builtin.Bitvec s)
          (Format.asprintf "bv#%s#" s) [] [] (Ty.bitv (String.length s))

      let concat =
        with_cache ~cache:(Hashtbl.create 13) (fun (n, m) ->
            mk' ~builtin:(Builtin.Bitv_concat{n;m}) "bitv_concat"
              [] [Ty.bitv n; Ty.bitv m] (Ty.bitv (n + m))
          )

      let extract =
        with_cache ~cache:(Hashtbl.create 13) (fun (i, j, n) ->
            mk' ~builtin:(Builtin.Bitv_extract {n; i; j})
              (Format.asprintf "bitv_extract_%d_%d" i j) []
              [Ty.bitv n] (Ty.bitv (i - j + 1))
          )

      let repeat =
        with_cache ~cache:(Hashtbl.create 13) (fun (k, n) ->
            mk' ~builtin:(Builtin.Bitv_repeat{n;k}) (Format.asprintf "bitv_repeat_%d" k)
              [] [Ty.bitv n] (Ty.bitv (n * k))
          )

      let zero_extend =
        with_cache ~cache:(Hashtbl.create 13) (fun (k, n) ->
            mk' ~builtin:(Builtin.Bitv_zero_extend{n;k}) (Format.asprintf "zero_extend_%d" k)
              [] [Ty.bitv n] (Ty.bitv (n + k))
          )

      let sign_extend =
        with_cache ~cache:(Hashtbl.create 13) (fun (k, n) ->
            mk' ~builtin:(Builtin.Bitv_sign_extend{n;k}) (Format.asprintf "sign_extend_%d" k)
              [] [Ty.bitv n] (Ty.bitv (n + k))
          )

      let rotate_right =
        with_cache ~cache:(Hashtbl.create 13) (fun (i, n) ->
            mk' ~builtin:(Builtin.Bitv_rotate_right{n;i})
              (Format.asprintf "rotate_right_%d" i) [] [Ty.bitv n] (Ty.bitv n)
          )

      let rotate_left =
        with_cache ~cache:(Hashtbl.create 13) (fun (i, n) ->
            mk' ~builtin:(Builtin.Bitv_rotate_left{n;i})
              (Format.asprintf "rotate_left_%d" i) [] [Ty.bitv n] (Ty.bitv n)
          )

      let not =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            mk' ~builtin:(Builtin.Bitv_not n) "bvnot" [] [Ty.bitv n] (Ty.bitv n)
          )

      let and_ =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            mk' ~builtin:(Builtin.Bitv_and n) "bvand" []
              [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let or_ =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            mk' ~builtin:(Builtin.Bitv_or n) "bvor" []
              [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let nand =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            mk' ~builtin:(Builtin.Bitv_nand n) "bvnand" []
              [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let nor =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            mk' ~builtin:(Builtin.Bitv_nor n) "bvnor" []
              [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let xor =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            mk' ~builtin:(Builtin.Bitv_xor n) "bvxor" []
              [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let xnor =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            mk' ~builtin:(Builtin.Bitv_xnor n) "bvxnor" []
              [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let comp =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            mk' ~builtin:(Builtin.Bitv_comp n) "bvcomp" []
              [Ty.bitv n; Ty.bitv n] (Ty.bitv 1)
          )

      let neg =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            mk' ~builtin:(Builtin.Bitv_neg n) "bvneg" [] [Ty.bitv n] (Ty.bitv n)
          )

      let add =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            mk' ~builtin:(Builtin.Bitv_add n) "bvadd" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let sub =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            mk' ~builtin:(Builtin.Bitv_sub n) "bvsub" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let mul =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            mk' ~builtin:(Builtin.Bitv_mul n) "bvmul" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let udiv =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            mk' ~builtin:(Builtin.Bitv_udiv n) "bvudiv" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let urem =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            mk' ~builtin:(Builtin.Bitv_urem n) "bvurem" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let sdiv =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            mk' ~builtin:(Builtin.Bitv_sdiv n) "bvsdiv" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let srem =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            mk' ~builtin:(Builtin.Bitv_srem n) "bvsrem" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let smod =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            mk' ~builtin:(Builtin.Bitv_smod n) "bvsmod" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let shl =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            mk' ~builtin:(Builtin.Bitv_shl n) "bvshl" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let lshr =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            mk' ~builtin:(Builtin.Bitv_lshr n) "bvlshr" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let ashr =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            mk' ~builtin:(Builtin.Bitv_ashr n) "bvashr" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let ult =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            mk' ~builtin:(Builtin.Bitv_ult n) "bvult" [] [Ty.bitv n; Ty.bitv n] Ty.prop
          )

      let ule =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            mk' ~builtin:(Builtin.Bitv_ule n) "bvule" [] [Ty.bitv n; Ty.bitv n] Ty.prop
          )

      let ugt =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            mk' ~builtin:(Builtin.Bitv_ugt n) "bvugt" [] [Ty.bitv n; Ty.bitv n] Ty.prop
          )

      let uge =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            mk' ~builtin:(Builtin.Bitv_uge n) "bvsge" [] [Ty.bitv n; Ty.bitv n] Ty.prop
          )

      let slt =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            mk' ~builtin:(Builtin.Bitv_slt n) "bvslt" [] [Ty.bitv n; Ty.bitv n] Ty.prop
          )

      let sle =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            mk' ~builtin:(Builtin.Bitv_sle n) "bvsle" [] [Ty.bitv n; Ty.bitv n] Ty.prop
          )

      let sgt =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            mk' ~builtin:(Builtin.Bitv_sgt n) "bvsgt" [] [Ty.bitv n; Ty.bitv n] Ty.prop
          )

      let sge =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            mk' ~builtin:(Builtin.Bitv_sge n) "bvsge" [] [Ty.bitv n; Ty.bitv n] Ty.prop
          )

    end

    module Float = struct

      let fp =
        with_cache ~cache:(Hashtbl.create 13) (fun (e, s) ->
            mk' ~builtin:(Builtin.Fp(e, s)) "fp" []
              [Ty.bitv 1; Ty.bitv e; Ty.bitv (s-1)] (Ty.float e s)
          )

      let roundNearestTiesToEven =
        mk' ~builtin:Builtin.RoundNearestTiesToEven "RoundNearestTiesToEven" [] [] Ty.roundingMode

      let roundNearestTiesToAway =
        mk' ~builtin:Builtin.RoundNearestTiesToAway "RoundNearestTiesToAway" [] [] Ty.roundingMode

      let roundTowardPositive =
        mk' ~builtin:Builtin.RoundTowardPositive "RoundTowardPositive" [] [] Ty.roundingMode

      let roundTowardNegative =
        mk' ~builtin:Builtin.RoundTowardNegative "RoundTowardNegative" [] [] Ty.roundingMode

      let roundTowardZero =
        mk' ~builtin:Builtin.RoundTowardZero "RoundTowardZero" [] [] Ty.roundingMode

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
            mk' ~builtin:(builtin es) name [] args res
          )

      let plus_infinity =
        fp_gen_fun ~args:0 "plus_infinity"
          (fun (e,s) -> Builtin.Plus_infinity (e,s))
      let minus_infinity =
        fp_gen_fun ~args:0 "minus_infinity"
          (fun (e,s) -> Builtin.Minus_infinity (e,s))
      let plus_zero =
        fp_gen_fun ~args:0 "plus_zero"
          (fun (e,s) -> Builtin.Plus_zero (e,s))
      let minus_zero =
        fp_gen_fun ~args:0 "minus_zero"
          (fun (e,s) -> Builtin.Minus_zero (e,s))
      let nan =
        fp_gen_fun ~args:0 "nan"
          (fun (e,s) -> Builtin.NaN (e,s))
      let abs =
        fp_gen_fun ~args:1 "fp.abs"
          (fun (e,s) -> Builtin.Fp_abs (e,s))
      let neg =
        fp_gen_fun ~args:1 "fp.neg"
          (fun (e,s) -> Builtin.Fp_neg (e,s))
      let add =
        fp_gen_fun ~args:2 ~rm:() "fp.add"
          (fun (e,s) -> Builtin.Fp_add (e,s))
      let sub =
        fp_gen_fun ~args:2 ~rm:() "fp.sub"
          (fun (e,s) -> Builtin.Fp_sub (e,s))
      let mul =
        fp_gen_fun ~args:2 ~rm:() "fp.mul"
          (fun (e,s) -> Builtin.Fp_mul (e,s))
      let div =
        fp_gen_fun ~args:2 ~rm:() "fp.div"
          (fun (e,s) -> Builtin.Fp_div (e,s))
      let fma =
        fp_gen_fun ~args:3 ~rm:() "fp.fma"
          (fun (e,s) -> Builtin.Fp_fma (e,s))
      let sqrt =
        fp_gen_fun ~args:1 ~rm:() "fp.sqrt"
          (fun (e,s) -> Builtin.Fp_sqrt (e,s))
      let rem =
        fp_gen_fun ~args:2 "fp.rem"
          (fun (e,s) -> Builtin.Fp_rem (e,s))
      let roundToIntegral =
        fp_gen_fun ~args:1 ~rm:() "fp.roundToIntegral"
          (fun (e,s) -> Builtin.Fp_roundToIntegral (e,s))
      let min =
        fp_gen_fun ~args:2 "fp.min"
          (fun (e,s) -> Builtin.Fp_min (e,s))
      let max =
        fp_gen_fun ~args:2 "fp.max"
          (fun (e,s) -> Builtin.Fp_max (e,s))
      let leq =
        fp_gen_fun ~args:2 ~res:Ty.prop "fp.leq"
          (fun (e,s) -> Builtin.Fp_leq (e,s))
      let lt =
        fp_gen_fun ~args:2 ~res:Ty.prop "fp.lt"
          (fun (e,s) -> Builtin.Fp_lt (e,s))
      let geq =
        fp_gen_fun ~args:2 ~res:Ty.prop "fp.geq"
          (fun (e,s) -> Builtin.Fp_geq (e,s))
      let gt =
        fp_gen_fun ~args:2 ~res:Ty.prop "fp.gt"
          (fun (e,s) -> Builtin.Fp_gt (e,s))
      let eq =
        fp_gen_fun ~args:2 ~res:Ty.prop "fp.eq"
          (fun (e,s) -> Builtin.Fp_eq (e,s))
      let isNormal =
        fp_gen_fun ~args:1 ~res:Ty.prop "fp.isnormal"
          (fun (e,s) -> Builtin.Fp_isNormal (e,s))
      let isSubnormal =
        fp_gen_fun ~args:1 ~res:Ty.prop "fp.issubnormal"
          (fun (e,s) -> Builtin.Fp_isSubnormal (e,s))
      let isZero =
        fp_gen_fun ~args:1 ~res:Ty.prop "fp.iszero"
          (fun (e,s) -> Builtin.Fp_isZero (e,s))
      let isInfinite =
        fp_gen_fun ~args:1 ~res:Ty.prop "fp.isinfinite"
          (fun (e,s) -> Builtin.Fp_isInfinite (e,s))
      let isNaN =
        fp_gen_fun ~args:1 ~res:Ty.prop "fp.isnan"
          (fun (e,s) -> Builtin.Fp_isNaN (e,s))
      let isNegative =
        fp_gen_fun ~args:1 ~res:Ty.prop "fp.isnegative"
          (fun (e,s) -> Builtin.Fp_isNegative (e,s))
      let isPositive =
        fp_gen_fun ~args:1 ~res:Ty.prop "fp.ispositive"
          (fun (e,s) -> Builtin.Fp_isPositive (e,s))
      let to_real =
        fp_gen_fun ~args:1 ~res:Ty.real "fp.to_real"
          (fun (e,s) -> Builtin.To_real (e,s))

      let ieee_format_to_fp =
        with_cache ~cache:(Hashtbl.create 13) (fun ((e,s) as es) ->
            mk' ~builtin:(Builtin.Ieee_format_to_fp (e,s)) "to_fp" [] [Ty.bitv (e+s)] (Ty.float' es)
          )
      let to_fp =
        with_cache ~cache:(Hashtbl.create 13) (fun (e1,s1,e2,s2) ->
            mk' ~builtin:(Builtin.Fp_to_fp (e1,s1,e2,s2)) "to_fp" [] [Ty.roundingMode;Ty.float e1 s1] (Ty.float e2 s2)
          )
      let real_to_fp =
        with_cache ~cache:(Hashtbl.create 13) (fun ((e,s) as es) ->
            mk' ~builtin:(Builtin.Real_to_fp (e,s)) "to_fp" [] [Ty.roundingMode;Ty.real] (Ty.float' es)
          )
      let sbv_to_fp =
        with_cache ~cache:(Hashtbl.create 13) (fun (bv,e,s) ->
            mk' ~builtin:(Builtin.Sbv_to_fp (bv,e,s)) "to_fp" [] [Ty.roundingMode;Ty.bitv bv] (Ty.float e s)
          )
      let ubv_to_fp =
        with_cache ~cache:(Hashtbl.create 13) (fun (bv,e,s) ->
            mk' ~builtin:(Builtin.Ubv_to_fp (bv,e,s)) "to_fp" [] [Ty.roundingMode;Ty.bitv bv] (Ty.float e s)
          )
      let to_ubv =
        with_cache ~cache:(Hashtbl.create 13) (fun (e,s,bv) ->
            mk' ~builtin:(Builtin.To_ubv (bv,e,s)) "fp.to_ubv" [] [Ty.roundingMode;Ty.float e s] (Ty.bitv bv)
          )
      let to_sbv =
        with_cache ~cache:(Hashtbl.create 13) (fun (e,s,bv) ->
            mk' ~builtin:(Builtin.To_sbv (bv,e,s)) "fp.to_sbv" [] [Ty.roundingMode;Ty.float e s] (Ty.bitv bv)
          )

    end

    module String = struct

      let string =
        with_cache ~cache:(Hashtbl.create 13) (fun s ->
            mk' ~builtin:(Builtin.Str s) (Format.asprintf {|"%s"|} s) [] [] Ty.string
          )

      let length =
        mk' ~builtin:Builtin.Str_length "length"
          [] [Ty.string] Ty.int
      let at =
        mk' ~builtin:Builtin.Str_at "at"
          [] [Ty.string; Ty.int] Ty.string
      let to_code =
        mk' ~builtin:Builtin.Str_to_code "to_code"
          [] [Ty.string] Ty.int
      let of_code =
        mk' ~builtin:Builtin.Str_of_code "of_code"
          [] [Ty.int] Ty.string
      let is_digit =
        mk' ~builtin:Builtin.Str_is_digit "is_digit"
          [] [Ty.string] Ty.prop
      let to_int =
        mk' ~builtin:Builtin.Str_to_int "to_int"
          [] [Ty.string] Ty.int
      let of_int =
        mk' ~builtin:Builtin.Str_of_int "of_int"
          [] [Ty.int] Ty.string
      let concat =
        mk' ~builtin:Builtin.Str_concat ~pos:Pretty.Infix "++"
          [] [Ty.string; Ty.string] Ty.string
      let sub =
        mk' ~builtin:Builtin.Str_sub "sub"
          [] [Ty.string; Ty.int; Ty.int] Ty.string
      let index_of =
        mk' ~builtin:Builtin.Str_index_of "index_of"
          [] [Ty.string; Ty.string; Ty.int] Ty.int
      let replace =
        mk' ~builtin:Builtin.Str_replace "replace"
          [] [Ty.string; Ty.string; Ty.string] Ty.string
      let replace_all =
        mk' ~builtin:Builtin.Str_replace_all "replace_all"
          [] [Ty.string; Ty.string; Ty.string] Ty.string
      let replace_re =
        mk' ~builtin:Builtin.Str_replace_re "replace_re"
          [] [Ty.string; Ty.string_reg_lang; Ty.string] Ty.string
      let replace_re_all =
        mk' ~builtin:Builtin.Str_replace_re_all "replace_re_all"
          [] [Ty.string; Ty.string_reg_lang; Ty.string] Ty.string
      let is_prefix =
        mk' ~builtin:Builtin.Str_is_prefix "is_prefix"
          [] [Ty.string; Ty.string] Ty.prop
      let is_suffix =
        mk' ~builtin:Builtin.Str_is_suffix "is_suffix"
          [] [Ty.string; Ty.string] Ty.prop
      let contains =
        mk' ~builtin:Builtin.Str_contains "contains"
          [] [Ty.string; Ty.string] Ty.prop
      let lt =
        mk' ~builtin:Builtin.Str_lexicographic_strict
          ~pos:Pretty.Infix "lt"
          [] [Ty.string; Ty.string] Ty.prop
      let leq =
        mk' ~builtin:Builtin.Str_lexicographic_large
          ~pos:Pretty.Infix "leq"
          [] [Ty.string; Ty.string] Ty.prop
      let in_re =
        mk' ~builtin:Builtin.Str_in_re "in_re"
          [] [Ty.string; Ty.string_reg_lang] Ty.prop

      module Reg_Lang = struct

        let empty =
          mk' ~builtin:Builtin.Re_empty "empty"
            [] [] Ty.string_reg_lang
        let all =
          mk' ~builtin:Builtin.Re_all "all"
            [] [] Ty.string_reg_lang
        let allchar =
          mk' ~builtin:Builtin.Re_allchar "allchar"
            [] [] Ty.string_reg_lang
        let of_string =
          mk' ~builtin:Builtin.Re_of_string "of_string"
            [] [Ty.string] Ty.string_reg_lang
        let range =
          mk' ~builtin:Builtin.Re_range "range"
            [] [Ty.string; Ty.string] Ty.string_reg_lang
        let concat =
          mk' ~builtin:Builtin.Re_concat ~pos:Pretty.Infix "++"
            [] [Ty.string_reg_lang; Ty.string_reg_lang] Ty.string_reg_lang
        let union =
          mk' ~builtin:Builtin.Re_union ~pos:Pretty.Infix "∪"
            [] [Ty.string_reg_lang; Ty.string_reg_lang] Ty.string_reg_lang
        let inter =
          mk' ~builtin:Builtin.Re_inter ~pos:Pretty.Infix "∩"
            [] [Ty.string_reg_lang; Ty.string_reg_lang] Ty.string_reg_lang
        let diff =
          mk' ~builtin:Builtin.Re_diff ~pos:Pretty.Infix "-"
            [] [Ty.string_reg_lang; Ty.string_reg_lang] Ty.string_reg_lang
        let star =
          mk' ~builtin:Builtin.Re_star ~pos:Pretty.Prefix "*"
            [] [Ty.string_reg_lang] Ty.string_reg_lang
        let cross =
          mk' ~builtin:Builtin.Re_cross ~pos:Pretty.Prefix "+"
            [] [Ty.string_reg_lang] Ty.string_reg_lang
        let complement =
          mk' ~builtin:Builtin.Re_complement "complement"
            [] [Ty.string_reg_lang] Ty.string_reg_lang
        let option =
          mk' ~builtin:Builtin.Re_option "option"
            [] [Ty.string_reg_lang] Ty.string_reg_lang
        let power =
          with_cache ~cache:(Hashtbl.create 13) (fun n ->
              mk' ~builtin:(Builtin.Re_power n) (Format.asprintf "power_%d" n)
                [] [Ty.string_reg_lang] Ty.string_reg_lang
            )
        let loop =
          with_cache ~cache:(Hashtbl.create 13) (fun (n1, n2) ->
              mk' ~builtin:(Builtin.Re_loop (n1, n2)) (Format.asprintf "loop_%d_%d" n1 n2)
                [] [Ty.string_reg_lang] Ty.string_reg_lang
            )

      end

    end

  end

  (* Constructors are simply constants *)
  module Cstr = struct
    type t = term_cst
    let hash = Id.hash
    let print = Id.print
    let equal = Id.equal
    let compare = Id.compare
    let get_tag = Id.get_tag
    let get_tag_last = Id.get_tag_last
    let get_tag_list = Id.get_tag_list
    let set_tag = Id.set_tag
    let add_tag = Id.add_tag
    let add_tag_opt = Id.add_tag_opt
    let add_tag_list = Id.add_tag_list
    let unset_tag = Id.unset_tag

    exception Bad_pattern_arity of term_cst * ty list * term list

    let ty ({ id_ty; _ } : t) = id_ty

    let tester c =
      match c.builtin with
      | Builtin.Constructor { adt; case; } ->
        begin match Ty.definition adt with
          | Some Adt { cases; _ } -> cases.(case).tester
          | _ -> assert false
        end
      | _ -> raise (Constructor_expected c)

    let void =
      match define_adt Ty.Const.unit [] [Path.global "void", []] with
      | _, [void, _] -> void
      | _ -> assert false

    let pattern_arity (c : t) ret tys =
      try
        let fun_vars, fun_args, fun_ret = Ty.poly_sig c.id_ty in
        let s = List.fold_left2 Ty.subst_bind Subst.empty fun_vars tys in
        let s = Ty.robinson s fun_ret ret in
        List.map (Ty.subst s) fun_args
      with
      | Ty.Impossible_unification _ -> raise (Wrong_sum_type (c, ret))
      | Invalid_argument _ -> raise (Bad_pattern_arity (c, tys, []))

  end

  (* Record fields are represented as their destructors, i.e. constants *)
  module Field = struct
    type t = term_cst
    let hash = Id.hash
    let print = Id.print
    let equal = Id.equal
    let compare = Id.compare
    let get_tag = Id.get_tag
    let get_tag_last = Id.get_tag_last
    let get_tag_list = Id.get_tag_list
    let set_tag = Id.set_tag
    let add_tag = Id.add_tag
    let add_tag_opt = Id.add_tag_opt
    let add_tag_list = Id.add_tag_list
    let unset_tag = Id.unset_tag

    (* Record field getter *)
    let find ty_c i =
      match Ty.definition ty_c with
      | Some Adt { record = true; cases = [| { dstrs; _ } |]; _ } ->
        begin match dstrs.(i) with
          | Some c -> c
          | None -> assert false
        end
      | _ ->
        raise (Record_type_expected ty_c)

    (* Record creation *)
    let index ty_c f =
      match f.builtin with
      | Builtin.Destructor { adt = ty_d; case = i; field = j; _ } ->
        if Id.equal ty_c ty_d then begin
          assert (i = 0);
          j
        end else
          raise (Wrong_record_type (f, ty_c))
      | _ ->
        raise (Field_expected f)

  end

  (* Binder creation *)
  let mk_bind b body =
    match b with
    | Let_seq []
    | Let_par []
    | Forall ([], [])
    | Exists ([], [])
    | Lambda ([], []) -> body

    | Forall _
    | Exists _ ->
      if not (Ty.(equal prop) (ty body)) then
        raise (Wrong_type (body, Ty.prop));
      mk (Binder (b, body)) Ty.prop
    | Let_seq l | Let_par l ->
      List.iter (fun ((v : Var.t), t) ->
          if not (Ty.equal v.id_ty (ty t)) then raise (Wrong_type (t, v.id_ty))
        ) l;
      mk (Binder (b, body)) (ty body)
    | Lambda (tys, ts) ->
      let res_ty =
        Ty.pi tys
          (Ty.arrow (List.map Var.ty ts) (ty body))
      in
      mk (Binder (b, body)) res_ty

  let lam (tys, ts) body = mk_bind (Lambda (tys, ts)) body
  let all (tys, ts) body = mk_bind (Forall (tys, ts)) body
  let ex (tys, ts) body = mk_bind (Exists (tys, ts)) body
  let letin l body = mk_bind (Let_seq l) body
  let letand l body = mk_bind (Let_par l) body

  (* Substitutions *)
  let rec ty_var_list_subst ty_var_map = function
    | [] -> ty_var_map
    | (v :: r : ty_var list) ->
      ty_var_list_subst (Subst.Var.remove v ty_var_map) r

  let rec term_var_list_subst ty_var_map t_var_map acc = function
    | [] -> List.rev acc, t_var_map
    | (v :: r : term_var list) ->
      let ty = Ty.subst ty_var_map v.id_ty in
      if not (Ty.equal ty v.id_ty) then
        let nv = Var.create v.path ty in
        term_var_list_subst ty_var_map
          (Subst.Var.bind t_var_map v (of_var nv)) (nv :: acc) r
      else
        term_var_list_subst ty_var_map
          (Subst.Var.remove v t_var_map) (v :: acc) r

  let rec subst_aux ~fix ty_var_map t_var_map (t : t) =
    match t.term_descr with
    | Var v ->
      begin match Subst.Var.get v t_var_map with
        | exception Not_found -> t
        | term ->
          if fix
          then subst_aux ~fix ty_var_map t_var_map term
          else term
      end
    | Cst _ -> t
    | App (f, tys, args) ->
      let new_f = subst_aux ~fix ty_var_map t_var_map f in
      let new_tys = List.map (Ty.subst ~fix ty_var_map) tys in
      let new_args = List.map (subst_aux ~fix ty_var_map t_var_map) args in
      if new_f == f &&
         List.for_all2 (==) new_tys tys &&
         List.for_all2 (==) new_args args
      then t
      else apply new_f new_tys new_args
    | Binder (b, body) ->
      let b', ty_var_map, t_var_map = binder_subst ~fix ty_var_map t_var_map b in
      mk_bind b' (subst_aux ~fix ty_var_map t_var_map body)
    | Match (scrutinee, branches) ->
      let scrutinee = subst_aux ~fix ty_var_map t_var_map scrutinee in
      let branches = List.map (branch_subst ~fix ty_var_map t_var_map) branches in
      pattern_match scrutinee branches

  and binder_subst ~fix ty_var_map t_var_map = function
    | Let_seq l ->
      let l, t_var_map = binding_list_subst ~fix ty_var_map t_var_map [] l in
      Let_seq l, ty_var_map, t_var_map
    | Let_par l ->
      let l, t_var_map = binding_list_subst ~fix ty_var_map t_var_map [] l in
      Let_par l, ty_var_map, t_var_map
    | Lambda (tys, ts) ->
      (* term variables in ts may have their types changed by the subst *)
      let ty_var_map = ty_var_list_subst ty_var_map tys in
      let ts, t_var_map = term_var_list_subst ty_var_map t_var_map [] ts in
      Lambda (tys, ts), ty_var_map, t_var_map
    | Exists (tys, ts) ->
      (* term variables in ts may have their types changed by the subst *)
      let ty_var_map = ty_var_list_subst ty_var_map tys in
      let ts, t_var_map = term_var_list_subst ty_var_map t_var_map [] ts in
      Exists (tys, ts), ty_var_map, t_var_map
    | Forall (tys, ts) ->
      (* term variables in ts may have their types changed by the subst *)
      let ty_var_map = ty_var_list_subst ty_var_map tys in
      let ts, t_var_map = term_var_list_subst ty_var_map t_var_map [] ts in
      Forall (tys, ts), ty_var_map, t_var_map

  and binding_list_subst ~fix ty_var_map t_var_map acc = function
    | [] -> List.rev acc, t_var_map
    | ((v, t) :: r : (term_var * term) list) ->
      let t = subst_aux ~fix ty_var_map t_var_map t in
      if Ty.equal (ty t) v.id_ty then begin
        let t_var_map = Subst.Var.remove v t_var_map in
        let acc = (v, t) :: acc in
        binding_list_subst ~fix ty_var_map t_var_map acc r
      end else begin
        let nv = Var.create v.path (ty t) in
        let t_var_map = Subst.Var.bind t_var_map v (of_var nv) in
        let acc = (nv, t) :: acc in
        binding_list_subst ~fix ty_var_map t_var_map acc r
      end

  and branch_subst ~fix ty_var_map t_var_map (pattern, body) =
    let _, l = fv pattern in
    let _, t_var_map = term_var_list_subst ty_var_map t_var_map [] l in
    (subst_aux ~fix ty_var_map t_var_map pattern,
     subst_aux ~fix ty_var_map t_var_map body)

  and subst ?(fix=true) ty_var_map t_var_map t =
    if Subst.is_empty ty_var_map && Subst.is_empty t_var_map then
      t
    else
      subst_aux ~fix ty_var_map t_var_map t

  (* Application typechecking *)
  and instantiate_finalize subst ty =
    let subst = Subst.map (Ty.subst ~fix:true subst) subst in
    Subst.iter Ty.set_wildcard subst;
    Ty.subst subst ty

  and instantiate_term_app subst fun_ty args =
    let rec aux subst fun_ty_args fun_ty_ret args =
      match fun_ty_args, args with
      (* full application *)
      | [], [] ->
        instantiate_finalize subst fun_ty_ret
      (* partial application *)
      | _ :: _, [] ->
        instantiate_finalize subst (Ty.arrow fun_ty_args fun_ty_ret)
      (* over application *)
      | [], arg :: rest ->
        let ret = Ty.of_var (Ty.Var.wildcard ()) in
        let potential_fun_ty = Ty.arrow [ty arg] ret in
        begin match Ty.robinson subst fun_ty_ret potential_fun_ty with
          | subst -> instantiate_term_app subst ret rest
          | exception Ty.Impossible_unification _ ->
            raise (Over_application args)
        end
      (* regular application, carry on *)
      | expected :: fun_ty_args, arg :: args ->
        begin match Ty.robinson subst expected (ty arg) with
          | subst -> aux subst fun_ty_args fun_ty_ret args
          | exception Ty.Impossible_unification _ ->
            raise (Wrong_type (arg, Ty.subst subst expected))
        end
    in
    match args with
    | [] -> instantiate_finalize subst fun_ty
    | _ ->
      let fun_ty_args, fun_ty_ret = Ty.split_arrow fun_ty in
      aux subst fun_ty_args fun_ty_ret args

  and instantiate_ty_app subst fun_ty tys args =
    let exception Bad_arity in
    let rec aux subst fun_ty_vars fun_ty_body tys args =
      match fun_ty_vars, tys with
      (* full type application *)
      | [], [] -> instantiate_term_app subst fun_ty_body args
      (* partial type application *)
      | _ :: _, [] ->
        begin match args with
          | [] -> instantiate_finalize subst (Ty.pi fun_ty_vars fun_ty)
          | _ -> raise Bad_arity
        end
      (* over application
         in prenex polymoprhism, type substitution cannot create Pi
         quantifications (the substitution rhs cannot be Pi _). *)
      | [], _ :: _ ->
        raise Bad_arity
      (* regular application
         we prevent type schemas (i.e. Pi _) from being instantiated
         with polymorphic types to preserve prenex polymorphism.
         The Ty.subst_bind function performs this check. *)
      | ty_var :: fun_ty_vars, ty :: tys ->
        let subst = Ty.subst_bind subst ty_var ty in
        aux subst fun_ty_vars fun_ty_body tys args
    in
    match tys with
    | [] -> instantiate_term_app subst fun_ty args
    | _ ->
      let fun_ty_vars, fun_ty_body = Ty.split_pi fun_ty in
      begin
        try aux subst fun_ty_vars fun_ty_body tys args
        with Bad_arity -> raise (Bad_poly_arity (fun_ty_vars, tys))
      end

  and instantiate fun_ty tys args =
    (*
    Format.eprintf "@[<v 2>inst: %a@ %a@ %a@ @]@."
      Print.ty fun_ty
      (Format.pp_print_list Print.ty) tys
      (Format.pp_print_list (fun fmt t ->
           Format.fprintf fmt "%a: %a" Print.term t Print.ty (ty t)
         )) args;
    *)
    instantiate_ty_app Subst.empty fun_ty tys args

  (* Application *)
  and apply f tys args =
    match tys, args with
    | [], [] -> f
    | _, _ ->
      let ret_ty = instantiate (ty f) tys args in
      mk (App (f, tys, args)) ret_ty

  (* Pattern matching *)
  and pattern_match ?(redundant=(fun _ -> ())) scrutinee branches =
    (* fail on empty pattern matching *)
    begin match branches with
      | [] -> raise Empty_pattern_matching
      | _ -> ()
    end;
    let scrutinee_ty = ty scrutinee in
    (* first,
       unify the type of the scrutinee and all patterns,
       and unify the type of all bodies *)
    let body_ty = Ty.of_var (Ty.Var.wildcard ()) in
    let s = List.fold_left (fun acc (pattern, body) ->
        let acc =
          try Ty.robinson acc scrutinee_ty (ty pattern)
          with Ty.Impossible_unification _ -> raise (Wrong_type (pattern, scrutinee_ty))
        in
        let acc =
          try Ty.robinson acc body_ty (ty body)
          with Ty.Impossible_unification _ -> raise (Wrong_type (body, body_ty))
        in
        acc
      ) Subst.empty branches
    in
    (* Apply the substitution to the scrutinee, patterns and bodies *)
    let () = Subst.iter Ty.set_wildcard s in
    let scrutinee = subst s Subst.empty scrutinee in
    let branches = List.map (fun (pat, body) ->
        (subst s Subst.empty pat, subst s Subst.empty body)
      ) branches in
    (* Check exhaustivity *)
    let redundant_cases, missing_cases =
      Pattern_matching.analyze ~apply (List.map fst branches)
    in
    (* warn about redundant cases *)
    List.iter redundant redundant_cases;
    (* *)
    match missing_cases with
    | [] ->
      (* Build the pattern matching *)
      mk (Match (scrutinee, branches)) body_ty
    | _ :: _ ->
      (* Partial matches are not allowed *)
      raise (Partial_pattern_match missing_cases)


  (* Wrappers around application *)

  let apply_cst (c : term_cst) tys args =
    apply (of_cst c) tys args

  let apply_cstr (c : Cstr.t) tys args =
    (* Format.printf "apply_cstr: %a (%a)(%a)@."
      Cstr.print c
      (Format.pp_print_list ~pp_sep:(Print.return ", ") Ty.print) tys
      (Format.pp_print_list ~pp_sep:(Print.return ", ") print) args; *)
    apply (of_cst c) tys args

  let apply_field (f : Field.t) t =
    let f_ty_vars, _ = Ty.split_pi f.id_ty in
    let tys =
      init_list (List.length f_ty_vars)
        (fun _ -> Ty.of_var (Ty.Var.wildcard ()))
    in
    apply (of_cst f) tys [t]

  (* ADT constructor tester *)
  let cstr_tester c t =
    let tester = Cstr.tester c in
    let tester_ty_vars, _ = Ty.split_pi tester.id_ty in
    let ty_args = init_list
        (List.length tester_ty_vars)
        (fun _ -> Ty.of_var (Ty.Var.wildcard ()))
    in
    apply_cst tester ty_args [t]

  (* Record creation *)
  let build_record_fields ty_c l =
    let n =
      match Ty.definition ty_c with
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
      begin match f.builtin with
        | Builtin.Destructor { adt = ty_c; cstr = c; _ } when Ty.is_record ty_c ->
          let fields = build_record_fields ty_c l in
          (* Check that all fields are indeed present, and create the list
             of term arguments *)
          let t_args = Array.to_list @@ Array.mapi (fun i o ->
              match o with
              | None -> missing ty_c i
              | Some v -> v
            ) fields in
          (* Create type wildcard to be unified during application. *)
          let c_ty_vars, _ = Ty.split_pi c.id_ty in
          let ty_args = init_list
              (List.length c_ty_vars)
              (fun _ -> Ty.of_var (Ty.Var.wildcard ()))
          in
          apply_cst c ty_args t_args
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
        apply_field f t
      in
      mk_record aux l

  (* Alt-Ergo's semantic triggers *)
  let in_interval t (b1, b2) t1 t2 =
    apply_cst (Const.in_interval (b1, b2)) [] [t; t1; t2]

  let maps_to tv t =
    let ntv = of_var tv in
    let tv_ty = ty ntv in
    let t_ty = ty t in
    apply_cst Const.maps_to [tv_ty; t_ty] [ntv; t]

  (* typing annotations *)
  let ensure t ty =
    match Ty.robinson Subst.empty ty t.term_ty with
    | s ->
      Subst.iter Ty.set_wildcard s;
      subst s Subst.empty t
    | exception Ty.Impossible_unification _ ->
      raise (Wrong_type (t, ty))

  (* coercion *)
  let coerce dst_ty t =
    let src_ty = ty t in
    apply_cst Const.coerce [src_ty; dst_ty] [t]

  (* Common constructions *)
  let void = apply_cst Cstr.void [] []

  let _true = apply_cst Const._true [] []
  let _false = apply_cst Const._false [] []

  let eqs = function
    | [] -> apply_cst (Const.eqs 0) [] []
    | (h :: _) as l -> apply_cst (Const.eqs (List.length l)) [ty h] l

  let eq a b = eqs [a; b]

  let distinct = function
    | [] -> apply_cst (Const.distinct 0) [] []
    | (h :: _) as l -> apply_cst (Const.distinct (List.length l)) [ty h] l

  let neq a b = distinct [a; b]

  let neg x = apply_cst Const.neg [] [x]

  let _and l = apply_cst (Const._and (List.length l)) [] l

  let _or l = apply_cst (Const._or (List.length l)) [] l

  let nand p q = apply_cst Const.nand [] [p; q]

  let nor p q = apply_cst Const.nor [] [p; q]

  let xor p q = apply_cst Const.xor [] [p; q]

  let imply p q = apply_cst Const.imply [] [p; q]

  let implied p q = apply_cst Const.implied [] [p; q]

  let equiv p q = apply_cst Const.equiv [] [p; q]

  let int s = apply_cst (Const.Int.int s) [] []
  let rat s = apply_cst (Const.Rat.rat s) [] []
  let real s = apply_cst (Const.Real.real s) [] []

  (* arithmetic *)
  module Int = struct
    let mk = int
    let div' = Const.Int.div_e
    let rem' = Const.Int.rem_e
    let minus t = apply_cst Const.Int.minus [] [t]
    let add a b = apply_cst Const.Int.add [] [a; b]
    let sub a b = apply_cst Const.Int.sub [] [a; b]
    let mul a b = apply_cst Const.Int.mul [] [a; b]
    let pow a b = apply_cst Const.Int.pow [] [a; b]
    let div a b = apply_cst Const.Int.div_e [] [a; b]
    let rem a b = apply_cst Const.Int.rem_e [] [a; b]
    let div_e a b = apply_cst Const.Int.div_e [] [a; b]
    let div_t a b = apply_cst Const.Int.div_t [] [a; b]
    let div_f a b = apply_cst Const.Int.div_f [] [a; b]
    let rem_e a b = apply_cst Const.Int.rem_e [] [a; b]
    let rem_t a b = apply_cst Const.Int.rem_t [] [a; b]
    let rem_f a b = apply_cst Const.Int.rem_f [] [a; b]
    let abs a = apply_cst Const.Int.abs [] [a]
    let lt a b = apply_cst Const.Int.lt [] [a; b]
    let le a b = apply_cst Const.Int.le [] [a; b]
    let gt a b = apply_cst Const.Int.gt [] [a; b]
    let ge a b = apply_cst Const.Int.ge [] [a; b]
    let floor a = apply_cst Const.Int.floor [] [a]
    let ceiling a = apply_cst Const.Int.ceiling [] [a]
    let truncate a = apply_cst Const.Int.truncate [] [a]
    let round a = apply_cst Const.Int.round [] [a]
    let is_int a = apply_cst Const.Int.is_int [] [a]
    let is_rat a = apply_cst Const.Int.is_rat [] [a]
    let to_int t = coerce Ty.int t
    let to_rat t = coerce Ty.rat t
    let to_real t = coerce Ty.real t
    let divisible s t = apply_cst Const.Int.divisible [] [int s; t]
  end

  module Rat = struct
    let mk = rat
    let minus t = apply_cst Const.Rat.minus [] [t]
    let add a b = apply_cst Const.Rat.add [] [a; b]
    let sub a b = apply_cst Const.Rat.sub [] [a; b]
    let mul a b = apply_cst Const.Rat.mul [] [a; b]
    let div a b = apply_cst Const.Rat.div [] [a; b]
    let div_e a b = apply_cst Const.Rat.div_e [] [a; b]
    let div_t a b = apply_cst Const.Rat.div_t [] [a; b]
    let div_f a b = apply_cst Const.Rat.div_f [] [a; b]
    let rem_e a b = apply_cst Const.Rat.rem_e [] [a; b]
    let rem_t a b = apply_cst Const.Rat.rem_t [] [a; b]
    let rem_f a b = apply_cst Const.Rat.rem_f [] [a; b]
    let lt a b = apply_cst Const.Rat.lt [] [a; b]
    let le a b = apply_cst Const.Rat.le [] [a; b]
    let gt a b = apply_cst Const.Rat.gt [] [a; b]
    let ge a b = apply_cst Const.Rat.ge [] [a; b]
    let floor a = apply_cst Const.Rat.floor [] [a]
    let ceiling a = apply_cst Const.Rat.ceiling [] [a]
    let truncate a = apply_cst Const.Rat.truncate [] [a]
    let round a = apply_cst Const.Rat.round [] [a]
    let is_int a = apply_cst Const.Rat.is_int [] [a]
    let is_rat a = apply_cst Const.Rat.is_rat [] [a]
    let to_int t = coerce Ty.int t
    let to_rat t = coerce Ty.rat t
    let to_real t = coerce Ty.real t
  end

  module Real = struct
    let mk = real
    let div' = Const.Real.div
    let minus t = apply_cst Const.Real.minus [] [t]
    let add a b = apply_cst Const.Real.add [] [a; b]
    let sub a b = apply_cst Const.Real.sub [] [a; b]
    let mul a b = apply_cst Const.Real.mul [] [a; b]
    let pow a b = apply_cst Const.Real.pow [] [a; b]
    let div a b = apply_cst Const.Real.div [] [a; b]
    let div_e a b = apply_cst Const.Real.div_e [] [a; b]
    let div_t a b = apply_cst Const.Real.div_t [] [a; b]
    let div_f a b = apply_cst Const.Real.div_f [] [a; b]
    let rem_e a b = apply_cst Const.Real.rem_e [] [a; b]
    let rem_t a b = apply_cst Const.Real.rem_t [] [a; b]
    let rem_f a b = apply_cst Const.Real.rem_f [] [a; b]
    let lt a b = apply_cst Const.Real.lt [] [a; b]
    let le a b = apply_cst Const.Real.le [] [a; b]
    let gt a b = apply_cst Const.Real.gt [] [a; b]
    let ge a b = apply_cst Const.Real.ge [] [a; b]
    let floor a = apply_cst Const.Real.floor [] [a]
    let floor_to_int a = apply_cst Const.Real.floor_to_int [] [a]
    let ceiling a = apply_cst Const.Real.ceiling [] [a]
    let truncate a = apply_cst Const.Real.truncate [] [a]
    let round a = apply_cst Const.Real.round [] [a]
    let is_int a = apply_cst Const.Real.is_int [] [a]
    let is_rat a = apply_cst Const.Real.is_rat [] [a]
    let to_int t = coerce Ty.int t
    let to_rat t = coerce Ty.rat t
    let to_real t = coerce Ty.real t
  end

  (* Arrays *)
  let match_array_type =
    let src = Ty.Var.mk "_" in
    let dst = Ty.Var.mk "_" in
    let pat = Ty.array (Ty.of_var src) (Ty.of_var dst) in
    (fun t ->
       match Ty.pmatch Subst.empty pat (ty t) with
       | exception Ty.Impossible_matching _ -> raise (Wrong_type (t, pat))
       | s -> begin match Subst.Var.get src s, Subst.Var.get dst s with
           | res -> res
           | exception Not_found -> assert false (* internal error *)
         end
    )

  (* Arrays *)
  module Array = struct

    let const index_ty base =
      apply_cst Const.Array.const [index_ty; ty base] [base]

    let select t idx =
      let src, dst = match_array_type t in
      apply_cst Const.Array.select [src; dst] [t; idx]

    let store t idx value =
      let src, dst = match_array_type t in
      apply_cst Const.Array.store [src; dst] [t; idx; value]

  end

  (* Bitvectors *)
  module Bitv = struct
    let match_bitv_type t =
      match Ty.descr (ty t) with
      | TyApp ({ builtin = Builtin.Bitv i; _ }, _) -> i
      | _ -> raise (Wrong_type (t, Ty.bitv 0))

    let mk s = apply_cst (Const.Bitv.bitv s) [] []

    let concat u v =
      let i = match_bitv_type u in
      let j = match_bitv_type v in
      apply_cst (Const.Bitv.concat (i, j)) [] [u; v]

    let extract i j t =
      let n = match_bitv_type t in
      (* TODO: check that i and j are correct index for a bitv(n) *)
      apply_cst (Const.Bitv.extract (i, j, n)) [] [t]

    let repeat k t =
      let n = match_bitv_type t in
      apply_cst (Const.Bitv.repeat (k, n)) [] [t]

    let zero_extend k t =
      let n = match_bitv_type t in
      apply_cst (Const.Bitv.zero_extend (k, n)) [] [t]

    let sign_extend k t =
      let n = match_bitv_type t in
      apply_cst (Const.Bitv.sign_extend (k, n)) [] [t]

    let rotate_right k t =
      let n = match_bitv_type t in
      apply_cst (Const.Bitv.rotate_right (k, n)) [] [t]

    let rotate_left k t =
      let n = match_bitv_type t in
      apply_cst (Const.Bitv.rotate_left (k, n)) [] [t]

    let not t =
      let n = match_bitv_type t in
      apply_cst (Const.Bitv.not n) [] [t]

    let and_ u v =
      let n = match_bitv_type u in
      apply_cst (Const.Bitv.and_ n) [] [u; v]

    let or_ u v =
      let n = match_bitv_type u in
      apply_cst (Const.Bitv.or_ n) [] [u; v]

    let nand u v =
      let n = match_bitv_type u in
      apply_cst (Const.Bitv.nand n) [] [u; v]

    let nor u v =
      let n = match_bitv_type u in
      apply_cst (Const.Bitv.nor n) [] [u; v]

    let xor u v =
      let n = match_bitv_type u in
      apply_cst (Const.Bitv.xor n) [] [u; v]

    let xnor u v =
      let n = match_bitv_type u in
      apply_cst (Const.Bitv.xnor n) [] [u; v]

    let comp u v =
      let n = match_bitv_type u in
      apply_cst (Const.Bitv.comp n) [] [u; v]

    let neg t =
      let n = match_bitv_type t in
      apply_cst (Const.Bitv.neg n) [] [t]

    let add u v =
      let n = match_bitv_type u in
      apply_cst (Const.Bitv.add n) [] [u; v]

    let sub u v =
      let n = match_bitv_type u in
      apply_cst (Const.Bitv.sub n) [] [u; v]

    let mul u v =
      let n = match_bitv_type u in
      apply_cst (Const.Bitv.mul n) [] [u; v]

    let udiv u v =
      let n = match_bitv_type u in
      apply_cst (Const.Bitv.udiv n) [] [u; v]

    let urem u v =
      let n = match_bitv_type u in
      apply_cst (Const.Bitv.urem n) [] [u; v]

    let sdiv u v =
      let n = match_bitv_type u in
      apply_cst (Const.Bitv.sdiv n) [] [u; v]

    let srem u v =
      let n = match_bitv_type u in
      apply_cst (Const.Bitv.srem n) [] [u; v]

    let smod u v =
      let n = match_bitv_type u in
      apply_cst (Const.Bitv.smod n) [] [u; v]

    let shl u v =
      let n = match_bitv_type u in
      apply_cst (Const.Bitv.shl n) [] [u; v]

    let lshr u v =
      let n = match_bitv_type u in
      apply_cst (Const.Bitv.lshr n) [] [u; v]

    let ashr u v =
      let n = match_bitv_type u in
      apply_cst (Const.Bitv.ashr n) [] [u; v]

    let ult u v =
      let n = match_bitv_type u in
      apply_cst (Const.Bitv.ult n) [] [u; v]

    let ule u v =
      let n = match_bitv_type u in
      apply_cst (Const.Bitv.ule n) [] [u; v]

    let ugt u v =
      let n = match_bitv_type u in
      apply_cst (Const.Bitv.ugt n) [] [u; v]

    let uge u v =
      let n = match_bitv_type u in
      apply_cst (Const.Bitv.uge n) [] [u; v]

    let slt u v =
      let n = match_bitv_type u in
      apply_cst (Const.Bitv.slt n) [] [u; v]

    let sle u v =
      let n = match_bitv_type u in
      apply_cst (Const.Bitv.sle n) [] [u; v]

    let sgt u v =
      let n = match_bitv_type u in
      apply_cst (Const.Bitv.sgt n) [] [u; v]

    let sge u v =
      let n = match_bitv_type u in
      apply_cst (Const.Bitv.sge n) [] [u; v]

  end

  module Float = struct
    (* Floats *)
    let match_float_type t =
      match Ty.descr (ty t) with
      | TyApp ({ builtin = Builtin.Float (e,s); _ }, _) -> (e,s)
      | _ -> raise (Wrong_type (t, Ty.float 0 0))

    let fp sign exp significand =
      let e = Bitv.match_bitv_type exp in
      let s = Bitv.match_bitv_type significand in
      apply_cst (Const.Float.fp (e, s+1)) [] [sign; exp; significand]

    let roundNearestTiesToEven = apply_cst Const.Float.roundNearestTiesToEven [] []
    let roundNearestTiesToAway = apply_cst Const.Float.roundNearestTiesToAway [] []
    let roundTowardPositive = apply_cst Const.Float.roundTowardPositive [] []
    let roundTowardNegative = apply_cst Const.Float.roundTowardNegative [] []
    let roundTowardZero = apply_cst Const.Float.roundTowardZero [] []

    let plus_infinity e s = apply_cst (Const.Float.plus_infinity (e,s)) [] []
    let minus_infinity e s = apply_cst (Const.Float.minus_infinity (e,s)) [] []
    let plus_zero e s = apply_cst (Const.Float.plus_zero (e,s)) [] []
    let minus_zero e s = apply_cst (Const.Float.minus_zero (e,s)) [] []
    let nan e s = apply_cst (Const.Float.nan (e,s)) [] []
    let abs x =
      let es = match_float_type x in
      apply_cst (Const.Float.abs es) [] [x]
    let neg x =
      let es = match_float_type x in
      apply_cst (Const.Float.neg es) [] [x]
    let add rm x y =
      let es = match_float_type x in
      apply_cst (Const.Float.add es) [] [rm;x;y]
    let sub rm x y =
      let es = match_float_type x in
      apply_cst (Const.Float.sub es) [] [rm;x;y]
    let mul rm x y =
      let es = match_float_type x in
      apply_cst (Const.Float.mul es) [] [rm;x;y]
    let div rm x y =
      let es = match_float_type x in
      apply_cst (Const.Float.div es) [] [rm;x;y]
    let fma rm x y z =
      let es = match_float_type x in
      apply_cst (Const.Float.fma es) [] [rm;x;y;z]
    let sqrt rm x =
      let es = match_float_type x in
      apply_cst (Const.Float.sqrt es) [] [rm;x]
    let rem x y =
      let es = match_float_type x in
      apply_cst (Const.Float.rem es) [] [x;y]
    let roundToIntegral rm x =
      let es = match_float_type x in
      apply_cst (Const.Float.roundToIntegral es) [] [rm;x]
    let min' = Const.Float.min
    let min x y =
      let es = match_float_type x in
      apply_cst (Const.Float.min es) [] [x;y]
    let max' = Const.Float.max
    let max x y =
      let es = match_float_type x in
      apply_cst (Const.Float.max es) [] [x;y]
    let leq x y =
      let es = match_float_type x in
      apply_cst (Const.Float.leq es) [] [x;y]
    let lt x y =
      let es = match_float_type x in
      apply_cst (Const.Float.lt es) [] [x;y]
    let geq x y =
      let es = match_float_type x in
      apply_cst (Const.Float.geq es) [] [x;y]
    let gt x y =
      let es = match_float_type x in
      apply_cst (Const.Float.gt es) [] [x;y]
    let eq x y =
      let es = match_float_type x in
      apply_cst (Const.Float.eq es) [] [x;y]
    let isNormal x =
      let es = match_float_type x in
      apply_cst (Const.Float.isNormal es) [] [x]
    let isSubnormal x =
      let es = match_float_type x in
      apply_cst (Const.Float.isSubnormal es) [] [x]
    let isZero x =
      let es = match_float_type x in
      apply_cst (Const.Float.isZero es) [] [x]
    let isInfinite x =
      let es = match_float_type x in
      apply_cst (Const.Float.isInfinite es) [] [x]
    let isNaN x =
      let es = match_float_type x in
      apply_cst (Const.Float.isNaN es) [] [x]
    let isNegative x =
      let es = match_float_type x in
      apply_cst (Const.Float.isNegative es) [] [x]
    let isPositive x =
      let es = match_float_type x in
      apply_cst (Const.Float.isPositive es) [] [x]
    let to_real x =
      let es = match_float_type x in
      apply_cst (Const.Float.to_real es) [] [x]
    let ieee_format_to_fp e s bv =
      apply_cst (Const.Float.ieee_format_to_fp (e,s)) [] [bv]
    let to_fp e2 s2 rm x =
      let (e1,s1) = match_float_type x in
      apply_cst (Const.Float.to_fp (e1,s1,e2,s2)) [] [rm;x]
    let real_to_fp e s rm r =
      apply_cst (Const.Float.real_to_fp (e,s)) [] [rm;r]
    let sbv_to_fp e s rm bv =
      let n = Bitv.match_bitv_type bv in
      apply_cst (Const.Float.sbv_to_fp (n,e,s)) [] [rm;bv]
    let ubv_to_fp e s rm bv =
      let n = Bitv.match_bitv_type bv in
      apply_cst (Const.Float.ubv_to_fp (n,e,s)) [] [rm;bv]
    let to_ubv' m (e,s) = Const.Float.to_ubv (e, s, m)
    let to_ubv m rm x =
      let (e,s) = match_float_type x in
      apply_cst (Const.Float.to_ubv (e,s,m)) [] [rm;x]
    let to_sbv' m (e,s) = Const.Float.to_sbv (e, s, m)
    let to_sbv m rm x =
      let (e,s) = match_float_type x in
      apply_cst (Const.Float.to_sbv (e,s,m)) [] [rm;x]
  end

  module String = struct

    let of_ustring s = apply_cst (Const.String.string s) [] []
    let length s = apply_cst Const.String.length [] [s]
    let at s i = apply_cst Const.String.at [] [s; i]
    let is_digit s = apply_cst Const.String.is_digit [] [s]
    let to_code s = apply_cst Const.String.to_code [] [s]
    let of_code i = apply_cst Const.String.of_code [] [i]
    let to_int s = apply_cst Const.String.to_int [] [s]
    let of_int i = apply_cst Const.String.of_int [] [i]
    let concat s s' = apply_cst Const.String.concat [] [s;s']
    let sub s i n = apply_cst Const.String.sub [] [s; i; n]
    let index_of s s' i = apply_cst Const.String.index_of [] [s; s'; i]
    let replace s pat by = apply_cst Const.String.replace [] [s; pat; by]
    let replace_all s pat by = apply_cst Const.String.replace_all [] [s; pat; by]
    let replace_re s pat by = apply_cst Const.String.replace_re [] [s; pat; by]
    let replace_re_all s pat by = apply_cst Const.String.replace_re_all [] [s; pat; by]
    let is_prefix s s' = apply_cst Const.String.is_prefix [] [s; s']
    let is_suffix s s' = apply_cst Const.String.is_suffix [] [s; s']
    let contains s s' = apply_cst Const.String.contains [] [s; s']
    let lt s s' = apply_cst Const.String.lt [] [s; s']
    let leq s s' = apply_cst Const.String.leq [] [s; s']
    let in_re s re = apply_cst Const.String.in_re [] [s; re]

    module RegLan = struct
      let empty = apply_cst Const.String.Reg_Lang.empty [] []
      let all = apply_cst Const.String.Reg_Lang.all [] []
      let allchar = apply_cst Const.String.Reg_Lang.allchar [] []
      let of_string s = apply_cst Const.String.Reg_Lang.of_string [] [s]
      let range s s' = apply_cst Const.String.Reg_Lang.range [] [s; s']
      let concat re re' = apply_cst Const.String.Reg_Lang.concat [] [re; re']
      let union re re' = apply_cst Const.String.Reg_Lang.union [] [re; re']
      let inter re re' = apply_cst Const.String.Reg_Lang.inter [] [re; re']
      let diff re re' = apply_cst Const.String.Reg_Lang.diff [] [re; re']
      let star re = apply_cst Const.String.Reg_Lang.star [] [re]
      let cross re = apply_cst Const.String.Reg_Lang.cross [] [re]
      let complement re = apply_cst Const.String.Reg_Lang.complement [] [re]
      let option re = apply_cst Const.String.Reg_Lang.option [] [re]
      let power n re = apply_cst (Const.String.Reg_Lang.power n) [] [re]
      let loop n1 n2 re = apply_cst (Const.String.Reg_Lang.loop (n1, n2)) [] [re]
    end

  end

  (* If-then-else *)

  let ite cond t_then t_else =
    let ty = ty t_then in
    apply_cst Const.ite [ty] [cond; t_then; t_else]

  (* Let-bindings *)

  let bind v t =
    let () = Id.set_tag v Tags.bound t in
    of_var v

end
