
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Type definitions *)
(* ************************************************************************* *)

(* private aliases *)
type hash = int
type index = int
type 'a tag = 'a Tag.t

(* Extensible variant type for builtin operations *)
type builtin = ..

(* Type for first order types *)
type ttype = Type

(* Identifiers, parametrized by the kind of the type of the variable and
   the lengths of the expected arguments lists *)
type 'ty id = {
  ty            : 'ty;
  name          : string;
  index         : index; (** unique *)
  builtin       : builtin;
  mutable tags  : Tag.map;
}

(* The type of functions *)
type ('ttype, 'ty) function_type = {
  fun_vars : 'ttype id list; (* prenex forall *)
  fun_args : 'ty list;
  fun_ret : 'ty;
}

(* Representation of types *)
type ty_var = ttype id

and ty_const = (unit, ttype) function_type id

and ty_descr =
  | Var of ty_var
  | App of ty_const * ty list

and ty = {
  as_ : ty_var option;
  mutable descr : ty_descr;
  mutable hash : hash; (* lazy hash *)
  mutable tags : Tag.map;
}

(* Terms and formulas *)
type term_var = ty id

and term_const = (ttype, ty) function_type id

and pattern = term

and term_descr =
  | Var of term_var
  | App of term_const * ty list * term list
  | Binder of binder * term
  | Match of term * (pattern * term) list

and binder =
  | Exists of ty_var list * term_var list
  | Forall of ty_var list * term_var list
  | Letin  of (term_var * term) list

and term = {
  ty : ty;
  descr : term_descr;
  mutable hash : hash;
  mutable tags : Tag.map;
}

(* Alias for dolmen_loop and others who allow to have different types
   for terms and formulas. *)
type formula = term

(* Builtins *)
(* ************************************************************************* *)

type builtin += Base | Wildcard
type builtin += Prop | Unit | Univ
type builtin += Coercion
type builtin +=
  | True | False
  | Equal | Distinct
  | Neg | And | Or
  | Nand | Nor | Xor
  | Imply | Equiv
type builtin += Ite
type builtin +=
  | Tester of {
      cstr : term_const;
    }
  | Constructor of {
      adt : ty_const;
      case : int;
    }
  | Destructor of {
      adt : ty_const;
      cstr : term_const;
      case : int;
      field: int;
    }

(* arithmetic *)
type builtin +=
  | Int | Integer of string
  | Rat | Rational of string
  | Real | Decimal of string
  | Lt | Leq | Gt | Geq
  | Minus | Add | Sub | Mul
  | Div
  | Div_e | Modulo_e
  | Div_t | Modulo_t
  | Div_f | Modulo_f
  | Abs | Divisible
  | Is_int | Is_rat
  | Floor | Ceiling | Truncate | Round

(* arrays *)
type builtin +=
  | Array | Store | Select

(* Bitvectors *)
type builtin +=
  | Bitv of int
  | Bitvec of string
  | Bitv_concat
  | Bitv_extract of int * int
  | Bitv_repeat
  | Bitv_zero_extend
  | Bitv_sign_extend
  | Bitv_rotate_right of int
  | Bitv_rotate_left of int
  | Bitv_not | Bitv_and | Bitv_or
  | Bitv_nand | Bitv_nor
  | Bitv_xor | Bitv_xnor
  | Bitv_comp
  | Bitv_neg | Bitv_add | Bitv_sub | Bitv_mul
  | Bitv_udiv | Bitv_urem
  | Bitv_sdiv | Bitv_srem | Bitv_smod
  | Bitv_shl | Bitv_lshr | Bitv_ashr
  | Bitv_ult | Bitv_ule
  | Bitv_ugt | Bitv_uge
  | Bitv_slt | Bitv_sle
  | Bitv_sgt | Bitv_sge

(* Floats *)
type builtin +=
  | Float of int * int
  | RoundingMode
  | Fp of int * int
  | RoundNearestTiesToEven
  | RoundNearestTiesToAway
  | RoundTowardPositive
  | RoundTowardNegative
  | RoundTowardZero
  | Plus_infinity of int * int
  | Minus_infinity of int * int
  | Plus_zero of int * int
  | Minus_zero of int * int
  | NaN of int * int
  | Fp_abs of int * int
  | Fp_neg of int * int
  | Fp_add of int * int
  | Fp_sub of int * int
  | Fp_mul of int * int
  | Fp_div of int * int
  | Fp_fma of int * int
  | Fp_sqrt of int * int
  | Fp_rem of int * int
  | Fp_roundToIntegral  of int * int
  | Fp_min of int * int
  | Fp_max of int * int
  | Fp_leq of int * int
  | Fp_lt of int * int
  | Fp_geq of int * int
  | Fp_gt of int * int
  | Fp_eq of int * int
  | Fp_isNormal of int * int
  | Fp_isSubnormal of int * int
  | Fp_isZero of int * int
  | Fp_isInfinite of int * int
  | Fp_isNaN of int * int
  | Fp_isNegative of int * int
  | Fp_isPositive of int * int
  | Ieee_format_to_fp of int * int
  | Fp_to_fp of int * int * int * int
  | Real_to_fp of int * int
  | Sbv_to_fp of int * int * int
  | Ubv_to_fp of int * int * int
  | To_ubv of int * int * int
  | To_sbv of int * int * int
  | To_real of int * int

(* Strings *)
type builtin +=
  | String
  | Str of string
  | Str_length
  | Str_at
  | Str_to_code
  | Str_of_code
  | Str_is_digit
  | Str_to_int
  | Str_of_int
  | Str_concat
  | Str_sub
  | Str_index_of
  | Str_replace
  | Str_replace_all
  | Str_replace_re
  | Str_replace_re_all
  | Str_is_prefix
  | Str_is_suffix
  | Str_contains
  | Str_lexicographic_strict
  | Str_lexicographic_large
  | Str_in_re

(* String Regular languages *)
type builtin +=
  | String_RegLan
  | Re_empty
  | Re_all
  | Re_allchar
  | Re_of_string
  | Re_range
  | Re_concat
  | Re_union
  | Re_inter
  | Re_star
  | Re_cross
  | Re_complement
  | Re_diff
  | Re_option
  | Re_power of int
  | Re_loop of int * int


(* Exceptions *)
(* ************************************************************************* *)

exception Bad_ty_arity of ty_const * ty list
exception Bad_term_arity of term_const * ty list * term list

exception Filter_failed_ty of string * ty * string
exception Filter_failed_term of string * term * string

exception Type_already_defined of ty_const
exception Record_type_expected of ty_const


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

  let named = Tag.create ()
  let triggers = Tag.create ()

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

  let ttype fmt Type = Format.fprintf fmt "Type"

  let pp_index fmt (v : _ id) = Format.fprintf fmt "/%d" v.index

  let id fmt (v : _ id) =
    match Tag.last v.tags name with
    | Some (Pretty.Exact s | Pretty.Renamed s) -> Format.fprintf fmt "%s" s
    | None ->
      if !print_index then
        Format.fprintf fmt "%s%a" v.name (Fmt.styled (`Fg (`Hi `Black)) pp_index) v
      else
        Format.fprintf fmt "%s" v.name

  let rec ty_descr fmt (descr : ty_descr) =
    match descr with
    | Var v -> id fmt v
    | App (f, []) -> id fmt f
    | App (f, l) ->
      begin match Tag.last f.tags pos with
        | Some Pretty.Prefix ->
          Format.fprintf fmt "@[<hov 2>%a %a@]"
            id f (Format.pp_print_list ~pp_sep:(return "") ty) l
        | Some Pretty.Infix when List.length l >= 2 ->
          let pp_sep fmt () = Format.fprintf fmt " %a@ " id f in
          Format.fprintf fmt "@[<hov 2>%a@]" (Format.pp_print_list ~pp_sep ty) l
        | None | Some Pretty.Infix ->
          Format.fprintf fmt "@[<hov 2>%a(%a)@]"
            id f (Format.pp_print_list ~pp_sep:(return ",@ ") ty) l
      end

  and ty fmt (t: ty) =
    match t.as_ with
    | None -> ty_descr fmt t.descr
    | Some v -> Format.fprintf fmt "(%a@ as@ %a)" ty_descr t.descr id v

  let params fmt = function
    | [] -> ()
    | l -> Format.fprintf fmt "∀ @[<hov>%a@].@ "
             (Format.pp_print_list ~pp_sep:(return ",@ ") id) l

  let signature print fmt f =
    match f.fun_args with
    | [] -> Format.fprintf fmt "@[<hov 2>%a%a@]" params f.fun_vars print f.fun_ret
    | l -> Format.fprintf fmt "@[<hov 2>%a%a ->@ %a@]" params f.fun_vars
             (Format.pp_print_list ~pp_sep:(return " ->@ ") print) l print f.fun_ret

  let fun_ty = signature ty
  let fun_ttype = signature ttype

  let id_pretty fmt (v : _ id) =
    match Tag.last v.tags pos with
    | None -> ()
    | Some Pretty.Infix -> Format.fprintf fmt "(%a)" id v
    | Some Pretty.Prefix -> Format.fprintf fmt "[%a]" id v

  let id_type print fmt (v : _ id) =
    Format.fprintf fmt "@[<hov 2>%a%a :@ %a@]" id v id_pretty v print v.ty

  let ty_var fmt id = id_type ttype fmt id
  let term_var fmt id = id_type ty fmt id

  let ty_const fmt id = id_type fun_ttype fmt id
  let term_const fmt id = id_type fun_ty fmt id

  let binder_name fmt = function
    | Exists _ -> Format.fprintf fmt "∃"
    | Forall _ -> Format.fprintf fmt "∀"
    | Letin _  -> Format.fprintf fmt "let"

  let binder_sep fmt = function
    | Exists _
    | Forall _ -> Format.fprintf fmt "."
    | Letin _  -> Format.fprintf fmt "in"

  let rec term fmt t = match t.descr with
    | Var v -> id fmt v
    | App (f, [], []) -> id fmt f
    | App (f, tys, args) -> term_app fmt f tys args
    | Binder (b, body) ->
      Format.fprintf fmt "@[<hv 2>%a%a@ %a@]" binder b binder_sep b term body
    | Match (scrutinee, branches) ->
      Format.fprintf fmt "@[<hv 2>match %a with@ %a@]"
        term scrutinee
        (Format.pp_print_list ~pp_sep:(return "@ ") branch) branches

  and term_app fmt (f : _ id) tys args =
    match Tag.last f.tags pos with
    | Some Pretty.Prefix ->
      begin match args with
        | [] -> id fmt f
        | _ ->
          Format.fprintf fmt "@[<hov>%a(%a)@]"
            id f (Format.pp_print_list ~pp_sep:(return ",@ ") term) args
      end
    | Some Pretty.Infix when List.length args >= 2 ->
      let pp_sep fmt () = Format.fprintf fmt " %a@ " id f in
      Format.fprintf fmt "(@[<hov>%a@])" (Format.pp_print_list ~pp_sep term) args
    | None | Some Pretty.Infix ->
      begin match tys, args with
        | _, [] ->
          Format.fprintf fmt "%a(@[<hov>%a@])"
            id f (Format.pp_print_list ~pp_sep:(return ",@ ") ty) tys
        | [], _ ->
          Format.fprintf fmt "%a(@[<hov>%a@])"
            id f (Format.pp_print_list ~pp_sep:(return ",@ ") term) args
        | _ ->
          Format.fprintf fmt "%a(@[<hov>%a%a%a@])" id f
            (Format.pp_print_list ~pp_sep:(return ",@ ") ty) tys
            (return ";@ ") ()
            (Format.pp_print_list ~pp_sep:(return ",@ ") term) args
      end

  and binder fmt b =
    match b with
    | Exists (l, [])
    | Forall (l, []) ->
      Format.fprintf fmt "%a @[<hov>%a@]"
        binder_name b
        (Format.pp_print_list ~pp_sep:(return ",@ ") ty_var) l
    | Exists ([], l)
    | Forall ([], l) ->
      Format.fprintf fmt "%a @[<hov>%a@]"
        binder_name b
        (Format.pp_print_list ~pp_sep:(return ",@ ") term_var) l
    | Exists (tys, ts)
    | Forall (tys, ts) ->
      Format.fprintf fmt "%a @[<hov>%a,@ %a@]"
        binder_name b
        (Format.pp_print_list ~pp_sep:(return ",@ ") ty_var) tys
        (Format.pp_print_list ~pp_sep:(return ",@ ") term_var) ts
    | Letin l ->
      Format.fprintf fmt "%a @[<hv>%a@]"
        binder_name b
        (Format.pp_print_list ~pp_sep:(return ",@ ") binding) l

  and binding fmt (v, t) =
    Format.fprintf fmt "@[<hov 2>%a =@ %a@]" id v term t

  and branch fmt (pattern, body) =
    Format.fprintf fmt "@[<hov 2>| %a@ ->@ %a" term pattern term body

  let iter ~sep pp fmt k =
    let first = ref true in
    k (fun x ->
        if !first then first := false else sep fmt ();
        pp fmt x
      )
end

(* Views *)
(* ************************************************************************* *)

module View = struct

  module Ty = struct
    type t = [
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
      | `App of [
          | `Generic of ty_const
          | `Builtin of builtin
        ] * ty list
    ]

    let view (ty : ty) : t =
      match ty.descr with
      | Var v -> `Var v
      | App (({ builtin; _ } as c), l) ->
        begin match builtin with
          | Int -> `Int
          | Rat -> `Rat
          | Real -> `Real
          | Bitv i -> `Bitv i
          | Float (e, s) -> `Float (e, s)
          | Array -> begin match l with
              | [src; dst] -> `Array (src, dst)
              | _ -> assert false (* not possible *)
            end
          | String -> `String
          | String_RegLan -> `String_reg_lang
          | Base -> `App (`Generic c, l)
          | _ -> `App (`Builtin builtin, l)
        end

  end

end

(* Flags and filters *)
(* ************************************************************************* *)

module Filter = struct

  type status = [
    | `Pass
    | `Warn
    | `Error of string
  ]
  type ty_filter = string * bool ref * (ty_const -> ty list -> status)
  type term_filter = string * bool ref * (term_const -> ty list -> term list -> status)

  let ty : ty_filter tag = Tag.create ()
  let term : term_filter tag = Tag.create ()

  module type S = sig
    val name : string
    val active : bool ref
    val reset : unit -> unit
  end

end


(* Helpers *)
(* ************************************************************************* *)

(* Useful shorthand for chaining comparisons *)
let (<?>) i (cmp, x, y) =
  match i with
  | 0 -> cmp x y
  | _ -> i

(* hash helpers *)
let hash2 x y = Hashtbl.seeded_hash x y
let hash3 x y z = hash2 x (hash2 y z)
let hash4 x y z t = hash2 x (hash3 y z t)

(* option iter *)
let option_iter f = function
  | None -> ()
  | Some x -> f x

(* list hash *)
let hash_list f l =
  let rec aux acc = function
    | [] -> acc
    | x :: r -> aux (Hashtbl.seeded_hash acc (f x)) r
  in
  aux 0 l

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

  (* Usual functions *)
  let hash (v : _ t) = v.index

  let compare v v' = compare v.index v'.index

  let equal v v' = compare v v' = 0

  let print fmt id = Format.pp_print_string fmt id.name

  (* Tags *)
  let tag (id : _ id) k v = id.tags <- Tag.add id.tags k v

  let get_tag (id : _ id) k = Tag.get id.tags k

  let get_tag_last (id : _ id) k = Tag.last id.tags k

  (* Creating ids *)
  let id_counter = ref 0

  let mk ?(builtin=Base) ?(tags=Tag.empty) name ty =
    incr id_counter;
    { name; ty; builtin; tags; index = !id_counter; }

  let const
      ?pos ?name ?builtin ?tags
      ?(ty_filters=[]) ?(term_filters=[])
      cname fun_vars fun_args fun_ret =
    let res = mk ?builtin ?tags cname { fun_vars; fun_args; fun_ret; } in
    (* Add filter tags *)
    List.iter (tag res Filter.ty) ty_filters;
    List.iter (tag res Filter.term) term_filters;
    (* Add pretty printing tags *)
    option_iter (tag res Print.pos) pos;
    option_iter (fun s -> tag res Print.name (Pretty.Exact s)) name;
    (* Return the id *)
    res

  let indexed
      ?pos ?name ?builtin ?tags
      cname fun_vars fun_arg fun_ret =
    let h = Hashtbl.create 13 in
    (fun i ->
       match Hashtbl.find h i with
       | res -> res
       | exception Not_found ->
         let fun_args = replicate i fun_arg in
         let c = const
             ?pos ?name ?builtin ?tags
             cname fun_vars fun_args fun_ret
         in
         Hashtbl.add h i c;
         c
    )

end

(* Maps from pairs of integers *)
(* ************************************************************************* *)

module Mi = Map.Make(struct
    type t = index
    let compare (a : int) b = compare a b
  end)

(* Sets of ids *)
(* ************************************************************************* *)

module FV = struct

  type elt =
    | Ty of ty_var
    | Term of term_var

  type t = elt Mi.t

  let tok v = v.index
  let token = function
    | Ty v -> tok v
    | Term v -> tok v

  (* let mem v s = Mi.mem (tok v) s *)
  (* let get v s = Mi.find (tok v) s *)
  let add x (s : t) = Mi.add (token x) x s
  let del x (s : t) = Mi.remove (tok x) s

  let empty = Mi.empty

  let remove s l =
    List.fold_left (fun acc v -> del v acc) s l

  let diff s s' =
    Mi.merge (fun _ o o' ->
        match o, o' with
        | None, None -> None
        | None, Some _ -> None
        | (Some _ as res), None -> res
        | Some _, Some _ -> None
      ) s s'

  let merge s s' =
    Mi.merge (fun _ o o' ->
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
    Mi.fold aux s ([], [])

end

(* Substitutions *)
(* ************************************************************************* *)

module Subst = struct

  type ('a, 'b) t = ('a * 'b) Mi.t

  (* Usual functions *)
  let empty = Mi.empty

  let is_empty = Mi.is_empty

  let wrap key = function
    | None -> None
    | Some x -> Some (key, x)

  let merge f = Mi.merge (fun _ opt1 opt2 ->
      match opt1, opt2 with
      | None, None -> assert false
      | Some (key, value), None ->
        wrap key @@ f key (Some value) None
      | None, Some (key, value) ->
        wrap key @@ f key None (Some value)
      | Some (key, value1), Some (_key, value2) ->
        wrap key @@ f key (Some value1) (Some value2)
    )

  let iter f = Mi.iter (fun _ (key, value) -> f key value)

  let map f = Mi.map (fun (key, value) -> (key, f value))

  let fold f = Mi.fold (fun _ (key, value) acc -> f key value acc)

  let bindings s = Mi.fold (fun _ (key, value) acc -> (key, value) :: acc) s []

  let filter p = Mi.filter (fun _ (key, value) -> p key value)

  (* Comparisons *)
  let equal f = Mi.equal (fun (_, value1) (_, value2) -> f value1 value2)
  let compare f = Mi.compare (fun (_, value1) (_, value2) -> f value1 value2)
  let hash h s = Mi.fold (fun i (_, value) acc -> Hashtbl.hash (acc, i, h value)) s 1

  let choose m = snd (Mi.choose m)

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
      Print.(iter ~sep:(return ";@ ") aux) (fun k -> Mi.iter (fun x y -> k(x,y)) map)

  let debug print_key print_value fmt map =
    let aux fmt (i, (key, value)) =
      Format.fprintf fmt "@[<hov 2>%d: %a ↦@ %a@]"
        i print_key key print_value value
    in
    Format.fprintf fmt "@[<hv>%a@]"
      Print.(iter ~sep:(return ";@ ") aux) (fun k -> Mi.iter (fun x y -> k(x,y)) map)

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
    let get v s = snd (Mi.find (tok v) s)
    let mem v s = Mi.mem (tok v) s
    let bind s v t = Mi.add (tok v) (v, t) s
    let remove v s = Mi.remove (tok v) s
  end
end

(* Types *)
(* ************************************************************************* *)

module Ty = struct

  (* Std type aliase *)

  type t = ty

  type view = View.Ty.t

  type subst = (ty_var, ty) Subst.t

  type 'a tag = 'a Tag.t

  (* printing *)
  let print = Print.ty

  (* hash function *)
  let rec hash_aux (t : t) = match t.descr with
    | Var v -> hash2 3 (Id.hash v)
    | App (f, args) -> hash3 5 (Id.hash f) (hash_list hash args)

  and hash (t : t) =
    if t.hash < 0 then t.hash <- hash_aux t;
    t.hash

  (* comparison *)
  let discr (t: t) = match t.descr with
    | Var _ -> 1
    | App _ -> 2

  let rec compare (u : t) (v : t) =
    if u == v || u.descr == v.descr then 0 else begin
      let hu = hash u and hv = hash v in
      if hu <> hv then hu - hv (* safe since both are positive *)
      else match u.descr, v.descr with
        | Var v, Var v' -> Id.compare v v'
        | App (f, args), App (f', args') ->
          Id.compare f f'
          <?> (lexicographic compare, args, args')
        | _, _ -> Stdlib.compare (discr u) (discr v)
    end

  let equal u v = compare u v = 0

  (* Set a wildcard/hole to a concrete type

     Wildcard can be set to point to another type by mutating the
     descr field of types (this is the only operation that mutates
     this field).
     In order to be correct, when we set a wildcard v to point at
     another wildcard w, we must remember that, so that when we set
     w to something, we also need to update v. *)

  let wildcard_tbl = ref Subst.empty

  let wildcard_get v =
    match Subst.Var.get v !wildcard_tbl with
    | l -> l
    | exception Not_found -> []

  let wildcard_add v l =
    let l' = wildcard_get v in
    wildcard_tbl := Subst.Var.bind !wildcard_tbl v (List.rev_append l l')

  let set_wildcard v (t: t) =
    let set_descr (t : t) (s: t) = s.descr <- t.descr in
    let l = wildcard_get v in
    List.iter (set_descr t) l;
    match t.descr with
    | Var ({ builtin = Wildcard; _ } as w) -> wildcard_add w l;
    | _ -> ()


  (* Types definitions *)

  type adt_case = {
    cstr : term_const;
    tester : term_const;
    dstrs : term_const option array;
  }

  type def =
    | Abstract
    | Adt of {
        ty : ty_const;
        record : bool;
        cases : adt_case array;
      }

  let definition_tag : def Tag.t = Tag.create ()

  let definition c = Id.get_tag_last c definition_tag

  let is_record c =
    match definition c with
    | Some Adt { record; _ } -> record
    | _ -> false

  let define c d =
    match definition c with
    | None -> Id.tag c definition_tag d
    | Some _ -> raise (Type_already_defined c)

  (* view *)
  let view = View.Ty.view

  (* Tags *)
  let tag (t : t) k v = t.tags <- Tag.add t.tags k v

  let get_tag (t : t) k = Tag.get t.tags k

  let get_tag_last (t : t) k = Tag.last t.tags k

  (* Module for namespacing *)
  module Var = struct
    type t = ty_var
    let tag = Id.tag
    let hash = Id.hash
    let equal = Id.equal
    let compare = Id.compare
    let get_tag = Id.get_tag
    let get_tag_last = Id.get_tag_last
    let mk name = Id.mk name Type
  end

  module Const = struct
    type t = ty_const
    let tag = Id.tag
    let hash = Id.hash
    let equal = Id.equal
    let compare = Id.compare
    let get_tag = Id.get_tag
    let get_tag_last = Id.get_tag_last
    let mk name n = Id.const name [] (replicate n Type) Type
    let arity (c : t) = List.length c.ty.fun_args

    let prop = Id.const ~builtin:Prop "Prop" [] [] Type
    let unit = Id.const ~builtin:Unit "unit" [] [] Type
    let base = Id.const ~builtin:Univ "$i" [] [] Type
    let int = Id.const ~builtin:Int "int" [] [] Type
    let rat = Id.const ~builtin:Rat "rat" [] [] Type
    let real = Id.const ~builtin:Real "real" [] [] Type
    let string = Id.const ~builtin:String "string" [] [] Type
    let string_reg_lang = Id.const ~builtin:String_RegLan "string_reglang" [] [] Type
    let array = Id.const ~builtin:Array "array" [] [Type; Type] Type
    let bitv =
      with_cache ~cache:(Hashtbl.create 13) (fun i ->
          Id.const ~builtin:(Bitv i) (Format.asprintf "Bitv_%d" i) [] [] Type
        )
    let float =
      with_cache ~cache:(Hashtbl.create 13) (fun (e,s) ->
          Id.const ~builtin:(Float(e,s)) (Format.asprintf "FloatingPoint_%d_%d" e s) [] [] Type
        )
    let roundingMode = Id.const ~builtin:RoundingMode "RoundingMode" [] [] Type
  end

  let mk descr = { as_ = None; descr; hash = -1; tags = Tag.empty; }

  let as_ t v = { t with as_ = Some v; }

  let of_var v = mk (Var v)

  let wildcard () =
    let v = Id.mk ~builtin:Wildcard "_" Type in
    let t = of_var v in
    wildcard_add v [t];
    t

  let rec check_filters res f args = function
    | [] -> res
    | (name, active, check) :: r ->
      if !active then match (check f args) with
        | `Pass -> check_filters res f args r
        | `Warn -> check_filters res f args r
        | `Error msg -> raise (Filter_failed_ty (name, res, msg))
      else
        check_filters res f args r

  let apply (f : Const.t) (args : ty list) =
    assert (f.ty.fun_vars = []);
    if List.length args <> List.length f.ty.fun_args then
      raise (Bad_ty_arity (f, args))
    else begin
      let res = mk (App (f, args)) in
      check_filters res f args (Const.get_tag f Filter.ty)
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

  (* Matching *)
  exception Impossible_matching of ty * ty

  let rec pmatch subst (pat : ty) (t : ty) =
    match pat, t with
    | { descr = Var v; _ }, _ ->
      begin match Subst.Var.get v subst with
        | t' ->
          if equal t t' then subst
          else raise (Impossible_matching (pat, t))
        | exception Not_found ->
          Subst.Var.bind subst v t
      end
    | { descr = App (f, f_args); _ },
      { descr = App (g, g_args); _ } ->
      if Id.equal f g then
        List.fold_left2 pmatch subst f_args g_args
      else
        raise (Impossible_matching (pat, t))
    | _ -> raise (Impossible_matching (pat, t))

  (* Unification *)
  exception Impossible_unification of t * t

  let rec follow subst (t : t) =
    match t with
    | { descr = Var v; _ } ->
      begin match Subst.Var.get v subst with
        | t' -> follow subst t'
        | exception Not_found -> t
      end
    | t -> t

  let rec occurs subst l (t : t) =
    match t with
    | { descr = Var v; _ } ->
      List.exists (Id.equal v) l ||
      begin match Subst.Var.get v subst with
        | exception Not_found -> false
        | e -> occurs subst (v :: l) e
      end
    | { descr = App (_, tys); _ } ->
      List.exists (occurs subst l) tys

  let robinson_bind subst m v u =
    if occurs subst [v] u then
      raise (Impossible_unification (m, u))
    else
      Subst.Var.bind subst v u

  let robinson_as subst s t =
    match s.as_ with
    | None -> subst
    | Some v -> robinson_bind subst s v t

  let rec robinson subst s t =
    let subst = robinson_as (robinson_as subst s t) t s in
    let s = follow subst s in
    let t = follow subst t in
    match s, t with
    | ({ descr = Var ({ builtin = Wildcard; _ } as v); _ } as m), u
    | u, ({ descr = Var ({ builtin = Wildcard; _ } as v); _ } as m) ->
      if equal m u then subst else robinson_bind subst m v u
    | ({ descr = Var v; _}, { descr = Var v'; _ }) ->
      if Id.equal v v' then subst
      else raise (Impossible_unification (s, t))
    | { descr = App (f, f_args); _ },
      { descr = App (g, g_args); _ } ->
      if Id.equal f g then
        List.fold_left2 robinson subst f_args g_args
      else
        raise (Impossible_unification (s, t))
    | _, _ ->
      raise (Impossible_unification (s, t))


  (* Substitutions *)
  let rec subst_aux ~fix var_map (t : t) =
    match t.descr with
    | Var v ->
      begin match Subst.Var.get v var_map with
        | exception Not_found -> t
        | ty -> if fix then subst_aux ~fix var_map ty else ty
      end
    | App (f, args) ->
      let new_args = List.map (subst_aux ~fix var_map) args in
      if List.for_all2 (==) args new_args then t
      else apply f new_args

  let subst ?(fix=true) var_map t =
    if Subst.is_empty var_map then t
    else subst_aux ~fix var_map t


  (* free variables *)
  let rec free_vars acc (t : t) = match t.descr with
    | Var v -> FV.add (FV.Ty v) acc
    | App (_, l) -> List.fold_left free_vars acc l

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

  exception Wrong_type of t * ty
  exception Wrong_sum_type of term_const * ty
  exception Wrong_record_type of term_const * ty_const

  exception Field_repeated of term_const
  exception Field_missing of term_const
  exception Field_expected of term_const

  exception Constructor_expected of term_const

  let print = Print.term

  (* Tags *)
  let tag (t : t) k v = t.tags <- Tag.add t.tags k v

  let get_tag (t : t) k = Tag.get t.tags k

  let get_tag_last (t : t) k = Tag.last t.tags k

  (* Hash *)
  let rec hash_aux t =
    match t.descr with
    | Var v -> hash2 3 (Id.hash v)
    | App (f, tys, args) ->
      hash4 5 (Id.hash f) (hash_list Ty.hash tys) (hash_list hash args)
    | Binder (b, body) ->
      hash3 7 (hash_binder b) (hash body)
    | Match (scrutinee, branches) ->
      hash3 11 (hash scrutinee) (hash_branches branches)

  and hash t =
    if t.hash <= 0 then t.hash <- hash_aux t;
    t.hash

  and hash_binder = function
    | Exists (tys, ts) ->
      hash3 3 (hash_list Id.hash tys) (hash_list Id.hash ts)
    | Forall (tys, ts) ->
      hash3 5 (hash_list Id.hash tys) (hash_list Id.hash ts)
    | Letin l ->
      let aux (v, t) = hash2 (Id.hash v) (hash t) in
      hash2 7 (hash_list aux l)

  and hash_branch (pattern, body) =
    hash2 (hash pattern) (hash body)

  and hash_branches l =
    hash_list hash_branch l

  (* Comparison *)
  let discr t =
    match t.descr with
    | Var _ -> 1
    | App _ -> 2
    | Binder _ -> 3
    | Match _ -> 4

  let binder_discr = function
    | Exists _ -> 1
    | Forall _ -> 2
    | Letin _ -> 3

  let rec compare u v =
    if u == v then 0 else begin
      let hu = hash u and hv = hash v in
      if hu <> hv then hu - hv
      else match u.descr, v.descr with
        | Var v1, Var v2 -> Id.compare v1 v2
        | App (f1, tys1, args1), App (f2, tys2, args2) ->
          Id.compare f1 f2
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
    | Exists (tys, ts), Exists (tys', ts') ->
      lexicographic Id.compare tys tys'
      <?> (lexicographic Id.compare, ts, ts')
    | Forall (tys, ts), Forall (tys', ts') ->
      lexicographic Id.compare tys tys'
      <?> (lexicographic Id.compare, ts, ts')
    | Letin l, Letin l' ->
      let aux (v, t) (v', t') = Id.compare v v' <?> (compare, t, t') in
      lexicographic aux l l'
    | _, _ -> (binder_discr b) - (binder_discr b')

  and compare_branch (p, b) (p', b') =
    compare p p' <?> (compare, b, b')

  let equal u v = compare u v = 0

  (* Inspection *)
  let ty { ty; _ } = ty

  (* free variables *)
  let rec free_vars acc (t : t) = match t.descr with
    | Var v -> FV.add (FV.Term v) (Ty.free_vars acc v.ty)
    | App (_, tys, ts) ->
      List.fold_left free_vars (
        List.fold_left Ty.free_vars acc tys
      ) ts
    | Binder ((Exists (tys, ts) | Forall (tys, ts)), body) ->
      let fv = free_vars FV.empty body in
      let fv = FV.remove fv tys in
      let fv = FV.remove fv ts in
      FV.merge fv acc
    | Binder (Letin l, body) ->
      let fv = free_vars FV.empty body in
      let fv = List.fold_right (fun (v, t) acc ->
          let acc = free_vars acc t in
          let acc = FV.del v acc in
          let acc = Ty.free_vars acc v.ty in
          acc
        ) l fv in
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
  let mk_cstr ty_c name i vars args ret =
    Id.const name vars args ret
      ~builtin:(Constructor { adt = ty_c; case = i; })

  let mk_cstr_tester cstr =
    let name = Format.asprintf "is:%a" Print.id cstr in
    Id.const ~builtin:(Tester { cstr })
      name cstr.ty.fun_vars [cstr.ty.fun_ret] Ty.prop

  (* ADT definition *)
  let define_adt_aux ~record ty_const vars l =
    let ty =  Ty.apply ty_const (List.map Ty.of_var vars) in
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
                Id.const name vars [ty] arg_ty
                  ~builtin:(Destructor {
                      adt = ty_const; cstr;
                      case = i; field = j; })
              in
              dstrs.(j) <- Some dstr;
              (arg_ty, Some dstr)
          ) args in
        cases := { Ty.cstr; tester; dstrs; } :: !cases;
        cstr, l'
      ) l in
    assert (not record || List.length !cases = 1);
    Ty.define ty_const (Adt { ty = ty_const; record;
                              cases = Array.of_list @@ List.rev !cases; });
    l'

  let define_adt = define_adt_aux ~record:false

  let define_record ty_const vars l =
    let name = ty_const.name in
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

  (* Exhaustivity check
     TODO: implement this *)
  let check_exhaustivity _ty _pats = ()

  (* Variables *)
  module Var = struct
    type t = term_var
    let tag = Id.tag
    let hash = Id.hash
    let equal = Id.equal
    let compare = Id.compare
    let get_tag = Id.get_tag
    let get_tag_last = Id.get_tag_last
    let ty ({ ty; _ } : t) = ty
    let mk name ty = Id.mk name ty
  end

  (* Constants *)
  module Const = struct
    type t = term_const
    let tag = Id.tag
    let hash = Id.hash
    let equal = Id.equal
    let compare = Id.compare
    let get_tag = Id.get_tag
    let get_tag_last = Id.get_tag_last

    let mk name vars args ret =
      let r = ref ret in
      let b = ref true in
      let args = List.map (fun ty ->
          if !b || not (Ty.equal ty ret) then ty
          else begin
            let v = Ty.Var.mk "'ret" in
            r := Ty.of_var v;
            b := true;
            Ty.as_ ty v
          end
        ) args
      in
      Id.const name vars args !r

    let arity (c : t) =
      List.length c.ty.fun_vars, List.length c.ty.fun_args

    (* Some constants *)
    let _true =
      Id.const ~name:"⊤" ~builtin:True "True" [] [] Ty.prop

    let _false =
      Id.const ~name:"⊥" ~builtin:False "False" [] [] Ty.prop

    let eq =
      let a = Ty.Var.mk "alpha" in
      let a_ty = Ty.of_var a in
      Id.const
        ~pos:Pretty.Infix ~name:"=" ~builtin:Equal
        "Equal" [a] [a_ty; a_ty] Ty.prop

    let eqs =
      let a = Ty.Var.mk "alpha" in
      let a_ty = Ty.of_var a in
      Id.indexed
        ~pos:Pretty.Infix ~name:"=" ~builtin:Equal
        "Equals" [a] a_ty Ty.prop

    let distinct =
      let a = Ty.Var.mk "alpha" in
      let a_ty = Ty.of_var a in
      Id.indexed ~builtin:Distinct "Distinct" [a] a_ty Ty.prop

    let neg = Id.const
        ~pos:Pretty.Prefix ~name:"¬" ~builtin:Neg
        "Neg" [] [Ty.prop] Ty.prop

    let _and = Id.indexed
        ~pos:Pretty.Infix ~name:"∧" ~builtin:And
        "And" [] Ty.prop Ty.prop

    let _or = Id.indexed
        ~pos:Pretty.Infix ~name:"∨" ~builtin:Or
        "Or" [] Ty.prop Ty.prop

    let nand = Id.const
        ~pos:Pretty.Infix ~name:"⊼" ~builtin:Nand
        "Nand" [] [Ty.prop; Ty.prop] Ty.prop

    let nor = Id.const
        ~pos:Pretty.Infix ~name:"V" ~builtin:Nor
        "or" [] [Ty.prop; Ty.prop] Ty.prop

    let xor = Id.const
        ~pos:Pretty.Infix ~name:"⊻" ~builtin:Xor
        "Xor" [] [Ty.prop; Ty.prop] Ty.prop

    let imply = Id.const
        ~pos:Pretty.Infix ~name:"⇒" ~builtin:Imply
        "Imply" [] [Ty.prop; Ty.prop] Ty.prop

    let equiv = Id.const
        ~pos:Pretty.Infix ~name:"⇔" ~builtin:Equiv
        "Equiv" [] [Ty.prop; Ty.prop] Ty.prop

    let ite =
      let a = Ty.Var.mk "alpha" in
      let a_ty = Ty.of_var a in
      Id.const
        ~name:"ite" ~builtin:Ite
        "Ite" [a] [Ty.prop; a_ty; a_ty] a_ty

    let select =
      let a = Ty.Var.mk "alpha" in
      let a_ty = Ty.of_var a in
      let b = Ty.Var.mk "beta" in
      let b_ty = Ty.of_var b in
      Id.const
        ~name:"select" ~builtin:Select
        "Select" [a; b] [Ty.array a_ty b_ty; a_ty] b_ty

    let store =
      let a = Ty.Var.mk "alpha" in
      let a_ty = Ty.of_var a in
      let b = Ty.Var.mk "beta" in
      let b_ty = Ty.of_var b in
      let arr = Ty.array a_ty b_ty in
      Id.const
        ~name:"store" ~builtin:Store
        "Store" [a; b] [arr; a_ty; b_ty] arr

    let coerce =
      let a = Ty.Var.mk "alpha" in
      let b = Ty.Var.mk "beta" in
      Id.const ~builtin:Coercion "coerce"
        [a; b] [Ty.of_var a] (Ty.of_var b)

    module Int = struct

      let int =
        with_cache ~cache:(Hashtbl.create 113) (fun s ->
            Id.const ~builtin:(Integer s) s [] [] Ty.int
          )

      let minus = Id.const
          ~pos:Pretty.Prefix ~name:"-" ~builtin:Minus
          "Minus" [] [Ty.int] Ty.int

      let add = Id.const
          ~pos:Pretty.Infix ~name:"+" ~builtin:Add
          "Add" [] [Ty.int; Ty.int] Ty.int

      let sub = Id.const
          ~pos:Pretty.Infix ~name:"-" ~builtin:Sub
          "Sub" [] [Ty.int; Ty.int] Ty.int

      let mul = Id.const
          ~pos:Pretty.Infix ~name:"*" ~builtin:Mul
          "Mul" [] [Ty.int; Ty.int] Ty.int

      let div_e = Id.const
          ~pos:Pretty.Infix ~name:"/" ~builtin:Div_e
          "Div_e" [] [Ty.int; Ty.int] Ty.int
      let div_t = Id.const
          ~pos:Pretty.Infix ~name:"/t" ~builtin:Div_t
          "Div_t" [] [Ty.int; Ty.int] Ty.int
      let div_f = Id.const
          ~pos:Pretty.Infix ~name:"/f" ~builtin:Div_f
          "Div_f" [] [Ty.int; Ty.int] Ty.int

      let rem_e = Id.const
          ~pos:Pretty.Infix ~name:"%" ~builtin:Modulo_e
          "Modulo" [] [Ty.int; Ty.int] Ty.int
      let rem_t = Id.const
          ~pos:Pretty.Infix ~name:"%e" ~builtin:Modulo_t
          "Modulo" [] [Ty.int; Ty.int] Ty.int
      let rem_f = Id.const
          ~pos:Pretty.Infix ~name:"%f" ~builtin:Modulo_f
          "Modulo" [] [Ty.int; Ty.int] Ty.int

      let abs = Id.const
          ~name:"abs" ~builtin:Abs
          "Abs" [] [Ty.int] Ty.int

      let lt = Id.const
          ~pos:Pretty.Infix ~name:"<" ~builtin:Lt
          "LessThan" [] [Ty.int; Ty.int] Ty.prop

      let le = Id.const
          ~pos:Pretty.Infix ~name:"<=" ~builtin:Leq
          "LessOrEqual" [] [Ty.int; Ty.int] Ty.prop

      let gt = Id.const
          ~pos:Pretty.Infix ~name:">" ~builtin:Gt
          "GreaterThan" [] [Ty.int; Ty.int] Ty.prop

      let ge = Id.const
          ~pos:Pretty.Infix ~name:">=" ~builtin:Geq
          "GreaterOrEqual" [] [Ty.int; Ty.int] Ty.prop

      let floor = Id.const
          ~name:"floor" ~builtin:Floor
          "Floor" [] [Ty.int] Ty.int

      let ceiling = Id.const
          ~name:"ceiling" ~builtin:Ceiling
          "Ceiling" [] [Ty.int] Ty.int

      let truncate = Id.const
          ~name:"truncate" ~builtin:Truncate
          "Truncate" [] [Ty.int] Ty.int

      let round = Id.const
          ~name:"round" ~builtin:Round
          "Round" [] [Ty.int] Ty.int

      let is_int = Id.const
          ~name:"is_int" ~builtin:Is_int
          "Is_int" [] [Ty.int] Ty.prop

      let is_rat = Id.const
          ~name:"is_rat" ~builtin:Is_rat
          "Is_rat" [] [Ty.int] Ty.prop

      let divisible = Id.const
          ~builtin:Divisible "Divisible"
          [] [Ty.int; Ty.int] Ty.prop

    end

    module Rat = struct

      let rat =
        with_cache ~cache:(Hashtbl.create 113) (fun s ->
            Id.const ~builtin:(Rational s) s [] [] Ty.rat
          )

      let minus = Id.const
          ~pos:Pretty.Prefix ~name:"-" ~builtin:Minus
          "Minus" [] [Ty.rat] Ty.rat

      let add = Id.const
          ~pos:Pretty.Infix ~name:"+" ~builtin:Add
          "Add" [] [Ty.rat; Ty.rat] Ty.rat

      let sub = Id.const
          ~pos:Pretty.Infix ~name:"-" ~builtin:Sub
          "Sub" [] [Ty.rat; Ty.rat] Ty.rat

      let mul = Id.const
          ~pos:Pretty.Infix ~name:"*" ~builtin:Mul
          "Mul" [] [Ty.rat; Ty.rat] Ty.rat

      let div = Id.const
          ~pos:Pretty.Infix ~name:"/" ~builtin:Div
          "Div" [] [Ty.rat; Ty.rat] Ty.rat
      let div_e = Id.const
          ~pos:Pretty.Infix ~name:"/e" ~builtin:Div_e
          "Div_e" [] [Ty.rat; Ty.rat] Ty.rat
      let div_t = Id.const
          ~pos:Pretty.Infix ~name:"/t" ~builtin:Div_t
          "Div_t" [] [Ty.rat; Ty.rat] Ty.rat
      let div_f = Id.const
          ~pos:Pretty.Infix ~name:"/f" ~builtin:Div_f
          "Div_f" [] [Ty.rat; Ty.rat] Ty.rat

      let rem_e = Id.const
          ~pos:Pretty.Infix ~name:"%" ~builtin:Modulo_e
          "Modulo" [] [Ty.rat; Ty.rat] Ty.rat
      let rem_t = Id.const
          ~pos:Pretty.Infix ~name:"%" ~builtin:Modulo_t
          "Modulo" [] [Ty.rat; Ty.rat] Ty.rat
      let rem_f = Id.const
          ~pos:Pretty.Infix ~name:"%" ~builtin:Modulo_f
          "Modulo" [] [Ty.rat; Ty.rat] Ty.rat

      let lt = Id.const
          ~pos:Pretty.Infix ~name:"<" ~builtin:Lt
          "LessThan" [] [Ty.rat; Ty.rat] Ty.prop

      let le = Id.const
          ~pos:Pretty.Infix ~name:"<=" ~builtin:Leq
          "LessOrEqual" [] [Ty.rat; Ty.rat] Ty.prop

      let gt = Id.const
          ~pos:Pretty.Infix ~name:">" ~builtin:Gt
          "GreaterThan" [] [Ty.rat; Ty.rat] Ty.prop

      let ge = Id.const
          ~pos:Pretty.Infix ~name:">=" ~builtin:Geq
          "GreaterOrEqual" [] [Ty.rat; Ty.rat] Ty.prop

      let floor = Id.const
          ~name:"floor" ~builtin:Floor
          "Floor" [] [Ty.rat] Ty.rat

      let ceiling = Id.const
          ~name:"ceiling" ~builtin:Ceiling
          "Ceiling" [] [Ty.rat] Ty.rat

      let truncate = Id.const
          ~name:"truncate" ~builtin:Truncate
          "Truncate" [] [Ty.rat] Ty.rat

      let round = Id.const
          ~name:"round" ~builtin:Round
          "Round" [] [Ty.rat] Ty.rat

      let is_int = Id.const
          ~name:"is_int" ~builtin:Is_int
          "Is_int" [] [Ty.rat] Ty.prop

      let is_rat = Id.const
          ~name:"is_rat" ~builtin:Is_rat
          "Is_rat" [] [Ty.rat] Ty.prop
    end

    module Real = struct

      let real =
        with_cache ~cache:(Hashtbl.create 113) (fun s ->
            Id.const ~builtin:(Decimal s) s [] [] Ty.real
          )

      let minus = Id.const
          ~pos:Pretty.Prefix ~name:"-" ~builtin:Minus
          "Minus" [] [Ty.real] Ty.real

      let add = Id.const
          ~pos:Pretty.Infix ~name:"+" ~builtin:Add
          "Add" [] [Ty.real; Ty.real] Ty.real

      let sub = Id.const
          ~pos:Pretty.Infix ~name:"-" ~builtin:Sub
          "Sub" [] [Ty.real; Ty.real] Ty.real

      let mul = Id.const
          ~pos:Pretty.Infix ~name:"*" ~builtin:Mul
          "Mul" [] [Ty.real; Ty.real] Ty.real

      let div = Id.const
          ~pos:Pretty.Infix ~name:"/" ~builtin:Div
          "Div" [] [Ty.real; Ty.real] Ty.real

      let div_e = Id.const
          ~pos:Pretty.Infix ~name:"/" ~builtin:Div_e
          "Div_e" [] [Ty.real; Ty.real] Ty.real
      let div_t = Id.const
          ~pos:Pretty.Infix ~name:"/t" ~builtin:Div_t
          "Div_t" [] [Ty.real; Ty.real] Ty.real
      let div_f = Id.const
          ~pos:Pretty.Infix ~name:"/f" ~builtin:Div_f
          "Div_f" [] [Ty.real; Ty.real] Ty.real

      let rem_e = Id.const
          ~pos:Pretty.Infix ~name:"%" ~builtin:Modulo_e
          "Modulo" [] [Ty.real; Ty.real] Ty.real
      let rem_t = Id.const
          ~pos:Pretty.Infix ~name:"%" ~builtin:Modulo_t
          "Modulo" [] [Ty.real; Ty.real] Ty.real
      let rem_f = Id.const
          ~pos:Pretty.Infix ~name:"%" ~builtin:Modulo_f
          "Modulo" [] [Ty.real; Ty.real] Ty.real

      let lt = Id.const
          ~pos:Pretty.Infix ~name:"<" ~builtin:Lt
          "LessThan" [] [Ty.real; Ty.real] Ty.prop

      let le = Id.const
          ~pos:Pretty.Infix ~name:"<=" ~builtin:Leq
          "LessOrEqual" [] [Ty.real; Ty.real] Ty.prop

      let gt = Id.const
          ~pos:Pretty.Infix ~name:">" ~builtin:Gt
          "GreaterThan" [] [Ty.real; Ty.real] Ty.prop

      let ge = Id.const
          ~pos:Pretty.Infix ~name:">=" ~builtin:Geq
          "GreaterOrEqual" [] [Ty.real; Ty.real] Ty.prop

      let floor = Id.const
          ~name:"floor" ~builtin:Floor
          "Floor" [] [Ty.real] Ty.real

      let ceiling = Id.const
          ~name:"ceiling" ~builtin:Ceiling
          "Ceiling" [] [Ty.real] Ty.real

      let truncate = Id.const
          ~name:"truncate" ~builtin:Truncate
          "Truncate" [] [Ty.real] Ty.real

      let round = Id.const
          ~name:"round" ~builtin:Round
          "Round" [] [Ty.real] Ty.real

      let is_int = Id.const
          ~name:"is_int" ~builtin:Is_int
          "Is_int" [] [Ty.real] Ty.prop

      let is_rat = Id.const
          ~name:"is_rat" ~builtin:Is_rat
          "Is_rat" [] [Ty.real] Ty.prop
    end

    module Bitv = struct

      let bitv s =
        Id.const ~builtin:(Bitvec s)
          (Format.asprintf "bv#%s#" s) [] [] (Ty.bitv (String.length s))

      let concat =
        with_cache ~cache:(Hashtbl.create 13) (fun (i, j) ->
            Id.const ~builtin:Bitv_concat "bitv_concat"
              [] [Ty.bitv i; Ty.bitv j] (Ty.bitv (i + j))
          )

      let extract =
        with_cache ~cache:(Hashtbl.create 13) (fun (i, j, n) ->
            Id.const ~builtin:(Bitv_extract (i, j))
              (Format.asprintf "bitv_extract_%d_%d" i j) []
              [Ty.bitv n] (Ty.bitv (i - j + 1))
          )

      let repeat =
        with_cache ~cache:(Hashtbl.create 13) (fun (k, n) ->
            Id.const ~builtin:Bitv_repeat (Format.asprintf "bitv_repeat_%d" k)
              [] [Ty.bitv n] (Ty.bitv (n * k))
          )

      let zero_extend =
        with_cache ~cache:(Hashtbl.create 13) (fun (k, n) ->
            Id.const ~builtin:Bitv_zero_extend (Format.asprintf "zero_extend_%d" k)
              [] [Ty.bitv n] (Ty.bitv (n + k))
          )

      let sign_extend =
        with_cache ~cache:(Hashtbl.create 13) (fun (k, n) ->
            Id.const ~builtin:Bitv_sign_extend (Format.asprintf "sign_extend_%d" k)
              [] [Ty.bitv n] (Ty.bitv (n + k))
          )

      let rotate_right =
        with_cache ~cache:(Hashtbl.create 13) (fun (k, n) ->
            Id.const ~builtin:(Bitv_rotate_right k)
              (Format.asprintf "rotate_right_%d" k) [] [Ty.bitv n] (Ty.bitv n)
          )

      let rotate_left =
        with_cache ~cache:(Hashtbl.create 13) (fun (k, n) ->
            Id.const ~builtin:(Bitv_rotate_left k)
              (Format.asprintf "rotate_left_%d" k) [] [Ty.bitv n] (Ty.bitv n)
          )

      let not =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            Id.const ~builtin:Bitv_not "bvnot" [] [Ty.bitv n] (Ty.bitv n)
          )

      let and_ =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            Id.const ~builtin:Bitv_and "bvand" []
              [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let or_ =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            Id.const ~builtin:Bitv_or "bvor" []
              [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let nand =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            Id.const ~builtin:Bitv_nand "bvnand" []
              [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let nor =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            Id.const ~builtin:Bitv_nor "bvnor" []
              [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let xor =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            Id.const ~builtin:Bitv_xor "bvxor" []
              [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let xnor =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            Id.const ~builtin:Bitv_xnor "bvxnor" []
              [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let comp =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            Id.const ~builtin:Bitv_comp "bvcomp" []
              [Ty.bitv n; Ty.bitv n] (Ty.bitv 1)
          )

      let neg =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            Id.const ~builtin:Bitv_neg "bvneg" [] [Ty.bitv n] (Ty.bitv n)
          )

      let add =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            Id.const ~builtin:Bitv_add "bvadd" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let sub =
      with_cache ~cache:(Hashtbl.create 13) (fun n ->
          Id.const ~builtin:Bitv_sub "bvsub" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
        )

      let mul =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            Id.const ~builtin:Bitv_mul "bvmul" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let udiv =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            Id.const ~builtin:Bitv_udiv "bvudiv" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let urem =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            Id.const ~builtin:Bitv_urem "bvurem" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let sdiv =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            Id.const ~builtin:Bitv_sdiv "bvsdiv" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let srem =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            Id.const ~builtin:Bitv_srem "bvsrem" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let smod =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            Id.const ~builtin:Bitv_smod "bvsmod" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let shl =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            Id.const ~builtin:Bitv_shl "bvshl" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let lshr =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            Id.const ~builtin:Bitv_lshr "bvlshr" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let ashr =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            Id.const ~builtin:Bitv_ashr "bvashr" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
          )

      let ult =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            Id.const ~builtin:Bitv_ult "bvult" [] [Ty.bitv n; Ty.bitv n] Ty.prop
          )

      let ule =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            Id.const ~builtin:Bitv_ule "bvule" [] [Ty.bitv n; Ty.bitv n] Ty.prop
          )

      let ugt =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            Id.const ~builtin:Bitv_ugt "bvugt" [] [Ty.bitv n; Ty.bitv n] Ty.prop
          )

      let uge =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            Id.const ~builtin:Bitv_uge "bvsge" [] [Ty.bitv n; Ty.bitv n] Ty.prop
          )

      let slt =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            Id.const ~builtin:Bitv_slt "bvslt" [] [Ty.bitv n; Ty.bitv n] Ty.prop
          )

      let sle =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            Id.const ~builtin:Bitv_sle "bvsle" [] [Ty.bitv n; Ty.bitv n] Ty.prop
          )

      let sgt =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            Id.const ~builtin:Bitv_sgt "bvsgt" [] [Ty.bitv n; Ty.bitv n] Ty.prop
          )

      let sge =
        with_cache ~cache:(Hashtbl.create 13) (fun n ->
            Id.const ~builtin:Bitv_sge "bvsge" [] [Ty.bitv n; Ty.bitv n] Ty.prop
          )

    end

    module Float = struct

      let fp =
        with_cache ~cache:(Hashtbl.create 13) (fun (e, s) ->
            Id.const ~builtin:(Fp(e, s)) "fp" []
              [Ty.bitv 1; Ty.bitv e; Ty.bitv (s-1)] (Ty.float e s)
          )

      let roundNearestTiesToEven =
        Id.const ~builtin:RoundNearestTiesToEven "RoundNearestTiesToEven" [] [] Ty.roundingMode

      let roundNearestTiesToAway =
        Id.const ~builtin:RoundNearestTiesToAway "RoundNearestTiesToAway" [] [] Ty.roundingMode

      let roundTowardPositive =
        Id.const ~builtin:RoundTowardPositive "RoundTowardPositive" [] [] Ty.roundingMode

      let roundTowardNegative =
        Id.const ~builtin:RoundTowardNegative "RoundTowardNegative" [] [] Ty.roundingMode

      let roundTowardZero =
        Id.const ~builtin:RoundTowardZero "RoundTowardZero" [] [] Ty.roundingMode

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
            Id.const ~builtin:(builtin es) name [] args res
          )

      let plus_infinity = fp_gen_fun ~args:0 "plus_infinity" (fun (e,s) -> Plus_infinity (e,s))
      let minus_infinity = fp_gen_fun ~args:0 "minus_infinity" (fun (e,s) -> Minus_infinity (e,s))
      let plus_zero = fp_gen_fun ~args:0 "plus_zero" (fun (e,s) -> Plus_zero (e,s))
      let minus_zero = fp_gen_fun ~args:0 "minus_zero" (fun (e,s) -> Minus_zero (e,s))
      let nan = fp_gen_fun ~args:0 "nan" (fun (e,s) -> NaN (e,s))
      let abs = fp_gen_fun ~args:1 "fp.abs" (fun (e,s) -> Fp_abs (e,s))
      let neg = fp_gen_fun ~args:1 "fp.neg" (fun (e,s) -> Fp_neg (e,s))
      let add = fp_gen_fun ~args:2 ~rm:() "fp.add" (fun (e,s) -> Fp_add (e,s))
      let sub = fp_gen_fun ~args:2 ~rm:() "fp.sub" (fun (e,s) -> Fp_sub (e,s))
      let mul = fp_gen_fun ~args:2 ~rm:() "fp.mul" (fun (e,s) -> Fp_mul (e,s))
      let div = fp_gen_fun ~args:2 ~rm:() "fp.div" (fun (e,s) -> Fp_div (e,s))
      let fma = fp_gen_fun ~args:3 ~rm:() "fp.fma" (fun (e,s) -> Fp_fma (e,s))
      let sqrt = fp_gen_fun ~args:1 ~rm:() "fp.sqrt" (fun (e,s) -> Fp_sqrt (e,s))
      let rem = fp_gen_fun ~args:2 "fp.rem" (fun (e,s) -> Fp_rem (e,s))
      let roundToIntegral = fp_gen_fun ~args:1 ~rm:() "fp.roundToIntegral" (fun (e,s) -> Fp_roundToIntegral (e,s))
      let min = fp_gen_fun ~args:2 "fp.min" (fun (e,s) -> Fp_min (e,s))
      let max = fp_gen_fun ~args:2 "fp.max" (fun (e,s) -> Fp_max (e,s))
      let leq = fp_gen_fun ~args:2 ~res:Ty.prop "fp.leq" (fun (e,s) -> Fp_leq (e,s))
      let lt = fp_gen_fun ~args:2 ~res:Ty.prop "fp.lt" (fun (e,s) -> Fp_lt (e,s))
      let geq = fp_gen_fun ~args:2 ~res:Ty.prop "fp.geq" (fun (e,s) -> Fp_geq (e,s))
      let gt = fp_gen_fun ~args:2 ~res:Ty.prop "fp.gt" (fun (e,s) -> Fp_gt (e,s))
      let eq = fp_gen_fun ~args:2 ~res:Ty.prop "fp.eq" (fun (e,s) -> Fp_eq (e,s))
      let isNormal = fp_gen_fun ~args:1 ~res:Ty.prop "fp.isnormal" (fun (e,s) -> Fp_isNormal (e,s))
      let isSubnormal = fp_gen_fun ~args:1 ~res:Ty.prop "fp.issubnormal" (fun (e,s) -> Fp_isSubnormal (e,s))
      let isZero = fp_gen_fun ~args:1 ~res:Ty.prop "fp.iszero" (fun (e,s) -> Fp_isZero (e,s))
      let isInfinite = fp_gen_fun ~args:1 ~res:Ty.prop "fp.isinfinite" (fun (e,s) -> Fp_isInfinite (e,s))
      let isNaN = fp_gen_fun ~args:1 ~res:Ty.prop "fp.isnan" (fun (e,s) -> Fp_isNaN (e,s))
      let isNegative = fp_gen_fun ~args:1 ~res:Ty.prop "fp.isnegative" (fun (e,s) -> Fp_isNegative (e,s))
      let isPositive = fp_gen_fun ~args:1 ~res:Ty.prop "fp.ispositive" (fun (e,s) -> Fp_isPositive (e,s))
      let to_real = fp_gen_fun ~args:1 ~res:Ty.real "fp.to_real" (fun (e,s) -> To_real (e,s))

      let ieee_format_to_fp =
        with_cache ~cache:(Hashtbl.create 13) (fun ((e,s) as es) ->
            Id.const ~builtin:(Ieee_format_to_fp (e,s)) "to_fp" [] [Ty.bitv (e+s)] (Ty.float' es)
          )
      let to_fp =
        with_cache ~cache:(Hashtbl.create 13) (fun (e1,s1,e2,s2) ->
            Id.const ~builtin:(Fp_to_fp (e1,s1,e2,s2)) "to_fp" [] [Ty.roundingMode;Ty.float e1 s1] (Ty.float e2 s2)
          )
      let real_to_fp =
        with_cache ~cache:(Hashtbl.create 13) (fun ((e,s) as es) ->
            Id.const ~builtin:(Real_to_fp (e,s)) "to_fp" [] [Ty.roundingMode;Ty.real] (Ty.float' es)
          )
      let sbv_to_fp =
        with_cache ~cache:(Hashtbl.create 13) (fun (bv,e,s) ->
            Id.const ~builtin:(Sbv_to_fp (bv,e,s)) "to_fp" [] [Ty.roundingMode;Ty.bitv bv] (Ty.float e s)
          )
      let ubv_to_fp =
        with_cache ~cache:(Hashtbl.create 13) (fun (bv,e,s) ->
            Id.const ~builtin:(Ubv_to_fp (bv,e,s)) "to_fp" [] [Ty.roundingMode;Ty.bitv bv] (Ty.float e s)
          )
      let to_ubv =
        with_cache ~cache:(Hashtbl.create 13) (fun (e,s,bv) ->
            Id.const ~builtin:(To_ubv (bv,e,s)) "fp.to_ubv" [] [Ty.roundingMode;Ty.float e s] (Ty.bitv bv)
          )
      let to_sbv =
        with_cache ~cache:(Hashtbl.create 13) (fun (e,s,bv) ->
            Id.const ~builtin:(To_sbv (bv,e,s)) "fp.to_sbv" [] [Ty.roundingMode;Ty.float e s] (Ty.bitv bv)
          )

    end

    module String = struct

      let string =
        with_cache ~cache:(Hashtbl.create 13) (fun s ->
            Id.const ~builtin:(Str s) (Format.asprintf {|"%s"|} s) [] [] Ty.string
          )

      let length =
        Id.const ~builtin:Str_length "length"
          [] [Ty.string] Ty.int
      let at =
        Id.const ~builtin:Str_at "at"
          [] [Ty.string; Ty.int] Ty.string
      let to_code =
        Id.const ~builtin:Str_to_code "to_code"
          [] [Ty.string] Ty.int
      let of_code =
        Id.const ~builtin:Str_of_code "of_code"
          [] [Ty.int] Ty.string
      let is_digit =
        Id.const ~builtin:Str_is_digit "is_digit"
          [] [Ty.string] Ty.prop
      let to_int =
        Id.const ~builtin:Str_to_int "to_int"
          [] [Ty.string] Ty.int
      let of_int =
        Id.const ~builtin:Str_of_int "of_int"
          [] [Ty.int] Ty.string
      let concat =
        Id.const ~builtin:Str_concat ~pos:Pretty.Infix "++"
          [] [Ty.string; Ty.string] Ty.string
      let sub =
        Id.const ~builtin:Str_sub "sub"
          [] [Ty.string; Ty.int; Ty.int] Ty.string
      let index_of =
        Id.const ~builtin:Str_index_of "index_of"
          [] [Ty.string; Ty.string; Ty.int] Ty.int
      let replace =
        Id.const ~builtin:Str_replace "replace"
          [] [Ty.string; Ty.string; Ty.string] Ty.string
      let replace_all =
        Id.const ~builtin:Str_replace_all "replace_all"
          [] [Ty.string; Ty.string; Ty.string] Ty.string
      let replace_re =
        Id.const ~builtin:Str_replace_re "replace_re"
          [] [Ty.string; Ty.string_reg_lang; Ty.string] Ty.string
      let replace_re_all =
        Id.const ~builtin:Str_replace_re_all "replace_re_all"
          [] [Ty.string; Ty.string_reg_lang; Ty.string] Ty.string
      let is_prefix =
        Id.const ~builtin:Str_is_prefix "is_prefix"
          [] [Ty.string; Ty.string] Ty.prop
      let is_suffix =
        Id.const ~builtin:Str_is_suffix "is_suffix"
          [] [Ty.string; Ty.string] Ty.prop
      let contains =
        Id.const ~builtin:Str_contains "contains"
          [] [Ty.string; Ty.string] Ty.prop
      let lt =
        Id.const ~builtin:Str_lexicographic_strict
          ~pos:Pretty.Infix "lt"
          [] [Ty.string; Ty.string] Ty.prop
      let leq =
        Id.const ~builtin:Str_lexicographic_large
          ~pos:Pretty.Infix "leq"
          [] [Ty.string; Ty.string] Ty.prop
      let in_re =
        Id.const ~builtin:Str_in_re "in_re"
          [] [Ty.string; Ty.string_reg_lang] Ty.prop

      module Reg_Lang = struct

        let empty =
          Id.const ~builtin:Re_empty "empty"
            [] [] Ty.string_reg_lang
        let all =
          Id.const ~builtin:Re_all "all"
            [] [] Ty.string_reg_lang
        let allchar =
          Id.const ~builtin:Re_allchar "allchar"
            [] [] Ty.string_reg_lang
        let of_string =
          Id.const ~builtin:Re_of_string "of_string"
            [] [Ty.string] Ty.string_reg_lang
        let range =
          Id.const ~builtin:Re_range "range"
            [] [Ty.string; Ty.string] Ty.string_reg_lang
        let concat =
          Id.const ~builtin:Re_concat ~pos:Pretty.Infix "++"
            [] [Ty.string_reg_lang; Ty.string_reg_lang] Ty.string_reg_lang
        let union =
          Id.const ~builtin:Re_union ~pos:Pretty.Infix "∪"
            [] [Ty.string_reg_lang; Ty.string_reg_lang] Ty.string_reg_lang
        let inter =
          Id.const ~builtin:Re_inter ~pos:Pretty.Infix "∩"
            [] [Ty.string_reg_lang; Ty.string_reg_lang] Ty.string_reg_lang
        let diff =
          Id.const ~builtin:Re_diff ~pos:Pretty.Infix "-"
            [] [Ty.string_reg_lang; Ty.string_reg_lang] Ty.string_reg_lang
        let star =
          Id.const ~builtin:Re_star ~pos:Pretty.Prefix "*"
            [] [Ty.string_reg_lang] Ty.string_reg_lang
        let cross =
          Id.const ~builtin:Re_cross ~pos:Pretty.Prefix "+"
            [] [Ty.string_reg_lang] Ty.string_reg_lang
        let complement =
          Id.const ~builtin:Re_complement "complement"
            [] [Ty.string_reg_lang] Ty.string_reg_lang
        let option =
          Id.const ~builtin:Re_option "option"
            [] [Ty.string_reg_lang] Ty.string_reg_lang
        let power =
          with_cache ~cache:(Hashtbl.create 13) (fun n ->
              Id.const ~builtin:(Re_power n) (Format.asprintf "power_%d" n)
                [] [Ty.string_reg_lang] Ty.string_reg_lang
            )
        let loop =
          with_cache ~cache:(Hashtbl.create 13) (fun (n1, n2) ->
              Id.const ~builtin:(Re_loop (n1, n2)) (Format.asprintf "loop_%d_%d" n1 n2)
                [] [Ty.string_reg_lang] Ty.string_reg_lang
            )

      end

    end

  end

  (* Constructors are simply constants *)
  module Cstr = struct
    type t = term_const
    let tag = Id.tag
    let hash = Id.hash
    let equal = Id.equal
    let compare = Id.compare
    let get_tag = Id.get_tag
    let get_tag_last = Id.get_tag_last
    let arity (c : t) =
      List.length c.ty.fun_vars, List.length c.ty.fun_args

    let tester c =
      match c.builtin with
      | Constructor { adt; case; } ->
        begin match Ty.definition adt with
          | Some Adt { cases; _ } -> cases.(case).tester
          | _ -> assert false
        end
      | _ -> raise (Constructor_expected c)

    let void =
      match define_adt Ty.Const.unit [] ["void", []] with
      | [void, _] -> void
      | _ -> assert false

    let pattern_arity (c : t) ret tys =
      try
        let s = List.fold_left2 Subst.Var.bind Subst.empty c.ty.fun_vars tys in
        let s = Ty.robinson s c.ty.fun_ret ret in
        List.map (Ty.subst s) c.ty.fun_args
      with
      | Ty.Impossible_unification _ -> raise (Wrong_sum_type (c, ret))
      | Invalid_argument _ -> raise (Bad_term_arity (c, tys, []))

  end

  (* Record fields are represented as their destructors, i.e. constants *)
  module Field = struct
    type t = term_const
    let hash = Id.hash
    let equal = Id.equal
    let compare = Id.compare

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
      | Destructor { adt = ty_d; case = i; field = j; _ } ->
        if Id.equal ty_c ty_d then begin
          assert (i = 0);
          j
        end else
          raise (Wrong_record_type (f, ty_c))
      | _ ->
        raise (Field_expected f)

  end


  (* Filter check *)
  let rec check_filters res f tys args = function
    | [] -> res
    | (name, active, check) :: r ->
      if !active then match (check f tys args) with
        | `Pass -> check_filters res f tys args r
        | `Warn -> check_filters res f tys args r
        | `Error msg -> raise (Filter_failed_term (name, res, msg))
      else
        check_filters res f tys args r

  (* Term creation *)
  let mk ?(tags=Tag.empty) descr ty = { descr; ty; hash = -1; tags; }

  let of_var v = mk (Var v) v.ty

  (* This function does not check types enough, do not export outside the module *)
  let mk_bind b body =
    mk (Binder (b, body)) (ty body)

  (* Substitutions *)
  let rec ty_var_list_subst ty_var_map = function
    | [] -> ty_var_map
    | (v :: r : ty_var list) ->
      ty_var_list_subst (Subst.Var.remove v ty_var_map) r

  let rec term_var_list_subst ty_var_map t_var_map acc = function
    | [] -> List.rev acc, t_var_map
    | (v :: r : term_var list) ->
      let ty = Ty.subst ty_var_map v.ty in
      if not (Ty.equal ty v.ty) then
        let nv = Var.mk v.name ty in
        term_var_list_subst ty_var_map
          (Subst.Var.bind t_var_map v (of_var nv)) (nv :: acc) r
      else
        term_var_list_subst ty_var_map
          (Subst.Var.remove v t_var_map) (v :: acc) r

  let rec subst_aux ~fix ty_var_map t_var_map (t : t) =
    match t.descr with
    | Var v ->
      begin match Subst.Var.get v t_var_map with
        | exception Not_found -> t
        | term ->
          if fix
          then subst_aux ~fix ty_var_map t_var_map term
          else term
      end
    | App (f, tys, args) ->
      let new_tys = List.map (Ty.subst ~fix ty_var_map) tys in
      let new_args = List.map (subst_aux ~fix ty_var_map t_var_map) args in
      if List.for_all2 (==) new_tys tys && List.for_all2 (==) new_args args then t
      else apply f new_tys new_args
    | Binder (b, body) ->
      let b', ty_var_map, t_var_map = binder_subst ~fix ty_var_map t_var_map b in
      mk_bind b' (subst_aux ~fix ty_var_map t_var_map body)
    | Match (scrutinee, branches) ->
      let scrutinee = subst_aux ~fix ty_var_map t_var_map scrutinee in
      let branches = List.map (branch_subst ~fix ty_var_map t_var_map) branches in
      pattern_match scrutinee branches

  and binder_subst ~fix ty_var_map t_var_map = function
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
    | Letin l ->
      let l, t_var_map = binding_list_subst ~fix ty_var_map t_var_map [] l in
      Letin l, ty_var_map, t_var_map

  and binding_list_subst ~fix ty_var_map t_var_map acc = function
    | [] -> List.rev acc, t_var_map
    | ((v, t) :: r : (term_var * term) list) ->
      let t = subst_aux ~fix ty_var_map t_var_map t in
      if Ty.equal (ty t) v.ty then begin
        let t_var_map = Subst.Var.remove v t_var_map in
        let acc = (v, t) :: acc in
        binding_list_subst ~fix ty_var_map t_var_map acc r
      end else begin
        let nv = Var.mk v.name (ty t) in
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
  and instantiate (f : term_const) tys args =
    if List.length f.ty.fun_vars <> List.length tys ||
       List.length f.ty.fun_args <> List.length args then begin
      raise (Bad_term_arity (f, tys, args))
    end else begin
      let map = List.fold_left2 Subst.Var.bind Subst.empty f.ty.fun_vars tys in
      let s = List.fold_left2 (fun s expected term ->
          try Ty.robinson s expected (ty term)
          with Ty.Impossible_unification _ ->
            raise (Wrong_type (term, Ty.subst s expected))
        ) map f.ty.fun_args args
      in
      Subst.iter Ty.set_wildcard s;
      Ty.subst s f.ty.fun_ret
    end

  (* Application *)
  and apply f tys args =
    let ret = instantiate f tys args in
    let res = mk (App (f, tys, args)) ret in
    check_filters res f tys args (Const.get_tag f Filter.term)

  (* Pattern matching *)
  and pattern_match scrutinee branches =
    let scrutinee_ty = ty scrutinee in
    (* first,
       unify the type of the scrutinee and all patterns,
       and unify the type of all bodies *)
    let body_ty = Ty.wildcard () in
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
    let () = check_exhaustivity (ty scrutinee) (List.map fst branches) in
    (* Build the pattern matching *)
    mk (Match (scrutinee, branches)) body_ty


  (* Wrappers around application *)

  let apply_cstr = apply

  let apply_field (f : term_const) t =
    let tys = init_list
        (List.length f.ty.fun_vars)
        (fun _ -> Ty.wildcard ())
    in
    apply f tys [t]

  (* ADT constructor tester *)
  let cstr_tester c t =
    let tester = Cstr.tester c in
    let ty_args = init_list
        (List.length tester.ty.fun_vars)
        (fun _ -> Ty.wildcard ())
    in
    apply tester ty_args [t]

  (* Recor creation *)
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
        | Destructor { adt = ty_c; cstr = c; _ } when Ty.is_record ty_c ->
          let fields = build_record_fields ty_c l in
          (* Check that all fields are indeed present, and create the list
             of term arguments *)
          let t_args = Array.to_list @@ Array.mapi (fun i o ->
              match o with
              | None -> missing ty_c i
              | Some v -> v
            ) fields in
          (* Create type wildcard to be unified during application. *)
          let ty_args = init_list
              (List.length c.ty.fun_vars)
              (fun _ -> Ty.wildcard ())
          in
          apply c ty_args t_args
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

  (* typing annotations *)
  let ensure t ty =
    match Ty.robinson Subst.empty ty t.ty with
    | s -> subst s Subst.empty t
    | exception Ty.Impossible_unification _ ->
      raise (Wrong_type (t, ty))

  (* coercion *)
  let coerce dst_ty t =
    let src_ty = ty t in
    apply Const.coerce [src_ty; dst_ty] [t]

  (* Common constructions *)
  let void = apply Cstr.void [] []

  let _true = apply Const._true [] []
  let _false = apply Const._false [] []

  let eq a b = apply Const.eq [ty a] [a; b]

  let eqs = function
    | [] -> apply (Const.eqs 0) [] []
    | (h :: _) as l -> apply (Const.eqs (List.length l)) [ty h] l

  let distinct = function
    | [] -> apply (Const.distinct 0) [] []
    | (h :: _) as l -> apply (Const.distinct (List.length l)) [ty h] l

  let neg x = apply Const.neg [] [x]

  let _and l = apply (Const._and (List.length l)) [] l

  let _or l = apply (Const._or (List.length l)) [] l

  let nand p q = apply Const.nand [] [p; q]

  let nor p q = apply Const.nor [] [p; q]

  let xor p q = apply Const.xor [] [p; q]

  let imply p q = apply Const.imply [] [p; q]

  let equiv p q = apply Const.equiv [] [p; q]

  let int s = apply (Const.Int.int s) [] []
  let rat s = apply (Const.Rat.rat s) [] []
  let real s = apply (Const.Real.real s) [] []

  (* arithmetic *)
  module Int = struct
    let mk = int
    let minus t = apply Const.Int.minus [] [t]
    let add a b = apply Const.Int.add [] [a; b]
    let sub a b = apply Const.Int.sub [] [a; b]
    let mul a b = apply Const.Int.mul [] [a; b]
    let div a b = apply Const.Int.div_e [] [a; b]
    let rem a b = apply Const.Int.rem_e [] [a; b]
    let div_e a b = apply Const.Int.div_e [] [a; b]
    let div_t a b = apply Const.Int.div_t [] [a; b]
    let div_f a b = apply Const.Int.div_f [] [a; b]
    let rem_e a b = apply Const.Int.rem_e [] [a; b]
    let rem_t a b = apply Const.Int.rem_t [] [a; b]
    let rem_f a b = apply Const.Int.rem_f [] [a; b]
    let abs a = apply Const.Int.abs [] [a]
    let lt a b = apply Const.Int.lt [] [a; b]
    let le a b = apply Const.Int.le [] [a; b]
    let gt a b = apply Const.Int.gt [] [a; b]
    let ge a b = apply Const.Int.ge [] [a; b]
    let floor a = apply Const.Int.floor [] [a]
    let ceiling a = apply Const.Int.ceiling [] [a]
    let truncate a = apply Const.Int.truncate [] [a]
    let round a = apply Const.Int.round [] [a]
    let is_int a = apply Const.Int.is_int [] [a]
    let is_rat a = apply Const.Int.is_rat [] [a]
    let to_int t = coerce Ty.int t
    let to_rat t = coerce Ty.rat t
    let to_real t = coerce Ty.real t
    let divisible s t = apply Const.Int.divisible [] [int s; t]
  end

  module Rat = struct
    (* let mk = rat *)
    let minus t = apply Const.Rat.minus [] [t]
    let add a b = apply Const.Rat.add [] [a; b]
    let sub a b = apply Const.Rat.sub [] [a; b]
    let mul a b = apply Const.Rat.mul [] [a; b]
    let div a b = apply Const.Rat.div [] [a; b]
    let div_e a b = apply Const.Rat.div_e [] [a; b]
    let div_t a b = apply Const.Rat.div_t [] [a; b]
    let div_f a b = apply Const.Rat.div_f [] [a; b]
    let rem_e a b = apply Const.Rat.rem_e [] [a; b]
    let rem_t a b = apply Const.Rat.rem_t [] [a; b]
    let rem_f a b = apply Const.Rat.rem_f [] [a; b]
    let lt a b = apply Const.Rat.lt [] [a; b]
    let le a b = apply Const.Rat.le [] [a; b]
    let gt a b = apply Const.Rat.gt [] [a; b]
    let ge a b = apply Const.Rat.ge [] [a; b]
    let floor a = apply Const.Rat.floor [] [a]
    let ceiling a = apply Const.Rat.ceiling [] [a]
    let truncate a = apply Const.Rat.truncate [] [a]
    let round a = apply Const.Rat.round [] [a]
    let is_int a = apply Const.Rat.is_int [] [a]
    let is_rat a = apply Const.Rat.is_rat [] [a]
    let to_int t = coerce Ty.int t
    let to_rat t = coerce Ty.rat t
    let to_real t = coerce Ty.real t
  end

  module Real = struct
    let mk = real
    let minus t = apply Const.Real.minus [] [t]
    let add a b = apply Const.Real.add [] [a; b]
    let sub a b = apply Const.Real.sub [] [a; b]
    let mul a b = apply Const.Real.mul [] [a; b]
    let div a b = apply Const.Real.div [] [a; b]
    let div_e a b = apply Const.Real.div_e [] [a; b]
    let div_t a b = apply Const.Real.div_t [] [a; b]
    let div_f a b = apply Const.Real.div_f [] [a; b]
    let rem_e a b = apply Const.Real.rem_e [] [a; b]
    let rem_t a b = apply Const.Real.rem_t [] [a; b]
    let rem_f a b = apply Const.Real.rem_f [] [a; b]
    let lt a b = apply Const.Real.lt [] [a; b]
    let le a b = apply Const.Real.le [] [a; b]
    let gt a b = apply Const.Real.gt [] [a; b]
    let ge a b = apply Const.Real.ge [] [a; b]
    let floor a = apply Const.Real.floor [] [a]
    let ceiling a = apply Const.Real.ceiling [] [a]
    let truncate a = apply Const.Real.truncate [] [a]
    let round a = apply Const.Real.round [] [a]
    let is_int a = apply Const.Real.is_int [] [a]
    let is_rat a = apply Const.Real.is_rat [] [a]
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

  let select t idx =
    let src, dst = match_array_type t in
    apply Const.select [src; dst] [t; idx]

  let store t idx value =
    let src, dst = match_array_type t in
    apply Const.store [src; dst] [t; idx; value]

  (* Bitvectors *)
  module Bitv = struct
    let match_bitv_type t =
      match ty t with
      | { descr = App ({ builtin = Bitv i; _ }, _); _ } -> i
      | _ -> raise (Wrong_type (t, Ty.bitv 0))

    let mk s = apply (Const.Bitv.bitv s) [] []

    let concat u v =
      let i = match_bitv_type u in
      let j = match_bitv_type v in
      apply (Const.Bitv.concat (i, j)) [] [u; v]

    let extract i j t =
      let n = match_bitv_type t in
      (* TODO: check that i and j are correct index for a bitv(n) *)
      apply (Const.Bitv.extract (i, j, n)) [] [t]

    let repeat k t =
      let n = match_bitv_type t in
      apply (Const.Bitv.repeat (k, n)) [] [t]

    let zero_extend k t =
      let n = match_bitv_type t in
      apply (Const.Bitv.zero_extend (k, n)) [] [t]

    let sign_extend k t =
      let n = match_bitv_type t in
      apply (Const.Bitv.sign_extend (k, n)) [] [t]

    let rotate_right k t =
      let n = match_bitv_type t in
      apply (Const.Bitv.rotate_right (k, n)) [] [t]

    let rotate_left k t =
      let n = match_bitv_type t in
      apply (Const.Bitv.rotate_left (k, n)) [] [t]

    let not t =
      let n = match_bitv_type t in
      apply (Const.Bitv.not n) [] [t]

    let and_ u v =
      let n = match_bitv_type u in
      apply (Const.Bitv.and_ n) [] [u; v]

    let or_ u v =
      let n = match_bitv_type u in
      apply (Const.Bitv.or_ n) [] [u; v]

    let nand u v =
      let n = match_bitv_type u in
      apply (Const.Bitv.nand n) [] [u; v]

    let nor u v =
      let n = match_bitv_type u in
      apply (Const.Bitv.nor n) [] [u; v]

    let xor u v =
      let n = match_bitv_type u in
      apply (Const.Bitv.xor n) [] [u; v]

    let xnor u v =
      let n = match_bitv_type u in
      apply (Const.Bitv.xnor n) [] [u; v]

    let comp u v =
      let n = match_bitv_type u in
      apply (Const.Bitv.comp n) [] [u; v]

    let neg t =
      let n = match_bitv_type t in
      apply (Const.Bitv.neg n) [] [t]

    let add u v =
      let n = match_bitv_type u in
      apply (Const.Bitv.add n) [] [u; v]

    let sub u v =
      let n = match_bitv_type u in
      apply (Const.Bitv.sub n) [] [u; v]

    let mul u v =
      let n = match_bitv_type u in
      apply (Const.Bitv.mul n) [] [u; v]

    let udiv u v =
      let n = match_bitv_type u in
      apply (Const.Bitv.udiv n) [] [u; v]

    let urem u v =
      let n = match_bitv_type u in
      apply (Const.Bitv.urem n) [] [u; v]

    let sdiv u v =
      let n = match_bitv_type u in
      apply (Const.Bitv.sdiv n) [] [u; v]

    let srem u v =
      let n = match_bitv_type u in
      apply (Const.Bitv.srem n) [] [u; v]

    let smod u v =
      let n = match_bitv_type u in
      apply (Const.Bitv.smod n) [] [u; v]

    let shl u v =
      let n = match_bitv_type u in
      apply (Const.Bitv.shl n) [] [u; v]

    let lshr u v =
      let n = match_bitv_type u in
      apply (Const.Bitv.lshr n) [] [u; v]

    let ashr u v =
      let n = match_bitv_type u in
      apply (Const.Bitv.ashr n) [] [u; v]

    let ult u v =
      let n = match_bitv_type u in
      apply (Const.Bitv.ult n) [] [u; v]

    let ule u v =
      let n = match_bitv_type u in
      apply (Const.Bitv.ule n) [] [u; v]

    let ugt u v =
      let n = match_bitv_type u in
      apply (Const.Bitv.ugt n) [] [u; v]

    let uge u v =
      let n = match_bitv_type u in
      apply (Const.Bitv.uge n) [] [u; v]

    let slt u v =
      let n = match_bitv_type u in
      apply (Const.Bitv.slt n) [] [u; v]

    let sle u v =
      let n = match_bitv_type u in
      apply (Const.Bitv.sle n) [] [u; v]

    let sgt u v =
      let n = match_bitv_type u in
      apply (Const.Bitv.sgt n) [] [u; v]

    let sge u v =
      let n = match_bitv_type u in
      apply (Const.Bitv.sge n) [] [u; v]

  end

  module Float = struct
    (* Floats *)
    let match_float_type t =
      match ty t with
      | { descr = App ({ builtin = Float (e,s); _ }, _); _ } -> (e,s)
      | _ -> raise (Wrong_type (t, Ty.float 0 0))

    let fp sign exp significand =
      let e = Bitv.match_bitv_type exp in
      let s = Bitv.match_bitv_type significand in
      apply (Const.Float.fp (e, s+1)) [] [sign; exp; significand]

    let roundNearestTiesToEven = apply Const.Float.roundNearestTiesToEven [] []
    let roundNearestTiesToAway = apply Const.Float.roundNearestTiesToAway [] []
    let roundTowardPositive = apply Const.Float.roundTowardPositive [] []
    let roundTowardNegative = apply Const.Float.roundTowardNegative [] []
    let roundTowardZero = apply Const.Float.roundTowardZero [] []

    let plus_infinity e s = apply (Const.Float.plus_infinity (e,s)) [] []
    let minus_infinity e s = apply (Const.Float.minus_infinity (e,s)) [] []
    let plus_zero e s = apply (Const.Float.plus_zero (e,s)) [] []
    let minus_zero e s = apply (Const.Float.minus_zero (e,s)) [] []
    let nan e s = apply (Const.Float.nan (e,s)) [] []
    let abs x =
      let es = match_float_type x in
      apply (Const.Float.abs es) [] [x]
    let neg x =
      let es = match_float_type x in
      apply (Const.Float.neg es) [] [x]
    let add rm x y =
      let es = match_float_type x in
      apply (Const.Float.add es) [] [rm;x;y]
    let sub rm x y =
      let es = match_float_type x in
      apply (Const.Float.sub es) [] [rm;x;y]
    let mul rm x y =
      let es = match_float_type x in
      apply (Const.Float.mul es) [] [rm;x;y]
    let div rm x y =
      let es = match_float_type x in
      apply (Const.Float.div es) [] [rm;x;y]
    let fma rm x y z =
      let es = match_float_type x in
      apply (Const.Float.fma es) [] [rm;x;y;z]
    let sqrt rm x =
      let es = match_float_type x in
      apply (Const.Float.sqrt es) [] [rm;x]
    let rem x y =
      let es = match_float_type x in
      apply (Const.Float.rem es) [] [x;y]
    let roundToIntegral rm x =
      let es = match_float_type x in
      apply (Const.Float.roundToIntegral es) [] [rm;x]
    let min x y =
      let es = match_float_type x in
      apply (Const.Float.min es) [] [x;y]
    let max x y =
      let es = match_float_type x in
      apply (Const.Float.max es) [] [x;y]
    let leq x y =
      let es = match_float_type x in
      apply (Const.Float.leq es) [] [x;y]
    let lt x y =
      let es = match_float_type x in
      apply (Const.Float.lt es) [] [x;y]
    let geq x y =
      let es = match_float_type x in
      apply (Const.Float.geq es) [] [x;y]
    let gt x y =
      let es = match_float_type x in
      apply (Const.Float.gt es) [] [x;y]
    let eq x y =
      let es = match_float_type x in
      apply (Const.Float.eq es) [] [x;y]
    let isNormal x =
      let es = match_float_type x in
      apply (Const.Float.isNormal es) [] [x]
    let isSubnormal x =
      let es = match_float_type x in
      apply (Const.Float.isSubnormal es) [] [x]
    let isZero x =
      let es = match_float_type x in
      apply (Const.Float.isZero es) [] [x]
    let isInfinite x =
      let es = match_float_type x in
      apply (Const.Float.isInfinite es) [] [x]
    let isNaN x =
      let es = match_float_type x in
      apply (Const.Float.isNaN es) [] [x]
    let isNegative x =
      let es = match_float_type x in
      apply (Const.Float.isNegative es) [] [x]
    let isPositive x =
      let es = match_float_type x in
      apply (Const.Float.isPositive es) [] [x]
    let to_real x =
      let es = match_float_type x in
      apply (Const.Float.to_real es) [] [x]
    let ieee_format_to_fp e s bv =
      apply (Const.Float.ieee_format_to_fp (e,s)) [] [bv]
    let to_fp e2 s2 rm x =
      let (e1,s1) = match_float_type x in
      apply (Const.Float.to_fp (e1,s1,e2,s2)) [] [rm;x]
    let real_to_fp e s rm r =
      apply (Const.Float.real_to_fp (e,s)) [] [rm;r]
    let sbv_to_fp e s rm bv =
      let n = Bitv.match_bitv_type bv in
      apply (Const.Float.sbv_to_fp (n,e,s)) [] [rm;bv]
    let ubv_to_fp e s rm bv =
      let n = Bitv.match_bitv_type bv in
      apply (Const.Float.ubv_to_fp (n,e,s)) [] [rm;bv]
    let to_ubv m rm x =
      let (e,s) = match_float_type x in
      apply (Const.Float.to_ubv (e,s,m)) [] [rm;x]
    let to_sbv m rm x =
      let (e,s) = match_float_type x in
      apply (Const.Float.to_sbv (e,s,m)) [] [rm;x]
  end

  module String = struct

    let of_ustring s = apply (Const.String.string s) [] []
    let length s = apply Const.String.length [] [s]
    let at s i = apply Const.String.at [] [s; i]
    let is_digit s = apply Const.String.is_digit [] [s]
    let to_code s = apply Const.String.to_code [] [s]
    let of_code i = apply Const.String.of_code [] [i]
    let to_int s = apply Const.String.to_int [] [s]
    let of_int i = apply Const.String.of_int [] [i]
    let concat s s' = apply Const.String.concat [] [s;s']
    let sub s i n = apply Const.String.sub [] [s; i; n]
    let index_of s s' i = apply Const.String.index_of [] [s; s'; i]
    let replace s pat by = apply Const.String.replace [] [s; pat; by]
    let replace_all s pat by = apply Const.String.replace_all [] [s; pat; by]
    let replace_re s pat by = apply Const.String.replace_re [] [s; pat; by]
    let replace_re_all s pat by = apply Const.String.replace_re_all [] [s; pat; by]
    let is_prefix s s' = apply Const.String.is_prefix [] [s; s']
    let is_suffix s s' = apply Const.String.is_suffix [] [s; s']
    let contains s s' = apply Const.String.contains [] [s; s']
    let lt s s' = apply Const.String.lt [] [s; s']
    let leq s s' = apply Const.String.leq [] [s; s']
    let in_re s re = apply Const.String.in_re [] [s; re]

    module RegLan = struct
      let empty = apply Const.String.Reg_Lang.empty [] []
      let all = apply Const.String.Reg_Lang.all [] []
      let allchar = apply Const.String.Reg_Lang.allchar [] []
      let of_string s = apply Const.String.Reg_Lang.of_string [] [s]
      let range s s' = apply Const.String.Reg_Lang.range [] [s; s']
      let concat re re' = apply Const.String.Reg_Lang.concat [] [re; re']
      let union re re' = apply Const.String.Reg_Lang.union [] [re; re']
      let inter re re' = apply Const.String.Reg_Lang.inter [] [re; re']
      let diff re re' = apply Const.String.Reg_Lang.diff [] [re; re']
      let star re = apply Const.String.Reg_Lang.star [] [re]
      let cross re = apply Const.String.Reg_Lang.cross [] [re]
      let complement re = apply Const.String.Reg_Lang.complement [] [re]
      let option re = apply Const.String.Reg_Lang.option [] [re]
      let power n re = apply (Const.String.Reg_Lang.power n) [] [re]
      let loop n1 n2 re = apply (Const.String.Reg_Lang.loop (n1, n2)) [] [re]
    end

  end

  (* Wrappers for the tff typechecker *)
  let all _ (tys, ts) body =
    if Ty.(equal prop) (ty body) then mk_bind (Forall (tys, ts)) body
    else raise (Wrong_type (body, Ty.prop))

  let ex _ (tys, ts) body =
    if Ty.(equal prop) (ty body) then mk_bind (Exists (tys, ts)) body
    else raise (Wrong_type (body, Ty.prop))

  let ite cond t_then t_else =
    let ty = ty t_then in
    apply Const.ite [ty] [cond; t_then; t_else]

  (* let-bindings *)

  let bind v t =
    let () = Id.tag v Tags.bound t in
    of_var v

  let letin l body =
    List.iter (fun ((v : Var.t), t) ->
        if not (Ty.equal v.ty (ty t)) then raise (Wrong_type (t, v.ty))
      ) l;
    mk_bind (Letin l) body



end

