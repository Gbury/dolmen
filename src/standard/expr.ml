
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
  descr : ty_descr;
  mutable hash : hash; (* lazy hash *)
  mutable tags : Tag.map;
}

(* Terms and formulas *)
type term_var = ty id

and term_const = (ttype, ty) function_type id

and term_descr =
  | Var of term_var
  | App of term_const * ty list * term list
  | Binder of binder * term

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
type builtin += Prop | Univ
type builtin += Coercion
type builtin +=
  | True | False
  | Equal | Distinct
  | Neg | And | Or
  | Nand | Nor | Xor
  | Imply | Equiv
type builtin += Ite
type builtin +=
  | Constructor of ty_const * int
  | Destructor of ty_const * term_const * int * int

(* arithmetic *)
type builtin +=
  | Int | Integer of string
  | Rat | Rational of string
  | Real | Decimal of string
  | Lt | Leq | Gt | Geq
  | Minus | Add | Sub | Mul
  | Div | Div_e | Div_t | Div_f
  | Modulo | Modulo_t | Modulo_f
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


(* Exceptions *)
(* ************************************************************************* *)

exception Bad_ty_arity of ty_const * ty list
exception Bad_term_arity of term_const * ty list * term list

exception Filter_failed_ty of string * ty
exception Filter_failed_term of string * term

exception Type_already_defined of ty_const
exception Record_type_expected of ty_const

(* Views *)
(* ************************************************************************* *)

module View = struct

  type ty_view =
    | Var of ty_var
    | App of ty_const * ty list
    | Builtin_app of builtin * ty list

  type term_view =
    | Var of term_var
    | App of term_const * ty list * term list
    | Binder of binder * term
    | Builtin_app of builtin * ty list * term list

  let ty (ty : ty) : ty_view =
    match ty.descr with
    | Var v -> Var v
    | App (({ builtin; _ } as c), l) ->
      begin match builtin with
        | Base -> App (c, l)
        | _ -> Builtin_app (builtin, l)
      end

  let term (t : term) : term_view =
    match t.descr with
    | Var v -> Var v
    | App (({ builtin; _ } as c), tys, ts) ->
      begin match builtin with
        | Base -> App (c, tys, ts)
        | _ -> Builtin_app (builtin, tys, ts)
      end
    | Binder (b, t) -> Binder (b, t)

end

(* Flags and filters *)
(* ************************************************************************* *)

module Filter = struct

  type status = [
    | `Pass
    | `Warn
    | `Error
  ]
  type ty_filter = ty_const -> ty list -> status
  type term_filter = term_const -> ty list -> term list -> status

  let ty : (string * bool ref * ty_filter) list tag = Tag.create ()
  let term : (string * bool ref * term_filter) list tag = Tag.create ()

  module Linear = struct

    let active = ref false

    let classify_ty (ty : ty) =
      match View.ty ty with
      | Builtin_app (Int, _) -> `Int
      | Builtin_app (Rat, _) -> `Rat
      | Builtin_app (Real, _) -> `Real
      | _ -> `Other

    let is_ty_int t = (classify_ty t = `Int)
    let is_ty_rat t = (classify_ty t = `Rat)
    let is_ty_real t = (classify_ty t = `Real)

    let classify_term (t : term) =
      match View.term t with
      | Var _ -> `Const
      | Builtin_app (Integer _, _, _) -> `Value `Integer
      | Builtin_app (Rational _, _, _) -> `Value `Rational
      | Builtin_app (Decimal _, _, _) -> `Value `Decimal
      | Builtin_app (Minus, _, [t']) -> `Negated t'
      | Builtin_app (Coercion, [src; dst], [t'])
        when (( is_ty_int src && (is_ty_rat dst || is_ty_real dst) )
              || ( is_ty_rat src &&  is_ty_real dst) ) ->
        `Coerced t'
      (* Rational values *)
      | Builtin_app (Div, _, [a; b]) ->
        `Div (a, b)
      (* Fallback *)
      | _ -> `Other

    let classify t =
      match classify_term t with
      | `Value _ -> `Value
      | `Const -> `Constant
      | `Negated t' ->
        begin match classify_term t' with
          | `Value _ -> `Value
          | _ -> `Other
        end
      | `Coerced t' ->
        begin match classify_term t' with
          | `Value _ -> `Value
          | _ -> `Other
        end
      | `Div (a, b) ->
        begin match classify_term a, classify_term b with
          | `Value `Integer, `Value `Integer -> `Value
          | _ -> `Other
        end
      | _ -> `Other

    let gen_wrapper _ _ _ = `Error

    let div_wrapper _ _ ts =
      match ts with
      | [a; b] ->
        begin match classify_term a, classify_term b with
          | `Value `Integer, `Value `Integer -> `Pass
          | _ -> `Error
        end
      | _ -> `Error

    let mul_wrapper _ _ ts =
      match ts with
      | [a; b] ->
        begin match classify a, classify b with
          | `Value, `Constant
          | `Constant, `Value -> `Pass
          | _ -> `Error
        end
      | _ -> `Error

    let gen = "linear", active, gen_wrapper
    let div = "linear", active, div_wrapper
    let mul = "linear", active, mul_wrapper

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
    match Tag.get v.tags name with
    | Some (Pretty.Exact s | Pretty.Renamed s) -> Format.fprintf fmt "%s" s
    | None ->
      if !print_index then
        Format.fprintf fmt "%s%a" v.name (Fmt.styled (`Fg (`Hi `Black)) pp_index) v
      else
        Format.fprintf fmt "%s" v.name

  let rec ty fmt (t : ty) = match t.descr with
    | Var v -> id fmt v
    | App (f, []) -> id fmt f
    | App (f, l) ->
      begin match Tag.get f.tags pos with
        | None ->
          Format.fprintf fmt "@[<hov 2>%a(%a)@]"
            id f (Format.pp_print_list ~pp_sep:(return ",@ ") ty) l
        | Some Pretty.Prefix ->
          assert (List.length l = 1);
          Format.fprintf fmt "@[<hov 2>%a %a@]"
            id f (Format.pp_print_list ~pp_sep:(return "") ty) l
        | Some Pretty.Infix ->
          assert (List.length l > 1);
          let pp_sep fmt () = Format.fprintf fmt " %a@ " id f in
          Format.fprintf fmt "@[<hov 2>%a@]" (Format.pp_print_list ~pp_sep ty) l
      end

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
    match Tag.get v.tags pos with
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

  and term_app fmt (f : _ id) tys args =
    match Tag.get f.tags pos with
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

  let iter ~sep pp fmt k =
    let first = ref true in
    k (fun x ->
        if !first then first := false else sep fmt ();
        pp fmt x
      )
end

(* Ids *)
(* ************************************************************************* *)

module Id = struct

  type 'a t = 'a id

  (* Usual functions *)
  let hash (v : _ t) = v.index

  let compare v v' = compare v.index v'.index

  let equal v v' = compare v v' = 0

  (* Tags *)
  let get_tag (id : _ id) k = Tag.get id.tags k

  let tag (id : _ id) k v = id.tags <- Tag.add id.tags k v

  (* Creating ids *)
  let id_counter = ref 0

  let mk ?(builtin=Base) ?(tags=Tag.empty) name ty =
    incr id_counter;
    { name; ty; builtin; tags; index = !id_counter; }

  let const
      ?pos ?name ?builtin ?tags
      cname fun_vars fun_args fun_ret =
    let res = mk ?builtin ?tags cname { fun_vars; fun_args; fun_ret; } in
    option_iter (tag res Print.pos) pos;
    option_iter (fun s -> tag res Print.name (Pretty.Exact s)) name;
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

  type subst = (ty_var, ty) Subst.t

  type 'a tag = 'a Tag.t

  (* Types definitions *)

  type adt_case = {
    cstr : term_const;
    dstrs : term_const option array;
  }

  type def =
    | Abstract
    | Adt of {
        ty : ty_const;
        record : bool;
        cstrs : adt_case list;
      }

  let definition_tag : def Tag.t = Tag.create ()

  let definition c = Id.get_tag c definition_tag

  let is_record c =
    match definition c with
    | Some Adt { record; _ } -> record
    | _ -> false

  let define c d =
    match definition c with
    | None -> Id.tag c definition_tag d
    | Some _ -> raise (Type_already_defined c)


  (* Tags *)
  let get_tag (t : t) k = Tag.get t.tags k

  let tag (t : t) k v = t.tags <- Tag.add t.tags k v

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

  let rec compare u v =
    if u == v then 0 else begin
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

  module Var = struct
    type t = ty_var
    let tag = Id.tag
    let hash = Id.hash
    let equal = Id.equal
    let compare = Id.compare
    let get_tag = Id.get_tag
    let mk name = Id.mk name Type
  end

  module Const = struct
    type t = ty_const
    let tag = Id.tag
    let hash = Id.hash
    let equal = Id.equal
    let compare = Id.compare
    let get_tag = Id.get_tag
    let mk name n = Id.const name [] (replicate n Type) Type
    let arity (c : t) = List.length c.ty.fun_args

    let prop = Id.const ~builtin:Prop "Prop" [] [] Type
    let base = Id.const ~builtin:Univ "$i" [] [] Type
    let int = Id.const ~builtin:Int "int" [] [] Type
    let rat = Id.const ~builtin:Rat "rat" [] [] Type
    let real = Id.const ~builtin:Real "real" [] [] Type
    let array = Id.const ~builtin:Array "array" [] [Type; Type] Type
    let bitv =
      with_cache ~cache:(Hashtbl.create 13) (fun i ->
          Id.const ~builtin:(Bitv i) (Format.asprintf "Bitv_%d" i) [] [] Type
        )
  end

  let mk descr = { descr; hash = -1; tags = Tag.empty; }

  let of_var v = mk (Var v)

  let wildcard () = of_var (Id.mk ~builtin:Wildcard "_" Type)

  let rec check_filters res f args = function
    | [] -> res
    | (name, active, check) :: r ->
      if !active then match (check f args) with
        | `Pass -> check_filters res f args r
        | `Warn -> check_filters res f args r
        | `Error -> raise (Filter_failed_ty (name, res))
      else
        check_filters res f args r

  let apply (f : Const.t) (args : ty list) =
    assert (f.ty.fun_vars = []);
    if List.length args <> List.length f.ty.fun_args then
      raise (Bad_ty_arity (f, args))
    else begin
      let res = mk (App (f, args)) in
      match Const.get_tag f Filter.ty with
      | None -> res
      | Some l -> check_filters res f args l
    end

  (* Builtin prop *)
  let prop = apply Const.prop []
  let base = apply Const.base []
  let int = apply Const.int []
  let rat = apply Const.rat []
  let real = apply Const.real []
  let array src dst = apply Const.array [src; dst]
  let bitv i = apply (Const.bitv i) []

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

  let rec robinson subst s t =
    let s = follow subst s in
    let t = follow subst t in
    match s, t with
    | ({ descr = Var ({ builtin = Wildcard; _ } as v); _ } as m), u
    | u, ({ descr = Var ({ builtin = Wildcard; _ } as v); _ } as m) ->
      if equal m u then
        subst
      else if occurs subst [v] u then
        raise (Impossible_unification (m, u))
      else
        Subst.Var.bind subst v u
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
  exception Wrong_record_type of term_const * ty_const
  exception Field_repeated of term_const
  exception Field_missing of term_const
  exception Field_expected of term_const

  let print = Print.term

  (* Tags *)
  let get_tag (t : t) k = Tag.get t.tags k

  let tag (t : t) k v = t.tags <- Tag.add t.tags k v

  (* Hash *)
  let rec hash_aux t =
    match t.descr with
    | Var v -> hash2 3 (Id.hash v)
    | App (f, tys, args) ->
      hash4 5 (Id.hash f) (hash_list Ty.hash tys) (hash_list hash args)
    | Binder (b, body) ->
      hash3 7 (hash_binder b) (hash body)

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

  (* Comparison *)
  let discr t =
    match t.descr with
    | Var _ -> 1
    | App _ -> 2
    | Binder _ -> 3

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

  let equal u v = compare u v = 0

  (* Inspection *)
  let ty { ty; _ } = ty

  (* Variables *)
  module Var = struct
    type t = term_var
    let tag = Id.tag
    let hash = Id.hash
    let equal = Id.equal
    let compare = Id.compare
    let get_tag = Id.get_tag
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
    let mk name vars args ret = Id.const name vars args ret
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

    let linear_gen_tags =
      Tag.(add empty) Filter.term [Filter.Linear.gen]

    let linear_div_tags =
      Tag.(add empty) Filter.term [Filter.Linear.div]

    let linear_mul_tags =
      Tag.(add empty) Filter.term [Filter.Linear.mul]

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
          ~pos:Pretty.Infix ~name:"*"
          ~builtin:Mul ~tags:linear_mul_tags
          "Mul" [] [Ty.int; Ty.int] Ty.int

      let div_e = Id.const
          ~pos:Pretty.Infix ~name:"/e"
          ~builtin:Div_e ~tags:linear_gen_tags
          "Div_e" [] [Ty.int; Ty.int] Ty.int
      let div_t = Id.const
          ~pos:Pretty.Infix ~name:"/t"
          ~builtin:Div_t ~tags:linear_gen_tags
          "Div_t" [] [Ty.int; Ty.int] Ty.int
      let div_f = Id.const
          ~pos:Pretty.Infix ~name:"/f"
          ~builtin:Div_f ~tags:linear_gen_tags
          "Div_f" [] [Ty.int; Ty.int] Ty.int

      let rem_e = Id.const
          ~pos:Pretty.Infix ~name:"%"
          ~builtin:Modulo ~tags:linear_gen_tags
          "Modulo" [] [Ty.int; Ty.int] Ty.int
      let rem_t = Id.const
          ~pos:Pretty.Infix ~name:"%"
          ~builtin:Modulo_t ~tags:linear_gen_tags
          "Modulo" [] [Ty.int; Ty.int] Ty.int
      let rem_f = Id.const
          ~pos:Pretty.Infix ~name:"%"
          ~builtin:Modulo_f ~tags:linear_gen_tags
          "Modulo" [] [Ty.int; Ty.int] Ty.int

      let abs = Id.const
          ~name:"abs" ~builtin:Abs
          ~tags:linear_gen_tags
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
          ~tags:linear_gen_tags
          "Floor" [] [Ty.int] Ty.int

      let ceiling = Id.const
          ~name:"ceiling" ~builtin:Ceiling
          ~tags:linear_gen_tags
          "Ceiling" [] [Ty.int] Ty.int

      let truncate = Id.const
          ~name:"truncate" ~builtin:Truncate
          ~tags:linear_gen_tags
          "Truncate" [] [Ty.int] Ty.int

      let round = Id.const
          ~name:"round" ~builtin:Round
          ~tags:linear_gen_tags
          "Round" [] [Ty.int] Ty.int

      let is_int = Id.const
          ~name:"is_int" ~builtin:Is_int
          "Is_int" [] [Ty.int] Ty.prop

      let is_rat = Id.const
          ~name:"is_rat" ~builtin:Is_rat
          "Is_rat" [] [Ty.int] Ty.prop

      let divisible = Id.const
          ~builtin:Divisible "Divisible" [] [Ty.int; Ty.int] Ty.int

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
          ~pos:Pretty.Infix ~name:"*"
          ~builtin:Mul ~tags:linear_mul_tags
          "Mul" [] [Ty.rat; Ty.rat] Ty.rat

      let div = Id.const
          ~pos:Pretty.Infix ~name:"/"
          ~builtin:Div ~tags:linear_div_tags
          "Div" [] [Ty.rat; Ty.rat] Ty.rat
      let div_e = Id.const
          ~pos:Pretty.Infix ~name:"/e"
          ~builtin:Div_e ~tags:linear_gen_tags
          "Div_e" [] [Ty.rat; Ty.rat] Ty.rat
      let div_t = Id.const
          ~pos:Pretty.Infix ~name:"/t"
          ~builtin:Div_t ~tags:linear_gen_tags
          "Div_t" [] [Ty.rat; Ty.rat] Ty.rat
      let div_f = Id.const
          ~pos:Pretty.Infix ~name:"/f"
          ~builtin:Div_f ~tags:linear_gen_tags
          "Div_f" [] [Ty.rat; Ty.rat] Ty.rat

      let rem_e = Id.const
          ~pos:Pretty.Infix ~name:"%"
          ~builtin:Modulo ~tags:linear_gen_tags
          "Modulo" [] [Ty.rat; Ty.rat] Ty.rat
      let rem_t = Id.const
          ~pos:Pretty.Infix ~name:"%"
          ~builtin:Modulo_t ~tags:linear_gen_tags
          "Modulo" [] [Ty.rat; Ty.rat] Ty.rat
      let rem_f = Id.const
          ~pos:Pretty.Infix ~name:"%"
          ~builtin:Modulo_f ~tags:linear_gen_tags
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
          ~tags:linear_gen_tags
          "Floor" [] [Ty.rat] Ty.rat

      let ceiling = Id.const
          ~name:"ceiling" ~builtin:Ceiling
          ~tags:linear_gen_tags
          "Ceiling" [] [Ty.rat] Ty.rat

      let truncate = Id.const
          ~name:"truncate" ~builtin:Truncate
          ~tags:linear_gen_tags
          "Truncate" [] [Ty.rat] Ty.rat

      let round = Id.const
          ~name:"round" ~builtin:Round
          ~tags:linear_gen_tags
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
          ~pos:Pretty.Infix ~name:"*"
          ~builtin:Mul ~tags:linear_mul_tags
          "Mul" [] [Ty.real; Ty.real] Ty.real

      let div = Id.const
          ~pos:Pretty.Infix ~name:"/"
          ~builtin:Div ~tags:linear_div_tags
          "Div" [] [Ty.real; Ty.real] Ty.real
      let div_e = Id.const
          ~pos:Pretty.Infix ~name:"/"
          ~builtin:Div_e ~tags:linear_gen_tags
          "Div_e" [] [Ty.real; Ty.real] Ty.real
      let div_t = Id.const
          ~pos:Pretty.Infix ~name:"/t"
          ~builtin:Div_t ~tags:linear_gen_tags
          "Div_t" [] [Ty.real; Ty.real] Ty.real
      let div_f = Id.const
          ~pos:Pretty.Infix ~name:"/f"
          ~builtin:Div_f ~tags:linear_gen_tags
          "Div_f" [] [Ty.real; Ty.real] Ty.real

      let rem_e = Id.const
          ~pos:Pretty.Infix ~name:"%"
          ~builtin:Modulo ~tags:linear_gen_tags
          "Modulo" [] [Ty.real; Ty.real] Ty.real
      let rem_t = Id.const
          ~pos:Pretty.Infix ~name:"%"
          ~builtin:Modulo_t ~tags:linear_gen_tags
          "Modulo" [] [Ty.real; Ty.real] Ty.real
      let rem_f = Id.const
          ~pos:Pretty.Infix ~name:"%"
          ~builtin:Modulo_f ~tags:linear_gen_tags
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
          ~tags:linear_gen_tags
          "Floor" [] [Ty.real] Ty.real

      let ceiling = Id.const
          ~name:"ceiling" ~builtin:Ceiling
          ~tags:linear_gen_tags
          "Ceiling" [] [Ty.real] Ty.real

      let truncate = Id.const
          ~name:"truncate" ~builtin:Truncate
          ~tags:linear_gen_tags
          "Truncate" [] [Ty.real] Ty.real

      let round = Id.const
          ~name:"round" ~builtin:Round
          ~tags:linear_gen_tags
          "Round" [] [Ty.real] Ty.real

      let is_int = Id.const
          ~name:"is_int" ~builtin:Is_int
          "Is_int" [] [Ty.real] Ty.prop

      let is_rat = Id.const
          ~name:"is_rat" ~builtin:Is_rat
          "Is_rat" [] [Ty.real] Ty.prop
    end

    let bitv s =
      Id.const ~builtin:(Bitvec s)
        (Format.asprintf "bv#%s#" s) [] [] (Ty.bitv (String.length s))

    let bitv_concat =
      with_cache ~cache:(Hashtbl.create 13) (fun (i, j) ->
          Id.const ~builtin:Bitv_concat "bitv_concat"
            [] [Ty.bitv i; Ty.bitv j] (Ty.bitv (i + j))
        )

    let bitv_extract =
      with_cache ~cache:(Hashtbl.create 13) (fun (i, j, n) ->
          Id.const ~builtin:(Bitv_extract (j, i))
            (Format.asprintf "bitv_extract_%d_%d" i j) []
            [Ty.bitv n] (Ty.bitv (i - j + 1))
        )

    let bitv_repeat =
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

    let bvnot =
      with_cache ~cache:(Hashtbl.create 13) (fun n ->
          Id.const ~builtin:Bitv_not "bvnot" [] [Ty.bitv n] (Ty.bitv n)
        )

    let bvand =
      with_cache ~cache:(Hashtbl.create 13) (fun n ->
          Id.const ~builtin:Bitv_and "bvand" [] [Ty.bitv n] (Ty.bitv n)
        )

    let bvor =
      with_cache ~cache:(Hashtbl.create 13) (fun n ->
          Id.const ~builtin:Bitv_or "bvor" [] [Ty.bitv n] (Ty.bitv n)
        )

    let bvnand =
      with_cache ~cache:(Hashtbl.create 13) (fun n ->
          Id.const ~builtin:Bitv_nand "bvnand" [] [Ty.bitv n] (Ty.bitv n)
        )

    let bvnor =
      with_cache ~cache:(Hashtbl.create 13) (fun n ->
          Id.const ~builtin:Bitv_nor "bvnor" [] [Ty.bitv n] (Ty.bitv n)
        )

    let bvxor =
      with_cache ~cache:(Hashtbl.create 13) (fun n ->
          Id.const ~builtin:Bitv_xor "bvxor" [] [Ty.bitv n] (Ty.bitv n)
        )

    let bvxnor =
      with_cache ~cache:(Hashtbl.create 13) (fun n ->
          Id.const ~builtin:Bitv_xnor "bvxnor" [] [Ty.bitv n] (Ty.bitv n)
        )

    let bvcomp =
      with_cache ~cache:(Hashtbl.create 13) (fun n ->
          Id.const ~builtin:Bitv_comp "bvcomp" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv 1)
        )

    let bvneg =
      with_cache ~cache:(Hashtbl.create 13) (fun n ->
          Id.const ~builtin:Bitv_neg "bvneg" [] [Ty.bitv n] (Ty.bitv n)
        )

    let bvadd =
      with_cache ~cache:(Hashtbl.create 13) (fun n ->
          Id.const ~builtin:Bitv_add "bvadd" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
        )

    let bvsub =
      with_cache ~cache:(Hashtbl.create 13) (fun n ->
          Id.const ~builtin:Bitv_sub "bvsub" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
        )

    let bvmul =
      with_cache ~cache:(Hashtbl.create 13) (fun n ->
          Id.const ~builtin:Bitv_mul "bvmul" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
        )

    let bvudiv =
      with_cache ~cache:(Hashtbl.create 13) (fun n ->
          Id.const ~builtin:Bitv_udiv "bvudiv" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
        )

    let bvurem =
      with_cache ~cache:(Hashtbl.create 13) (fun n ->
          Id.const ~builtin:Bitv_urem "bvurem" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
        )

    let bvsdiv =
      with_cache ~cache:(Hashtbl.create 13) (fun n ->
          Id.const ~builtin:Bitv_sdiv "bvsdiv" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
        )

    let bvsrem =
      with_cache ~cache:(Hashtbl.create 13) (fun n ->
          Id.const ~builtin:Bitv_srem "bvsrem" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
        )

    let bvsmod =
      with_cache ~cache:(Hashtbl.create 13) (fun n ->
          Id.const ~builtin:Bitv_smod "bvsmod" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
        )

    let bvshl =
      with_cache ~cache:(Hashtbl.create 13) (fun n ->
          Id.const ~builtin:Bitv_shl "bvshl" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
        )

    let bvlshr =
      with_cache ~cache:(Hashtbl.create 13) (fun n ->
          Id.const ~builtin:Bitv_lshr "bvlshr" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
        )

    let bvashr =
      with_cache ~cache:(Hashtbl.create 13) (fun n ->
          Id.const ~builtin:Bitv_ashr "bvashr" [] [Ty.bitv n; Ty.bitv n] (Ty.bitv n)
        )

    let bvult =
      with_cache ~cache:(Hashtbl.create 13) (fun n ->
          Id.const ~builtin:Bitv_ult "bvult" [] [Ty.bitv n; Ty.bitv n] Ty.prop
        )

    let bvule =
      with_cache ~cache:(Hashtbl.create 13) (fun n ->
          Id.const ~builtin:Bitv_ule "bvule" [] [Ty.bitv n; Ty.bitv n] Ty.prop
        )

    let bvugt =
      with_cache ~cache:(Hashtbl.create 13) (fun n ->
          Id.const ~builtin:Bitv_ugt "bvugt" [] [Ty.bitv n; Ty.bitv n] Ty.prop
        )

    let bvuge =
      with_cache ~cache:(Hashtbl.create 13) (fun n ->
          Id.const ~builtin:Bitv_uge "bvsge" [] [Ty.bitv n; Ty.bitv n] Ty.prop
        )

    let bvslt =
      with_cache ~cache:(Hashtbl.create 13) (fun n ->
          Id.const ~builtin:Bitv_slt "bvslt" [] [Ty.bitv n; Ty.bitv n] Ty.prop
        )

    let bvsle =
      with_cache ~cache:(Hashtbl.create 13) (fun n ->
          Id.const ~builtin:Bitv_sle "bvsle" [] [Ty.bitv n; Ty.bitv n] Ty.prop
        )

    let bvsgt =
      with_cache ~cache:(Hashtbl.create 13) (fun n ->
          Id.const ~builtin:Bitv_sgt "bvsgt" [] [Ty.bitv n; Ty.bitv n] Ty.prop
        )

    let bvsge =
      with_cache ~cache:(Hashtbl.create 13) (fun n ->
          Id.const ~builtin:Bitv_sge "bvsge" [] [Ty.bitv n; Ty.bitv n] Ty.prop
        )

  end

  (* Constructors are simply constants *)
  module Cstr = struct
    type t = term_const
    let tag = Id.tag
    let hash = Id.hash
    let equal = Id.equal
    let compare = Id.compare
    let get_tag = Id.get_tag
    let mk ty_c name i vars args ret =
      Id.const ~builtin:(Constructor (ty_c, i)) name vars args ret
    let arity (c : t) =
      List.length c.ty.fun_vars, List.length c.ty.fun_args
  end

  (* Record fields are represented as their destructors, i.e. constants *)
  module Field = struct
    type t = term_const
    let hash = Id.hash
    let equal = Id.equal
  end

  (* ADT definition *)
  let define_adt_aux ~record ty_const vars l =
    let ty =  Ty.apply ty_const (List.map Ty.of_var vars) in
    let cases = ref [] in
    let l' = List.mapi (fun i (cstr_name, args) ->
        let args_ty = List.map fst args in
        let cstr = Cstr.mk ty_const cstr_name i vars args_ty ty in
        let dstrs = Array.make (List.length args) None in
        let l' = List.mapi (fun j -> function
            | (arg_ty, None) -> (arg_ty, None)
            | (arg_ty, Some name) ->
              let dstr =
                Id.const
                  ~builtin:(Destructor (ty_const, cstr, i, j))
                  name vars [ty] arg_ty
              in
              dstrs.(i) <- Some dstr;
              (arg_ty, Some dstr)
          ) args in
        cases := { Ty.cstr; dstrs; } :: !cases;
        cstr, l'
      ) l in
    assert (not record || List.length !cases = 1);
    Ty.define ty_const (Adt { ty = ty_const; record; cstrs = List.rev !cases; });
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

  (* Filter check *)
  let rec check_filters res f tys args = function
    | [] -> res
    | (name, active, check) :: r ->
      if !active then match (check f tys args) with
        | `Pass -> check_filters res f tys args r
        | `Warn -> check_filters res f tys args r
        | `Error -> raise (Filter_failed_term (name, res))
      else
        check_filters res f tys args r

  (* Term creation *)
  let mk ?(tags=Tag.empty) descr ty = { descr; ty; hash = -1; tags; }

  let of_var v = mk (Var v) v.ty

  (* This function does not check types enough, do not export outside the module *)
  let bind b body = mk (Binder (b, body)) (ty body)

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
      bind b' (subst_aux ~fix ty_var_map t_var_map body)

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

  and subst ?(fix=true) ty_var_map t_var_map t =
    if Subst.is_empty ty_var_map && Subst.is_empty t_var_map then
      t
    else
      subst_aux ~fix ty_var_map t_var_map t

  (* Application typechecking *)
  and instantiate (f : term_const) tys args =
    if List.length f.ty.fun_vars <> List.length tys ||
       List.length f.ty.fun_args <> List.length args then
      raise (Bad_term_arity (f, tys, args))
    else begin
      let map = List.fold_left2 Subst.Var.bind Subst.empty f.ty.fun_vars tys in
      let expected_types = List.map (Ty.subst map) f.ty.fun_args in
      let s = List.fold_left2 (fun s expected term ->
          try Ty.robinson s expected (ty term)
          with Ty.Impossible_unification _ -> raise (Wrong_type (term, expected))
        ) map expected_types args
      in
      let actual_ty_args = List.map (Ty.subst s) tys in
      let actual_args = List.map (subst s Subst.empty) args in
      actual_ty_args, actual_args, Ty.subst s f.ty.fun_ret
    end

  (* Application *)
  and apply f tys args =
    let tys, args, ret = instantiate f tys args in
    let res = mk (App (f, tys, args)) ret in
    match Const.get_tag f Filter.term with
    | None -> res
    | Some l -> check_filters res f tys args l

  let apply_cstr = apply

  let apply_field (f : term_const) t =
    let tys = init_list
        (List.length f.ty.fun_vars)
        (fun _ -> Ty.wildcard ())
    in
    apply f tys [t]

  (* Record field getter *)
  let record_field ty_c i =
    match Ty.definition ty_c with
    | Some Adt { record = true; cstrs = [ { dstrs; _ } ]; _ } ->
      begin match dstrs.(i) with
        | Some c -> c
        | None -> assert false
      end
    | _ ->
      raise (Record_type_expected ty_c)

  (* Record creation *)
  let record_field_id ty_c f =
    match f.builtin with
    | Destructor (ty_d, _, i, j) ->
      if Id.equal ty_c ty_d then begin
        assert (i = 0);
        j
      end else
        raise (Wrong_record_type (f, ty_c))
    | _ ->
      raise (Field_expected f)

  let build_record_fields ty_c l =
    let n =
      match Ty.definition ty_c with
      | Some Adt { record = true; cstrs = [ { dstrs; _ } ]; _ } ->
        Array.length dstrs
      | _ -> raise (Record_type_expected ty_c)
    in
    let fields = Array.make n None in
    List.iter (fun (field, value) ->
        let i = record_field_id ty_c field in
        match fields.(i) with
        | Some _ -> raise (Field_repeated field)
        | None -> fields.(i) <- Some value
      ) l;
    fields

  let mk_record missing = function
    | [] -> raise (Invalid_argument "Dolmen.Expr.record")
    | ((f, _) :: _) as l ->
      begin match f.builtin with
        | Destructor (ty_c, c, _, _) when Ty.is_record ty_c ->
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
    mk_record (fun ty_c i -> raise (Field_missing (record_field ty_c i))) l

  let record_with t = function
    | [] -> t
    | l ->
      let aux ty_c i =
        let f = record_field ty_c i in
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
    let int = int
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
    let real = real
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
  let match_bitv_type t =
    match ty t with
    | { descr = App ({ builtin = Bitv i; _ }, _); _ } -> i
    | _ -> raise (Wrong_type (t, Ty.bitv 0))

  let mk_bitv s = apply (Const.bitv s) [] []

  let bitv_concat u v =
    let i = match_bitv_type u in
    let j = match_bitv_type v in
    apply (Const.bitv_concat (i, j)) [] [u; v]

  let bitv_extract i j t =
    let n = match_bitv_type t in
    apply (Const.bitv_extract (i, j, n)) [] [t]

  let bitv_repeat k t =
    let n = match_bitv_type t in
    apply (Const.bitv_repeat (k, n)) [] [t]

  let zero_extend k t =
    let n = match_bitv_type t in
    apply (Const.zero_extend (k, n)) [] [t]

  let sign_extend k t =
    let n = match_bitv_type t in
    apply (Const.sign_extend (k, n)) [] [t]

  let rotate_right k t =
    let n = match_bitv_type t in
    apply (Const.rotate_right (k, n)) [] [t]

  let rotate_left k t =
    let n = match_bitv_type t in
    apply (Const.rotate_left (k, n)) [] [t]

  let bvnot t =
    let n = match_bitv_type t in
    apply (Const.bvnot n) [] [t]

  let bvand u v =
    let n = match_bitv_type u in
    apply (Const.bvand n) [] [u; v]

  let bvor u v =
    let n = match_bitv_type u in
    apply (Const.bvor n) [] [u; v]

  let bvnand u v =
    let n = match_bitv_type u in
    apply (Const.bvnand n) [] [u; v]

  let bvnor u v =
    let n = match_bitv_type u in
    apply (Const.bvnor n) [] [u; v]

  let bvxor u v =
    let n = match_bitv_type u in
    apply (Const.bvxor n) [] [u; v]

  let bvxnor u v =
    let n = match_bitv_type u in
    apply (Const.bvxnor n) [] [u; v]

  let bvcomp u v =
    let n = match_bitv_type u in
    apply (Const.bvcomp n) [] [u; v]

  let bvneg t =
    let n = match_bitv_type t in
    apply (Const.bvneg n) [] [t]

  let bvadd u v =
    let n = match_bitv_type u in
    apply (Const.bvadd n) [] [u; v]

  let bvsub u v =
    let n = match_bitv_type u in
    apply (Const.bvsub n) [] [u; v]

  let bvmul u v =
    let n = match_bitv_type u in
    apply (Const.bvmul n) [] [u; v]

  let bvudiv u v =
    let n = match_bitv_type u in
    apply (Const.bvudiv n) [] [u; v]

  let bvurem u v =
    let n = match_bitv_type u in
    apply (Const.bvurem n) [] [u; v]

  let bvsdiv u v =
    let n = match_bitv_type u in
    apply (Const.bvsdiv n) [] [u; v]

  let bvsrem u v =
    let n = match_bitv_type u in
    apply (Const.bvsrem n) [] [u; v]

  let bvsmod u v =
    let n = match_bitv_type u in
    apply (Const.bvsmod n) [] [u; v]

  let bvshl u v =
    let n = match_bitv_type u in
    apply (Const.bvshl n) [] [u; v]

  let bvlshr u v =
    let n = match_bitv_type u in
    apply (Const.bvlshr n) [] [u; v]

  let bvashr u v =
    let n = match_bitv_type u in
    apply (Const.bvashr n) [] [u; v]

  let bvult u v =
    let n = match_bitv_type u in
    apply (Const.bvult n) [] [u; v]

  let bvule u v =
    let n = match_bitv_type u in
    apply (Const.bvule n) [] [u; v]

  let bvugt u v =
    let n = match_bitv_type u in
    apply (Const.bvugt n) [] [u; v]

  let bvuge u v =
    let n = match_bitv_type u in
    apply (Const.bvuge n) [] [u; v]

  let bvslt u v =
    let n = match_bitv_type u in
    apply (Const.bvslt n) [] [u; v]

  let bvsle u v =
    let n = match_bitv_type u in
    apply (Const.bvsle n) [] [u; v]

  let bvsgt u v =
    let n = match_bitv_type u in
    apply (Const.bvsgt n) [] [u; v]

  let bvsge u v =
    let n = match_bitv_type u in
    apply (Const.bvsge n) [] [u; v]


  (* Wrappers for the tff typechecker *)
  let all _ (tys, ts) body =
    if Ty.(equal prop) (ty body) then bind (Forall (tys, ts)) body
    else raise (Wrong_type (body, Ty.prop))

  let ex _ (tys, ts) body =
    if Ty.(equal prop) (ty body) then bind (Exists (tys, ts)) body
    else raise (Wrong_type (body, Ty.prop))

  let letin l body =
    List.iter (fun ((v : Var.t), t) ->
        if not (Ty.equal v.ty (ty t)) then raise (Wrong_type (t, v.ty))
      ) l;
    bind (Letin l) body

  let ite cond t_then t_else =
    let ty = ty t_then in
    apply Const.ite [ty] [cond; t_then; t_else]


  (** free variables *)
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

  let fv t =
    let s = free_vars FV.empty t in
    FV.to_list s


end

