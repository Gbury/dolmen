
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
  | Constructor of ty_const * int
  | Destructor of ty_const * term_const * int * int

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

  let rec ty fmt (t : ty) = match t.descr with
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
          | Base -> `App (`Generic c, l)
          | _ -> `App (`Builtin builtin, l)
        end

  end

  module Term = struct

    type t = [
      | `Var of term_var
      | `Binder of binder * term
      | `App of [
          | `Generic of term_const
          | `Builtin of builtin
        ] * ty list * term list
    ]

    let view (t : term) : t =
      match t.descr with
      | Var v -> `Var v
      | App (({ builtin; _ } as c), tys, ts) ->
        begin match builtin with
          | Base -> `App (`Generic c, tys, ts)
          | _ -> `App (`Builtin builtin, tys, ts)
        end
      | Binder (b, t) -> `Binder (b, t)

  end

  module Arith = struct

    module Shallow = struct

      (* Shallow classification of terms into:
         - variables
         - constants
         - arithmetic literals aka values
         - exprssion with an arithmetic operation as head symbol
         - other expressions *)
      type arith_ty = [
        | `Int
        | `Rat
        | `Real
      ]

      type 'a t = [
        | `Var of term_var
        | `Cst of term_const
        | `Value of arith_ty * string
        | `Arith of builtin * 'a list
        | `Other
      ]

      let rec view (t : term) : term t =
        match Term.view t with
        | `Var v ->
          begin match Tag.last v.tags Tags.bound with
            | None -> `Var v
            | Some t' -> view t'
          end
        | `App (`Builtin Integer s, _, _) -> `Value (`Int, s)
        | `App (`Builtin Rational s, _, _) -> `Value (`Rat, s)
        | `App (`Builtin Decimal s, _, _) -> `Value (`Real, s)
        | `App (`Builtin Coercion, [src; dst], [t']) ->
          (* Only match lossless coercions, that likely have been added
             as syntactic sugar because of syntax restrictions, e.g.
             to allow integeer literals to be used in rational/real
             equations.
             For these coercions, simply ignore the coercion and
             return the view of the coerced term. *)
          begin match Ty.view src, Ty.view dst with
            | `Int, ((`Rat | `Real) as dst)
            | `Rat, (`Real as dst)
              -> begin match view t' with
                  | `Value (_, s) -> `Value (dst, s)
                  | res -> res
                end
            | _ -> `Other
          end
        (* Other Arithmetic constructions *)
        | `App (`Builtin (Minus | Add | Sub | Mul | Div | Abs
                         | Div_e | Div_t | Div_f |
                          Modulo_e | Modulo_t | Modulo_f as b), _, l) ->
          `Arith (b, l)
        (* Constants *)
        | `App (`Generic c, [], []) -> `Cst c
        (* Fallback *)
        | _ -> `Other

      (* Error messages for filters that fail *)
      let print fmt v =
        match v with
        | `Var v ->
          Format.fprintf fmt "the variable %a" Print.id v
        | `Cst c ->
          Format.fprintf fmt "the constant %a" Print.id c
        | `Value (`Int, _) ->
          Format.fprintf fmt "an integer literal"
        | `Value (`Rat, _) ->
          Format.fprintf fmt "a rational literal"
        | `Value (`Real, _) ->
          Format.fprintf fmt "a real literal"
        | `Arith (Minus, _) ->
          Format.fprintf fmt "a unary negation"
        | `Arith (Add, _) ->
          Format.fprintf fmt "an addition"
        | `Arith (Sub, _) ->
          Format.fprintf fmt "a substraction"
        | `Arith (Mul, _) ->
          Format.fprintf fmt "a multiplication"
        | `Arith (Div, _) ->
          Format.fprintf fmt "an exact division"
        | `Arith (Abs, _) ->
          Format.fprintf fmt "an absolute value"
        | `Arith (Div_e, _) ->
          Format.fprintf fmt "a euclidian division"
        | `Arith (Div_t, _) ->
          Format.fprintf fmt "a truncated division"
        | `Arith (Div_f, _) ->
          Format.fprintf fmt "a floored division"
        | `Arith (Modulo_e, _) ->
          Format.fprintf fmt "a euclidian remainder"
        | `Arith (Modulo_t, _) ->
          Format.fprintf fmt "a truncated remainder"
        | `Arith (Modulo_f, _) ->
          Format.fprintf fmt "a floored remainder"
        | `Arith _ ->
          Format.fprintf fmt "an unknown complex arithmetic expression \
                              (please report upstream, ^^)"
        | `Other ->
          Format.fprintf fmt "an arbitrary term"

      let expect_error v v' expected =
        Format.asprintf "expects %s but was given:\n- %a\n- %a"
          expected print v print v'

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

  module Quantifier = struct

    let active = ref false
    let name = "quantifier"
    let reset () = active := false

  end

  module Smtlib2 = struct

    module Linear_strict = struct

      let active = ref false
      let name = "linear"
      let reset () = active := false

      (* Local view of arithmetic expressions in the terms of the smtlib
         spec, in:
         + Free constants (no need to check their types)
         + Integer coeficients, i.e.:
         - either raw integer literals, or
         - the negation of a raw integer literal
           + Rational coeficients (but are used for Real SMTLIb arithmetic,
           since there is no rational arithmetci in SMTLIB contrary to
           TPTP), which are:
         - a raw decimal literal
         - the negation of a raw decimal literal
         - a division of an integer coeficient by a strictly positive
             integer literal (i.e. non-zero, since integer literals in SMTLIB
             cannot have a minus sign, and hence are always non-negative).
           + Other expressions *)
      let view (t : term) =
        match View.Arith.Shallow.view t with
        | `Cst _ -> `Symbol
        | `Value (`Int, _) -> `Int_coef
        | `Value (`Real, _) -> `Real_coef
        | `Arith (Minus, [t']) ->
          begin match View.Arith.Shallow.view t' with
            | `Value (`Int, _) -> `Int_coef
            | `Value (`Real, _) -> `Real_coef
            | _ -> `Other
          end
        (* We can assume the filters for linear arithmetic are used, hence
           a division must conform to the {div} filter, and thus can only
           be a rational/real coeficient *)
        | `Arith (Div, _) -> `Real_coef
        (* Anything else is in the other category *)
        | _ -> `Other

      (* Generic wrapper for arithmetic operations not permitted
         in linear arithmetic *)
      let forbidden_wrapper _ _ _ =
        `Error "this operator is forbidden in linear arithmetic"

      (* Division wrapper *)

      let div_wrapper _ _ ts =
        match ts with
        | [a; b] ->
          begin match view a with
            | `Int_coef ->
              begin match View.Arith.Shallow.view b with
                | `Value (`Int, "0") -> `Error (
                    Format.asprintf "division in linear arithmetic \
                                     expects a non-zero denominator")
                | `Value (`Int, _) -> `Pass
                | v -> `Error (
                    Format.asprintf "division in linear arithmetic \
                                     expects a constant positive \
                                     integer literal as denominator, \
                                     but was given %a"
                      View.Arith.Shallow.print v)
              end
            | _ -> `Error (
                Format.asprintf "division in linear arithmetic \
                                 expects as first argument an integer \
                                 coeficient, i.e. either a raw integer \
                                 literal, or the negation of one, but \
                                 here was given a %a"
                  View.Arith.Shallow.print (View.Arith.Shallow.view a))
          end
        | _ -> `Error "bad arity for multiplication"


      (* Multiplication wrappers *)

      let mul_wrapper _ _ ts =
        match ts with
        | [a; b] ->
          begin match view a, view b with
            | (`Int_coef | `Real_coef), `Symbol
            | `Symbol, (`Int_coef | `Real_coef)
              -> `Pass
            | _ -> `Error (
                Format.asprintf "multiplication in linear arithmetic %s"
                  (View.Arith.Shallow.(expect_error (view a) (view b)
                                         "an integer or rational literal \
                                          and a symbol (variable or constant)")))
          end
        | _ -> `Error "bad arity for multiplication"

      let div = name, active, div_wrapper
      let mul = name, active, mul_wrapper
      let forbidden = name, active, forbidden_wrapper

    end

    module Linear_large = struct

      let active = ref false
      let name = "linear"
      let reset () = active := false

      (* Local view of arithmetic expressions in the terms of the smtlib
         spec, in:
         + Generic terms allowed to be multiplied, i.e.:
           - constants
           - term with top symbol not in Ints (i.e. not an Int builtin operation)
         + Integer coeficients, i.e.:
           - either raw integer literals, or
           - the negation of a raw integer literal
         + Rational coeficients (but are used for Real SMTLIb arithmetic,
           since there is no rational arithmetci in SMTLIB contrary to
           TPTP), which are:
           - a raw decimal literal
           - the negation of a raw decimal literal
           - a division of an integer coeficient by a strictly positive
             integer literal (i.e. non-zero, since integer literals in SMTLIB
             cannot have a minus sign, and hence are always non-negative).
          + Other expressions *)
      let view (t : term) =
        match View.Arith.Shallow.view t with
        | `Value (`Int, _) -> `Int_coef
        | `Value (`Real, _) -> `Real_coef
        | `Arith (Minus, [t']) ->
          begin match View.Arith.Shallow.view t' with
            | `Value (`Int, _) -> `Int_coef
            | `Value (`Real, _) -> `Real_coef
            | _ -> `Complex_arith
          end
        (* We can assume the filters for linear arithmetic are used, hence
           a division must conform to the {div} filter, and thus can only
           be a rational/real coeficient *)
        | `Arith (Div, _) -> `Real_coef
        (* Terms with a head symbol in Arith must be distinguished
           from the rest *)
        | `Arith _ -> `Complex_arith
        (* Anything else is technically a generic term *)
        | _ -> `Generic

      (* Generic wrapper for arithmetic operations not permitted
         in linear arithmetic *)
      let forbidden_wrapper _ _ _ =
        `Error "this operator is forbidden in linear arithmetic"

      (* Division wrapper *)

      let div_wrapper _ _ ts =
        match ts with
        | [a; b] ->
          begin match view a with
            | `Int_coef ->
              begin match View.Arith.Shallow.view b with
                | `Value (`Int, "0") -> `Error (
                    Format.asprintf "division in linear arithmetic \
                                     expects a non-zero denominator")
                | `Value (`Int, _) -> `Pass
                | v -> `Error (
                    Format.asprintf "division in linear arithmetic \
                                     expects a constant positive \
                                     integer literal as denominator, \
                                     but was given %a"
                      View.Arith.Shallow.print v)
              end
            | _ -> `Error (
                Format.asprintf "division in linear arithmetic \
                                 expects as first argument an integer \
                                 coeficient, i.e. either a raw integer \
                                 literal, or the negation of one, but \
                                 here was given a %a"
                  View.Arith.Shallow.print (View.Arith.Shallow.view a))
          end
        | _ -> `Error "bad arity for multiplication"


      (* Multiplication wrappers *)

      let mul_wrapper _ _ ts =
        match ts with
        | [a; b] ->
          begin match view a, view b with
            | (`Int_coef | `Real_coef), `Generic
            | `Generic, (`Int_coef | `Real_coef)
              -> `Pass
            | _ -> `Error (
                Format.asprintf "multiplication in linear arithmetic %s"
                  (View.Arith.Shallow.(expect_error (view a) (view b)
                                         "an integer or rational literal \
                                          and a symbol (variable or constant)")))
          end
        | _ -> `Error "bad arity for multiplication"

      let div = name, active, div_wrapper
      let mul = name, active, mul_wrapper
      let forbidden = name, active, forbidden_wrapper

    end

    module IDL = struct

      let active = ref false
      let name = "integer difference logic"
      let reset () = active := false

      let forbidden_wrapper _ _ _ =
        `Error "this operator is forbidden in difference logic"

      let minus_wrapper _ _ ts =
        match ts with
        | [a] ->
          begin match View.Arith.Shallow.view a with
            | `Value (`Int, _) -> `Pass
            | v -> `Error (
                Format.asprintf
                  "unary substraction in difference logic expects \
                   an integer literal, but was given %a"
                  View.Arith.Shallow.print v)
          end
        | _ -> `Error "bas arity for unary substraction"

      let sub_wrapper _ _ ts =
        match ts with
        | [a; b] ->
          begin match View.Arith.Shallow.view a,
                      View.Arith.Shallow.view b with
          | (`Var _ | `Cst _), (`Var _ | `Cst _) -> `Pass
          | v, v' -> `Error (
              Format.asprintf "substraction in difference logic %s"
                (View.Arith.Shallow.expect_error v v' "two constants/variables"))
          end
        | _ -> `Error "bad arity for substraction"

      let comp_wrapper _ _ ts =
        match ts with
        | [a; b] ->
          begin match View.Arith.Shallow.view a,
                      View.Arith.Shallow.view b with
          (* Comparison of constants is allowed *)
          | (`Var _ | `Cst _), (`Var _ | `Cst _) -> `Pass
          (* If the first argument is a substraction, it must have passed
             the sub_wrapper filter, which means both its side must be
             constants, so no need to check it here again. *)
          | `Arith (Sub, _), `Value (`Int, _) -> `Pass
          | `Arith (Sub, _), `Arith(Minus, _) -> `Pass
          (* Error case *)
          | v, v' -> `Error (
              Format.asprintf "comparison in difference logic %s"
                (View.Arith.Shallow.expect_error v v'
                   "a substraction on the left and a (possibly negated) \
                    integer literal on the right"))
          end
        | _ -> `Error "bad arity for comparison"

      let sub = name, active, sub_wrapper
      let minus = name, active, minus_wrapper
      let comp = name, active, comp_wrapper
      let forbidden = name, active, forbidden_wrapper

    end

    module RDL = struct

      let active = ref false
      let name = "real difference logic"
      let reset () = active := false

      let forbidden_wrapper _ _ _ =
        `Error "this operator is forbidden in difference logic"

      let minus_wrapper _ _ ts =
        match ts with
        | [a] ->
          begin match View.Arith.Shallow.view a with
            | `Value (`Real, _) -> `Pass
            | v -> `Error (
                Format.asprintf
                  "unary substraction in difference logic expects \
                   a real literal, but was given %a"
                  View.Arith.Shallow.print v)
          end
        | _ -> `Error "bas arity for unary substraction"

      let div_wrapper _ _ ts =
        match ts with
        | [a; b] ->
          begin match View.Arith.Shallow.view a with
            | `Value (`Real, _)
            | `Arith (Minus, _) ->
              begin match View.Arith.Shallow.view b with
                | `Value (`Int, "0") -> `Error (
                    Format.asprintf "division in real difference logic \
                                     expects a non-zero denominator")
                | `Value (`Int, _) -> `Pass
                | v -> `Error (
                    Format.asprintf "division in real difference logic \
                                     expects a constant positive \
                                     integer literal as denominator, \
                                     but was given %a"
                      View.Arith.Shallow.print v)
              end
            | v -> `Error (
                Format.asprintf "division in real difference logic \
                                 expects a real literal as numerator, \
                                 but was given %a"
                  View.Arith.Shallow.print v)
          end
        | _ -> `Error "bad arity for division"

      (* Analyse a sum (or direct application) of n variables or constants
         and return n and the variable/constant *)
      let rec analyze t : [
        `Ok of [`Var of term_var | `Cst of term_const] * int
      | `Error of string
      ] =
        match View.Arith.Shallow.view t with
        | ((`Var _) as res)
        | ((`Cst _) as res) -> `Ok (res, 1)
        | `Arith (Add, l) -> analyze_list l
        | v -> `Error (
            Format.asprintf "addition in real difference logic expects \
                             either variables/constants or an addition of \
                             variables/constants, but was here given %a"
              View.Arith.Shallow.print v)

      and analyze_list = function
        | h :: r ->
          begin match analyze h with
            | (`Error _) as res -> res
            | `Ok (symb, n) ->
              analyze_list_aux n symb r
          end
        | [] -> `Error "bad arity for addition"

      and analyze_list_aux n s = function
        | [] -> `Ok (s, n)
        | t :: r ->
          begin match analyze t with
            | (`Error _) as res -> res
            | `Ok (s', n') ->
              if equal_symb s s' then
                analyze_list_aux (n + n') s r
              else
                `Error (
                  Format.asprintf "addition in real difference logic expects
                                 n-th times the same variable/constant, but was
                                 here applied to %a and %a which are different"
                    View.Arith.Shallow.print s
                    View.Arith.Shallow.print s')
          end

      and equal_symb s s' =
        match s, s' with
        | `Var v, `Var v' -> v.index = v'.index
        | `Cst c, `Cst c' -> c.index = c'.index
        | _ -> false

      let add_wrapper _ _ ts =
        begin match analyze_list ts with
          | `Ok _ -> `Pass
          | (`Error _) as res -> res
        end

      let sub_wrapper _ _ ts =
        match ts with
        | [a; b] ->
          begin match analyze a,
                      analyze b with
          | ((`Error _) as res), _ -> res
          | _, ((`Error _) as res) -> res
          | `Ok (_, n), `Ok (_, n') ->
            if n = n' then `Pass
            else `Error (
                Format.asprintf "substraction in real difference logic \
                                 expects both sides to be sums of the same \
                                 length, but here the sums have lengths \
                                 %d and %d" n n')
          end
        | _ -> `Error "bad arity for substraction"

      let comp_wrapper _ _ ts =
        match ts with
        | [a; b] ->
          begin match View.Arith.Shallow.view a,
                      View.Arith.Shallow.view b with
          (* Comparison of constants is allowed *)
          | (`Var _ | `Cst _), (`Var _ | `Cst _) -> `Pass
          (* If the first argument is a substraction, it must have passed
             the sub_wrapper filter, which means both its side must be
             constants, so no need to check it here again. *)
          | `Arith (Sub, _), `Value (`Real, _) -> `Pass
          | `Arith (Sub, _), `Arith (Minus, _) -> `Pass
          (* Syntaxic sugar *)
          | `Arith (Sub, [x; y]), `Arith (Div, _) ->
            begin match analyze x, analyze y with
              | ((`Error _) as res), _ -> res
              | _, ((`Error _) as res) -> res
              | `Ok (_, n), `Ok (_, n') ->
                (* since the sub filter passed, we should have n = n' *)
                if n = 1 && n' = 1 then `Pass
                else `Error (
                    Format.asprintf "in real difference logic, when comparing \
                                     the result of a substraction with a rational \
                                     number, each side of the substraction can only \
                                     contain a single variable/constant, but here there
                                     was %d" n)
            end
          (* Error case *)
          | v, v' -> `Error (
              Format.asprintf "comparison in difference logic %s"
                (View.Arith.Shallow.expect_error v v'
                   "a substraction on the left and a (possibly negated) \
                    integer literal on the right"))
          end
        | _ -> `Error "bad arity for comparison"

      let add = name, active, add_wrapper
      let sub = name, active, sub_wrapper
      let div = name, active, div_wrapper
      let minus = name, active, minus_wrapper
      let comp = name, active, comp_wrapper
      let forbidden = name, active, forbidden_wrapper

    end

  end

  let reset () =
    Quantifier.reset ();
    Smtlib2.IDL.reset ();
    Smtlib2.RDL.reset ();
    Smtlib2.Linear_large.reset ();
    Smtlib2.Linear_strict.reset ();
    ()

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

  let mk descr = { descr; hash = -1; tags = Tag.empty; }

  let of_var v = mk (Var v)

  let wildcard () = of_var (Id.mk ~builtin:Wildcard "_" Type)

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

  (* Helpers for adt definition *)
  let mk_cstr ty_c name i vars args ret =
    Id.const ~builtin:(Constructor (ty_c, i)) name vars args ret

  (* ADT definition *)
  let define_adt_aux ~record ty_const vars l =
    let ty =  Ty.apply ty_const (List.map Ty.of_var vars) in
    let cases = ref [] in
    let l' = List.mapi (fun i (cstr_name, args) ->
        let args_ty = List.map fst args in
        let cstr = mk_cstr ty_const cstr_name i vars args_ty ty in
        let dstrs = Array.make (List.length args) None in
        let l' = List.mapi (fun j -> function
            | (arg_ty, None) -> (arg_ty, None)
            | (arg_ty, Some name) ->
              let dstr =
                Id.const
                  ~builtin:(Destructor (ty_const, cstr, i, j))
                  name vars [ty] arg_ty
              in
              dstrs.(j) <- Some dstr;
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

    module Int = struct

      let int =
        with_cache ~cache:(Hashtbl.create 113) (fun s ->
            Id.const ~builtin:(Integer s) s [] [] Ty.int
          )

      let minus = Id.const
          ~pos:Pretty.Prefix ~name:"-" ~builtin:Minus
          ~term_filters:[Filter.Smtlib2.IDL.minus]
          "Minus" [] [Ty.int] Ty.int

      let add = Id.const
          ~pos:Pretty.Infix ~name:"+" ~builtin:Add
          ~term_filters:[Filter.Smtlib2.IDL.forbidden]
          "Add" [] [Ty.int; Ty.int] Ty.int

      let sub = Id.const
          ~pos:Pretty.Infix ~name:"-" ~builtin:Sub
          ~term_filters:[Filter.Smtlib2.IDL.sub]
          "Sub" [] [Ty.int; Ty.int] Ty.int

      let mul = Id.const
          ~pos:Pretty.Infix ~name:"*" ~builtin:Mul
          ~term_filters:[Filter.Smtlib2.Linear_large.mul;
                         Filter.Smtlib2.Linear_strict.mul;
                         Filter.Smtlib2.IDL.forbidden]
          "Mul" [] [Ty.int; Ty.int] Ty.int

      let div_e = Id.const
          ~pos:Pretty.Infix ~name:"/" ~builtin:Div_e
          ~term_filters:[Filter.Smtlib2.Linear_large.forbidden;
                         Filter.Smtlib2.Linear_strict.forbidden;
                         Filter.Smtlib2.IDL.forbidden]
          "Div_e" [] [Ty.int; Ty.int] Ty.int
      let div_t = Id.const
          ~pos:Pretty.Infix ~name:"/t" ~builtin:Div_t
          "Div_t" [] [Ty.int; Ty.int] Ty.int
      let div_f = Id.const
          ~pos:Pretty.Infix ~name:"/f" ~builtin:Div_f
          "Div_f" [] [Ty.int; Ty.int] Ty.int

      let rem_e = Id.const
          ~pos:Pretty.Infix ~name:"%" ~builtin:Modulo_e
          ~term_filters:[Filter.Smtlib2.Linear_large.forbidden;
                         Filter.Smtlib2.Linear_strict.forbidden;
                         Filter.Smtlib2.IDL.forbidden]
          "Modulo" [] [Ty.int; Ty.int] Ty.int
      let rem_t = Id.const
          ~pos:Pretty.Infix ~name:"%e" ~builtin:Modulo_t
          "Modulo" [] [Ty.int; Ty.int] Ty.int
      let rem_f = Id.const
          ~pos:Pretty.Infix ~name:"%f" ~builtin:Modulo_f
          "Modulo" [] [Ty.int; Ty.int] Ty.int

      let abs = Id.const
          ~name:"abs" ~builtin:Abs
          ~term_filters:[Filter.Smtlib2.Linear_large.forbidden;
                         Filter.Smtlib2.Linear_strict.forbidden;
                         Filter.Smtlib2.IDL.forbidden]
          "Abs" [] [Ty.int] Ty.int

      let lt = Id.const
          ~pos:Pretty.Infix ~name:"<" ~builtin:Lt
          ~term_filters:[Filter.Smtlib2.IDL.comp]
          "LessThan" [] [Ty.int; Ty.int] Ty.prop

      let le = Id.const
          ~pos:Pretty.Infix ~name:"<=" ~builtin:Leq
          ~term_filters:[Filter.Smtlib2.IDL.comp]
          "LessOrEqual" [] [Ty.int; Ty.int] Ty.prop

      let gt = Id.const
          ~pos:Pretty.Infix ~name:">" ~builtin:Gt
          ~term_filters:[Filter.Smtlib2.IDL.comp]
          "GreaterThan" [] [Ty.int; Ty.int] Ty.prop

      let ge = Id.const
          ~pos:Pretty.Infix ~name:">=" ~builtin:Geq
          ~term_filters:[Filter.Smtlib2.IDL.comp]
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
          ~term_filters:[Filter.Smtlib2.RDL.minus]
          "Minus" [] [Ty.real] Ty.real

      let add = Id.const
          ~pos:Pretty.Infix ~name:"+" ~builtin:Add
          ~term_filters:[Filter.Smtlib2.RDL.add]
          "Add" [] [Ty.real; Ty.real] Ty.real

      let sub = Id.const
          ~pos:Pretty.Infix ~name:"-" ~builtin:Sub
          ~term_filters:[Filter.Smtlib2.RDL.sub]
          "Sub" [] [Ty.real; Ty.real] Ty.real

      let mul = Id.const
          ~pos:Pretty.Infix ~name:"*" ~builtin:Mul
          ~term_filters:[Filter.Smtlib2.Linear_large.mul;
                         Filter.Smtlib2.Linear_strict.mul;
                         Filter.Smtlib2.RDL.forbidden]
          "Mul" [] [Ty.real; Ty.real] Ty.real

      let div = Id.const
          ~pos:Pretty.Infix ~name:"/" ~builtin:Div
          ~term_filters:[Filter.Smtlib2.Linear_large.div;
                         Filter.Smtlib2.Linear_strict.div;
                         Filter.Smtlib2.RDL.div]
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
          ~term_filters:[Filter.Smtlib2.RDL.comp]
          "LessThan" [] [Ty.real; Ty.real] Ty.prop

      let le = Id.const
          ~pos:Pretty.Infix ~name:"<=" ~builtin:Leq
          ~term_filters:[Filter.Smtlib2.RDL.comp]
          "LessOrEqual" [] [Ty.real; Ty.real] Ty.prop

      let gt = Id.const
          ~pos:Pretty.Infix ~name:">" ~builtin:Gt
          ~term_filters:[Filter.Smtlib2.RDL.comp]
          "GreaterThan" [] [Ty.real; Ty.real] Ty.prop

      let ge = Id.const
          ~pos:Pretty.Infix ~name:">=" ~builtin:Geq
          ~term_filters:[Filter.Smtlib2.RDL.comp]
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

    let void =
      match define_adt Ty.Const.unit [] ["void", []] with
      | [void, _] -> void
      | _ -> assert false

  end

  (* Record fields are represented as their destructors, i.e. constants *)
  module Field = struct
    type t = term_const
    let hash = Id.hash
    let equal = Id.equal
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
    let res = mk (Binder (b, body)) (ty body) in
    match !Filter.Quantifier.active, b with
    | false, _
    | _, (Letin _) -> res
    | true, (Exists _ | Forall _) ->
      raise (Filter_failed_term (Filter.Quantifier.name, res, ""))

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
       List.length f.ty.fun_args <> List.length args then begin
      raise (Bad_term_arity (f, tys, args))
    end else begin
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
    check_filters res f tys args (Const.get_tag f Filter.term)

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

