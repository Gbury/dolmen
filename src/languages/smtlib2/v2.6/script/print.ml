
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Printing of identifiers *)
(* ************************************************************************* *)

module Misc = Dolmen_std.Misc

exception Cannot_print of string

(* TODO: structured errors *)
let _cannot_print format =
  Format.kasprintf (fun msg -> raise (Cannot_print msg)) format

(* lexical definitions taken from the smtlib specification *)

let[@inline] is_whitespace c =
  let c = Char.code c in
  c = 9 (* tab *) || c = 10 (* line feed *) ||
  c = 13 (* cariage return *) || c = 32 (* space *)

let[@inline] is_printable c =
  let c = Char.code c in
  (32 <= c && c <= 126) || 128 <= c

let[@inline] is_quoted_symbol_char c =
  (is_whitespace c || is_printable c) &&
  (c <> '|' && c <> '\\')

(* symbol categorization *)

type symbol =
  | Simple
  | Quoted
  | Unprintable

let categorize_symbol s =
  if String.length s = 0 (* s = "" *) then
    Unprintable
  else begin
    match Lexer.M.find_opt s Lexer.reserved_words with
    | Some _ -> Quoted
    | None ->
      if Misc.lex_string Lexer.check_simple_symbol s then
        Simple
      else if Misc.lex_string Lexer.check_quoted_symbol s then
        Quoted
      else
        Unprintable
  end

let symbol_aux fmt s =
  (* TODO: expose/add a cache to not redo the `categorize_symbol` computation each time *)
  match categorize_symbol s with
  | Simple -> Format.pp_print_string fmt s
  | Quoted -> Format.fprintf fmt "|%s|" s
  | Unprintable ->
    _cannot_print "symbol \"%s\" cannot be printed due to lexical constraints" s

let index fmt s =
  if Misc.lex_string Lexer.check_num s then
    Format.pp_print_string fmt s
  else
    symbol_aux fmt s


let symbol fmt name =
  match (name : Dolmen_std.Name.t) with
  | Simple s ->
    symbol_aux fmt s
  | Indexed { basename = _; indexes = [] } ->
    _cannot_print "indexed id with no indexes: %a" Dolmen_std.Name.print name
  | Indexed { basename; indexes; } ->
    let pp_sep fmt () = Format.fprintf fmt " " in
    Format.fprintf fmt "(_ %a %a)"
      symbol_aux basename (Format.pp_print_list ~pp_sep index) indexes
  | Qualified _ ->
    _cannot_print "qualified identifier: %a" Dolmen_std.Name.print name

let keyword fmt name =
  match (name : Dolmen_std.Name.t) with
  | Simple s when Misc.lex_string Lexer.check_keyword s ->
    Format.pp_print_string fmt s
  | _ -> _cannot_print "not a keyword"

let num fmt s =
  if Misc.lex_string Lexer.check_num s then
    Format.pp_print_string fmt s
  else
    _cannot_print "num"

let dec fmt s =
  if Misc.lex_string Lexer.check_dec s then
    Format.pp_print_string fmt s
  else
    _cannot_print "dec"

let hex fmt s =
  if Misc.lex_string Lexer.check_hex s then
    Format.pp_print_string fmt s
  else
    _cannot_print "hex"

let bin fmt s =
  if Misc.lex_string Lexer.check_bin s then
    Format.pp_print_string fmt s
  else
    _cannot_print "bin"

let string fmt s =
  let can_print = ref true in
  let quotation = ref 0 in
  String.iter (fun c ->
      if c = '"' then quotation := !quotation + 1;
      if not (is_whitespace c || is_printable c) then can_print := false
    ) s;
  if not !can_print then
    _cannot_print "string: \"%s\"" s
  else if !quotation = 0 then
    Format.fprintf fmt {|"%s"|} s
  else begin
    Format.pp_print_char fmt '"';
    String.iter (function
        | '"' -> Format.fprintf fmt {|""|}
        | c -> Format.fprintf fmt "%c" c) s;
    Format.pp_print_char fmt '"'
  end

(* sanitization *)

let sanitize_aux _idx o =
  (* smtlib identifiers can be quoted, and quotable symbols are a strict
     superset of non-quoted symbols, so we only need to make sure that
     all characters are quotable *)
  match o with
  | None -> [Uchar.of_char '_']
  | Some c ->
    if Uchar.is_char c &&
       is_quoted_symbol_char (Uchar.to_char c) then
      [c]
    else
      [Uchar.of_char '_']

let sanitize _id name =
  match (name : Dolmen_std.Name.t) with
  | Simple "" ->
    Dolmen_std.Name.simple "_"
  | Simple s ->
    let s' = Dolmen_std.Misc.string_unicode_map sanitize_aux s in
    (* avoid an allocation if the name has not changed *)
    if s' == s then name else Dolmen_std.Name.simple s'
  | Indexed { basename = basename; indexes; } ->
    let basename' = Dolmen_std.Misc.string_unicode_map sanitize_aux basename in
    let indexes' =
      Dolmen_std.Misc.list_map_sharing
      (Dolmen_std.Misc.string_unicode_map sanitize_aux) indexes
    in
    if basename == basename' && indexes = indexes'
    then name
    else Dolmen_std.Name.indexed basename' indexes'
  | Qualified _ ->
    _cannot_print "qualified names in smtlib2"


(* Printing of terms and statements *)
(* ************************************************************************* *)

module Make
    (Env : Dolmen_intf.Env.Print
     with type name := Dolmen_std.Name.t)
    (S : Dolmen_intf.View.Sexpr.S
     with type id := Dolmen_std.Id.t)
    (V : Dolmen_intf.View.TFF.S
     with type ty = Env.ty
      and type ty_var = Env.ty_var
      and type ty_cst = Env.ty_cst
      and type term = Env.term
      and type term_var = Env.term_var
      and type term_cst = Env.term_cst
      and type formula = Env.formula)
= struct

  module N = Dolmen_std.Name
  module B = Dolmen_std.Builtin
  module F = Dolmen_intf.View.TFF
  module E = Dolmen_std.View.Assoc(V)

  (* Env suff *)
  (* ******** *)

  (* Applications of `to_real` that are **directly** under an arithmetic
     operator (such as '+'), can omit to print applications of `to_real`,
     since these will be added back when parsing/typing. *)
  let can_omit_to_real_key : bool Env.key = Env.key ()
  let set_omit_to_real env b =
    match Env.get env can_omit_to_real_key with
    | Some b' when b = b' -> env
    | _ -> Env.set env can_omit_to_real_key b
  let can_omit_to_real env =
    match Env.get env can_omit_to_real_key with
    | Some true -> true
    | _ -> false


  (* Helpers *)
  (* ******* *)

  let list pp env fmt l =
    let pp_sep fmt () = Format.fprintf fmt "@ " in
    Format.pp_print_list ~pp_sep (pp env) fmt l


  (* Ids *)
  (* *** *)

  let symbol _env fmt name = symbol fmt name

  let id ~allow_keyword env fmt id =
    match (id : Dolmen_std.Id.t) with
    | { ns = Value String; name = Simple s; } -> string fmt s
    | { ns = Value Integer; name = Simple s; } -> num fmt s
    | { ns = Value Real; name = Simple s; } -> dec fmt s
    | { ns = Value Hexadecimal; name = Simple s; } -> hex fmt s
    | { ns = Value Binary; name = Simple s; } -> bin fmt s
    | { ns = (Attr | Term); name = Simple s; } ->
      if (allow_keyword && Misc.lex_string Lexer.check_keyword s)
         || Misc.lex_string Lexer.check_simple_symbol s then
        Format.pp_print_string fmt s
      else if Misc.lex_string Lexer.check_quoted_symbol s then
        Format.fprintf fmt "|%s|" s
      else
        _cannot_print "unprintable symbol"
    | { ns = Term; name; } ->
      symbol env fmt name
    | _ ->
      _cannot_print "unprintable id"


  (* Types *)
  (* ***** *)

  let ty_var env fmt v =
    let name = Env.Ty_var.name env v in
    (* TODO: setup a cache from names to category in one of the env's keys *)
    symbol env fmt name

  let ty_cst env fmt c =
    let name = Env.Ty_cst.name env c in
    (* TODO: setup a cache from names to category in one of the env's keys *)
    symbol env fmt name

  let ty_head_name env c =
    (* TODO: add reservations for the builtin names in the env *)
    let int = string_of_int in
    match V.Ty.Cst.builtin c with
    | B.Base -> Env.Ty_cst.name env c
    | B.Prop -> N.simple "Bool"
    | B.Int -> N.simple "Int"
    | B.Real -> N.simple "Real"
    | B.Array -> N.simple "Array"
    | B.Bitv n -> N.indexed "Bitvec" [int n]
    | B.Float (5, 11) -> N.simple "Float16"
    | B.Float (8, 24) -> N.simple "Float32"
    | B.Float (11,53) -> N.simple "Float64"
    | B.Float (15,113) -> N.simple "Float128"
    | B.Float (e, s) -> N.indexed "FloatingPoint" [int e; int s]
    | B.RoundingMode -> N.simple "RoundingMode"
    | B.String -> N.simple "String"
    | B.String_RegLan -> N.simple "RegLan"
    | _ -> _cannot_print "unknown type builtin"

  let rec ty env fmt t =
    match V.Ty.view t with
    | Var v -> ty_var env fmt v
    | App (head, args) ->
      let f = ty_head_name env head in
      begin match args with
        | [] ->
          symbol env fmt f
        | _ :: _ ->
          Format.fprintf fmt "(%a %a)"
            (symbol env) f (list ty env) args
      end


  (* Terms *)
  (* ***** *)

  let term_var env fmt v =
    let name = Env.Term_var.name env v in
    (* TODO: setup a cache from names to category in one of the env's keys *)
    symbol env fmt name

  let term_cst env fmt c =
    let name = Env.Term_cst.name env c in
    (* TODO: setup a cache from names to category in one of the env's keys *)
    symbol env fmt name

  let sorted_var env fmt v =
    Format.fprintf fmt "(%a %a)" (term_var env) v (ty env) (V.Term.Var.ty v)

  let add_binding_to_env env (v, _) =
    Env.Term_var.bind env v

  let add_pattern_to_env env pat =
    match (pat : _ F.Term.pattern) with
    | Var v -> Env.Term_var.bind env v
    | Constructor (_, l) -> List.fold_left Env.Term_var.bind env l

  let term_cst_chainable _env c =
    (* WARNING: this `blt` function should only be called with builtins that
       do not have payload (such as terms), since the polymorphic comparison
       will not work adequately in these cases. *)
    let blt b = fun c -> V.Term.Cst.builtin c = b in
    let b = V.Term.Cst.builtin c in
    let yup () = `Chainable (blt b) in
    match b with
    | B.Equal
    | B.Lt (`Int | `Real) | B.Leq (`Int | `Real)
    | B.Gt (`Int | `Real) | B.Geq (`Int | `Real)
      -> yup ()
    | _ -> `Nope

  let term_cst_assoc _env c =
    (* WARNING: this `blt` function should only be called with builtins that
       do not have payload (such as terms), since the polymorphic comparison
       will not work adequately in these cases. *)
    let blt b = fun c -> V.Term.Cst.builtin c = b in
    let b = V.Term.Cst.builtin c in
    let left () = `Left_assoc (blt b) in
    let right () = `Right_assoc (blt b) in
    match b with
    (* left associative builtins *)
    | B.Or | B.And | B.Xor
    | B.Add (`Int | `Real)
    | B.Sub (`Int | `Real)
    | B.Mul (`Int | `Real)
    | B.Div `Real
      -> left ()
    (* Right associative builtins *)
    | B.Imply
      -> right ()
    (* all others are non-associative *)
    | _ -> `None

  let term_cst_poly _env c =
    match V.Sig.view (V.Term.Cst.ty c) with
    | Signature (_ :: _, _, _) -> true
    | _ -> false

  (* Note: unfortunately, most of the smtlib term constructions end with a
     parenthesis, and therefore their printers are currently not tail-rec.
     This is particularly true for sequential let-bindings.

     This means that the current printers are likely to produce a stack
     overflow if used on extremely big/deep terms (or with a lot of sequential
     let bindings). We might want to consider ocaml 5 and lifting the stack
     limit for these cases. *)
  let rec term env fmt t =
    term_view env fmt (V.Term.ty t) (V.Term.view t)

  and term_view env fmt t_ty view =
    match (view : _ F.Term.view) with
    | Var v -> term_var env fmt v
    | App (head, ty_args, args) -> term_app env fmt (t_ty, head, ty_args, args)
    | Match (scrutinee, cases) -> term_match env fmt (scrutinee, cases)
    | Binder (Exists (tys, ts), body) -> quant "exists" env fmt (tys, ts, body)
    | Binder (Forall (tys, ts), body) -> quant "forall" env fmt (tys, ts, body)
    | Binder (Letand l, body) -> letand env fmt (l, body)
    | Binder (Letin l, body) -> letin env fmt (l, body)

  and term_app env fmt (t_ty, head, ty_args, args) =
    (* first, we need to undo any left/right associativity/chainability that
       may have been expanded by the typechecker or other mechanism. *)
    let head, ty_args, args =
      let ty_args, args =
        match term_cst_assoc env head with
        | `Left_assoc top_head -> None, E.left_assoc top_head args
        | `Right_assoc top_head -> None, E.right_assoc top_head args
        | `None -> Some ty_args, args
      in
      match V.Term.Cst.builtin head, args with
      | B.And, t :: _ ->
        begin match V.Term.view t with
          | App (h, _, _) ->
            begin match term_cst_chainable env h with
              | `Chainable top_head ->
                begin match E.chainable top_head args with
                  | Some new_args -> h, None, new_args
                  | None -> head, ty_args, args
                end
              | `Nope -> head, ty_args, args
            end
          | _ -> head, ty_args, args
        end
      | _ -> head, ty_args, args
    in

    (* smtlib has implicit type arguments, i.e. the type args are not printed.
       Therefore, whenever a polymorphic symbol is used, its type arguments
       need to be inferable from its term arguments. Hence, when a symbol is
       polymorphic and there are no term arguments, we need to print an
       explicit type annotation to disambiguate things. In the other cases,
       we suppose that a symbol's type arguments can be deduced from the term
       arguments. *)
    let aux ?(omit_to_real=false) h args =
      let env = set_omit_to_real env omit_to_real in
      match args with
      | [] ->
        if term_cst_poly env head then
          Format.fprintf fmt "(as@ %a@ %a)"
            (id ~allow_keyword:false env) h (ty env) t_ty
        else
          Format.fprintf fmt "%a" (id ~allow_keyword:false env) h
      | _ :: _ ->
        Format.fprintf fmt "(%a@ %a)" (id ~allow_keyword:false env) h (list term env) args
    in

    (* small shorthand *)
    let int = string_of_int in
    let p ?omit_to_real ns name =
      aux ?omit_to_real (Dolmen_std.Id.create ns name) args
    in
    let simple ?omit_to_real s =
      p ?omit_to_real Term (N.simple s)
    in

    (* Matching *)
    match V.Term.Cst.builtin head with

    (* Base + Algebraic datatypes *)
    | B.Base | B.Constructor _ | B.Destructor _ ->
      p Term (Env.Term_cst.name env head)
    | B.Tester { cstr; _ } ->
      begin match Env.Term_cst.name env cstr with
        | Simple s -> p Term (N.indexed "is" [s])
        | _ -> _cannot_print "expected a simple for a constructor name"
      end

    (* Cast *)
    | B.Coercion ->
      begin match ty_args with
        | None -> assert false (* coercions should not be chainable/associative *)
        | Some [a; b] ->
          begin match V.Ty.view a, V.Ty.view b with

            (* Int-> Real conversion *)
            | App (ah, []), App (bh, [])
              when (match V.Ty.Cst.builtin ah with B.Int -> true | _ -> false) &&
                   (match V.Ty.Cst.builtin bh with B.Real -> true | _ -> false) ->
              if can_omit_to_real env then
                match args with
                | [t] ->
                  term env fmt t
                | _ -> _cannot_print "bad applicaiton of coercion"
              else
                simple "to_real"

            (* fallback *)
            | _ -> _cannot_print "unhandled builtin"
          end
        | Some _ -> _cannot_print "bad coercion application"
      end

    (* Boolean core *)
    | B.True -> simple "true"
    | B.False -> simple "false"
    | B.Neg -> simple "not"
    | B.And -> simple "and"
    | B.Or -> simple "or"
    | B.Xor -> simple "xor"
    | B.Imply -> simple "=>"
    | B.Ite -> simple "ite"
    | B.Equal -> simple "="
    | B.Distinct -> simple "distinct"

    (* Arrays *)
    | B.Store -> simple "store"
    | B.Select -> simple "select"

    (* Arithmetic *)
    | B.Integer s -> p (Value Integer) (N.simple s)
    | B.Decimal s -> p (Value Real) (N.simple s)
    | B.Lt (`Int | `Real) -> simple ~omit_to_real:true "<"
    | B.Leq (`Int | `Real) -> simple ~omit_to_real:true "<="
    | B.Gt (`Int | `Real) -> simple ~omit_to_real:true ">"
    | B.Geq (`Int | `Real) -> simple ~omit_to_real:true ">="
    | B.Minus ( `Int | `Real) -> simple "-"
    | B.Add (`Int | `Real) -> simple ~omit_to_real:true "+"
    | B.Sub (`Int | `Real) -> simple ~omit_to_real:true "-"
    | B.Mul (`Int | `Real) -> simple ~omit_to_real:true "*"
    | B.Div `Real -> simple ~omit_to_real:true "/"
    | B.Div_e `Int -> simple "div"
    | B.Modulo_e `Int -> simple "mod"
    | B.Abs -> simple "abs"
    | B.Is_int `Real -> simple "is_int"
    | B.Floor_to_int `Real -> simple "to_int"
    | B.Divisible ->
      begin match args with
        | [x; y] ->
          begin match V.Term.view y with
            | App (f, [], []) ->
              begin match V.Term.Cst.builtin f with
                | B.Integer s ->
                  let id = Dolmen_std.Id.create Term (N.indexed "divisible" [s]) in
                  aux id [x]
                | _ -> _cannot_print "bad divisible application"
              end
            | _ -> _cannot_print "bad divisible application"
          end
        | _ -> _cannot_print "bad divisible application"
      end

    (* Bitvectors *)
    | B.Bitvec s -> p (Value Binary) (N.simple ("#b" ^ s)) (* TODO: see if we can recover hex form ? *)
    | B.Bitv_not _ -> simple "bvnot"
    | B.Bitv_and _ -> simple "bvand"
    | B.Bitv_or _ -> simple "bvor"
    | B.Bitv_nand _ -> simple "bvnand"
    | B.Bitv_nor _ -> simple "bvnor"
    | B.Bitv_xor _ -> simple "bvxor"
    | B.Bitv_xnor _ -> simple "bvxnor"
    | B.Bitv_comp _ -> simple "bvcomp"
    | B.Bitv_neg _ -> simple "bvneg"
    | B.Bitv_add _ -> simple "bvadd"
    | B.Bitv_sub _ -> simple "bvsub"
    | B.Bitv_mul _ -> simple "bvsub"
    | B.Bitv_udiv _ -> simple "bvudiv"
    | B.Bitv_srem _ -> simple "bvsrem"
    | B.Bitv_smod _ -> simple "bvsmod"
    | B.Bitv_shl _ -> simple "bvshl"
    | B.Bitv_lshr _ -> simple "bvlshr"
    | B.Bitv_ashr _ -> simple "bvashr"
    | B.Bitv_ult _ -> simple "bvult"
    | B.Bitv_ule _ -> simple "bvule"
    | B.Bitv_ugt _ -> simple "bvugt"
    | B.Bitv_uge _ -> simple "bvuge"
    | B.Bitv_slt _ -> simple "bvslt"
    | B.Bitv_sle _ -> simple "bvsle"
    | B.Bitv_sgt _ -> simple "bvsgt"
    | B.Bitv_sge _ -> simple "bvsge"
    | B.Bitv_concat _ -> simple "concat"
    | B.Bitv_repeat { n = _; k; } -> p Term (N.indexed "repeat" [int k])
    | B.Bitv_zero_extend { n = _; k; } -> p Term (N.indexed "zero_extend" [int k])
    | B.Bitv_sign_extend { n = _; k; } -> p Term (N.indexed "sign_extend" [int k])
    | B.Bitv_rotate_right { n = _; i; } -> p Term (N.indexed "rotate_right" [int i])
    | B.Bitv_rotate_left { n = _; i; } -> p Term (N.indexed "rotate_left" [int i])
    | B.Bitv_extract { n = _; i; j; } -> p Term (N.indexed "extract" [int i; int j])

    (* bvconv extension
       TODO: use a flag to enable extensions such as this one ? *)
    | B.Bitv_to_nat { n = _; } -> simple "bv2nat"
    | B.Bitv_of_int { n } -> p Term (N.indexed "int2bv" [int n])

    (* Floats *)
    | B.Fp _ -> simple "fp"
    | B.RoundNearestTiesToEven -> simple "RNE"
    | B.RoundNearestTiesToAway -> simple "RNA"
    | B.RoundTowardPositive -> simple "RTP"
    | B.RoundTowardNegative -> simple "RTN"
    | B.RoundTowardZero -> simple "RTZ"
    | B.Fp_abs _ -> simple "fp.abs"
    | B.Fp_neg _ -> simple "fp.neg"
    | B.Fp_add _ -> simple "fp.add"
    | B.Fp_sub _ -> simple "fp.sub"
    | B.Fp_mul _ -> simple "fp.mul"
    | B.Fp_div _ -> simple "fp.div"
    | B.Fp_fma _ -> simple "fp.fma"
    | B.Fp_sqrt _ -> simple "fp.sqrt"
    | B.Fp_rem _ -> simple "fp.rem"
    | B.Fp_roundToIntegral _ -> simple "fp.roundToInegral"
    | B.Fp_min _ -> simple "fp.min"
    | B.Fp_max _ -> simple "fp.max"
    | B.Fp_leq _ -> simple "fp.leq"
    | B.Fp_lt _ -> simple "fp.lt"
    | B.Fp_geq _ -> simple "fp.geq"
    | B.Fp_gt _ -> simple "fp.gt"
    | B.Fp_eq _ -> simple "fp.eq"
    | B.Fp_isNormal _ -> simple "fp.isNormal"
    | B.Fp_isSubnormal _ -> simple "fp.isSubnormal"
    | B.Fp_isZero _ -> simple "fp.isZero"
    | B.Fp_isInfinite _ -> simple "fp.isInfinite"
    | B.Fp_isNaN _ -> simple "fp.isNan"
    | B.Fp_isNegative _ -> simple "fp.isNegative"
    | B.Fp_isPositive _ -> simple "fp.isPositive"
    | B.To_real _ -> simple "fp.to_real"
    | B.Plus_infinity (e, s) -> p Term (N.indexed "+oo" [int e; int s])
    | B.Minus_infinity (e, s) -> p Term (N.indexed "-oo" [int e; int s])
    | B.Plus_zero (e, s) -> p Term (N.indexed "+zero" [int e; int s])
    | B.Minus_zero (e, s) -> p Term (N.indexed "-zero" [int e; int s])
    | B.NaN (e, s) -> p Term (N.indexed "NaN" [int e; int s])
    | B.Ieee_format_to_fp (e, s) -> p Term (N.indexed "to_fp" [int e; int s])
    | B.Fp_to_fp (_, _, e, s) -> p Term (N.indexed "to_fp" [int e; int s])
    | B.Real_to_fp (e, s) -> p Term (N.indexed "to_fp" [int e; int s])
    | B.Sbv_to_fp (_, e, s) -> p Term (N.indexed "to_fp" [int e; int s])
    | B.Ubv_to_fp (_, e, s) -> p Term (N.indexed "to_fp_unsigned" [int e; int s])
    | B.To_ubv (_, _, m) -> p Term (N.indexed "fp.to_ubv" [int m])
    | B.To_sbv (_, _, m) -> p Term (N.indexed "fp.to_sbv" [int m])

    (* fallback *)
    | _ -> _cannot_print "unknown term builtin"

  and letin env fmt (l, body) =
    (* reset some env state *)
    let env = set_omit_to_real env false in
    (* actual printing *)
    match l with
    | [] -> term env fmt body
    | binding :: r ->
      let env' = add_binding_to_env env binding in
      Format.fprintf fmt "@[<hv>(let (%a)@ %a)@]"
        (var_binding env' env) binding (letin env') (r, body)

  and letand env fmt (l, body) =
    (* reset some env state *)
    let env = set_omit_to_real env false in
    (* actual printing *)
    let env' = List.fold_left add_binding_to_env env l in
    Format.fprintf fmt "@[<hv>(let @[<hv>(%a)@]@ %a)@]"
      (list (var_binding env') env) l (term env) body

  and var_binding var_env t_env fmt (v, t) =
    Format.fprintf fmt "@[<hov 2>(%a@ %a)@]" (term_var var_env) v (term t_env) t

  and term_match env fmt (scrutinee, cases) =
    (* reset some env state *)
    let env = set_omit_to_real env false in
    (* actual printing *)
    Format.fprintf fmt "@[<hv 2>(match@ @[<hov>%a@]@ (%a))"
      (term env) scrutinee
      (list match_case env) cases

  and match_case env fmt (pat, arm) =
    let env = add_pattern_to_env env pat in
    Format.fprintf fmt "(@[<h>%a@] @[<hov>%a@])" (pattern env) pat (term env) arm

  and pattern env fmt pat =
    match (pat : _ F.Term.pattern) with
    | Var v -> term_var env fmt v
    | Constructor (c, []) -> term_cst env fmt c
    | Constructor (c, args) ->
      Format.fprintf fmt "(%a %a)"
        (term_cst env) c (list term_var env) args

  and quant q env fmt (tys, ts, body) =
    (* reset some env state *)
    let env = set_omit_to_real env false in
    (* actual printing *)
    (* TODO: patterns/triggers *)
    match tys, ts with
    | _ :: _, _ -> _cannot_print "type quantification"
    | [], [] -> term env fmt body
    | [], _ :: _ ->
      Format.fprintf fmt "(%s@ (%a)@ %a)" q
        (list sorted_var env) ts
        (term env) body

  and prop_literal env fmt t =
    (* either 'c' or 'not c' with 'c' a constant *)
    match V.Formula.view t with
    | App (f, [], [arg]) when (match V.Term.Cst.builtin f with B.Neg -> true | _ -> false) ->
      begin match V.Term.view arg with
        | App (c, [], []) ->
          let name = Env.Term_cst.name env c in
          Format.fprintf fmt "(not %a)" (symbol env) name
        | _ -> _cannot_print "not a prop literal"
      end
    | App (c, [], []) ->
      let name = Env.Term_cst.name env c in
      symbol env fmt name
    | _ -> _cannot_print "not a prop literal"

  and formula env fmt t =
    term_view env fmt (V.Formula.ty t) (V.Formula.view t)

  (* Datatypes *)
  (* ********* *)

  let constructor_dec env fmt (cstr, params) =
    match params with
    | [] ->
      Format.fprintf fmt "(%a)" (term_cst env) cstr
    | _ ->
      let aux env fmt (tty, dstr) =
        Format.fprintf fmt "@[<h>(%a %a)@]"
          (term_cst env) dstr (ty env) tty
      in
      Format.fprintf fmt "@[<hv 1>(%a@ %a)@]"
        (term_cst env) cstr
        (list aux env) params

  let datatype_dec env fmt (_, vars, cases) =
    match vars with
    | [] ->
      Format.fprintf fmt "@[<hv 1>(%a)@]" (list constructor_dec env) cases
    | _ ->
      let env = List.fold_left Env.Ty_var.bind env vars in
      Format.fprintf fmt "(par (%a)@ @[<v 1>(%a))@]"
        (list ty_var env) vars (list constructor_dec env) cases


  (* Attributes *)
  (* ********** *)

  let rec sexpr env fmt t =
    match S.view t with
    | Symbol s -> id ~allow_keyword:true env fmt s
    | App l -> list sexpr env fmt l

  let is_keyword t =
    match S.view t with
    | Symbol { ns = Attr; name = Simple s; }
      when Misc.lex_string Lexer.check_keyword s -> true
    | _ -> false

  let attribute env fmt t =
    match S.view t with
    | App ([k; _]) when is_keyword k -> sexpr env fmt t
    | _ -> _cannot_print "not an attribtue"

  let keyword env fmt t =
    if is_keyword t
    then sexpr env fmt t
    else _cannot_print "not a keyword"


  (* Statements *)
  (* ********** *)

  (* unit/trivial statements *)

  let unit_stmt s (_env : Env.t) fmt () =
    Format.fprintf fmt "(%s)" s

  let check_sat = unit_stmt "check-sat"

  let reset = unit_stmt "reset"
  let reset_assertions = unit_stmt "reset-assertions"

  let get_unsat_core = unit_stmt "get-unsat-core"
  let get_unsat_assumptions = unit_stmt "get-unsat-assumptions"

  let get_proof = unit_stmt "get-proof"
  let get_model = unit_stmt "get-model"

  let get_assertions = unit_stmt "get-assertions"
  let get_assignment = unit_stmt "get-assignment"

  let exit = unit_stmt "exit"

  (* statements with payloads *)

  let echo _env fmt s =
    Format.fprintf fmt "(echo \"%a\")" string s

  let set_logic _env fmt s =
    Format.fprintf fmt "@[<h>(set-logic %a)@]" symbol_aux s

  let set_info env fmt t =
    Format.fprintf fmt "@[<h>(set-info %a)@]" (attribute env) t

  let set_option env fmt t =
    Format.fprintf fmt "@[<h>(set-option %a)@]" (attribute env) t

  let get_info env fmt t =
    Format.fprintf fmt "@[<h>(get-info %a)@]" (keyword env) t

  let get_option env fmt t =
    Format.fprintf fmt "@[<h>(get-option %a)@]" (keyword env) t

  let get_value env fmt l =
    Format.fprintf fmt "@[<hv 2>(get-value (@,@[<hv>%a@]@,))"
      (list term env) l

  let pop _env fmt n =
    if n <= 0
    then raise (Cannot_print "pop with non-positive level")
    else Format.fprintf fmt "(pop %d)" n

  let push _env fmt n =
    if n <= 0
    then raise (Cannot_print "push with non-positive level")
    else Format.fprintf fmt "(push %d)" n

  let declare_sort env fmt c =
    let n = V.Ty.Cst.arity c in
    let name = Env.Ty_cst.name env c in
    Format.fprintf fmt "(declare-sort %a %d)" (symbol env) name n

  let declare_datatype env fmt ((c, _, _) as dec) =
    Format.fprintf fmt "@[<hv 2>(declare-datatype %a@ %a)@]"
      (ty_cst env) c
      (datatype_dec env) dec

  let declare_datatypes env fmt l =
    let sort_dec env fmt (c, _, _) =
      let n = V.Ty.Cst.arity c in
      Format.fprintf fmt "@[<h>(%a %d)@]" (ty_cst env) c n
    in
    match l with
    | [decl] -> declare_datatype env fmt decl
    | _ ->
      Format.fprintf fmt "@[<v 2>(declare-datatypes@ @[<v 1>(%a)@]@ (%a))@]"
        (list sort_dec env) l
        (list datatype_dec env) l

  let declare_fun env fmt c =
    let name = Env.Term_cst.name env c in
    let c_sig = V.Term.Cst.ty c in
    match V.Sig.view c_sig with
    | Signature ([], [], c_ty) ->
      Format.fprintf fmt "(declare-const %a %a)"
        (symbol env) name (ty env) c_ty
    | Signature ([], params, ret) ->
      Format.fprintf fmt "(declare-fun %a (%a) %a)"
        (symbol env) name (list ty env) params (ty env) ret
    | Signature (_ :: _, _, _) ->
      _cannot_print "polymorphic function declaration"

  let define_sort env fmt (c, params, body) =
    let env = List.fold_left Env.Ty_var.bind env params in
    Format.fprintf fmt "(define-sort %a (%a) %a)"
      (ty_cst env) c
      (list ty_var env) params
      (ty env) body

  let define_fun_aux ~recursive env fmt (f, params, body) =
    let env = List.fold_left Env.Term_var.bind env params in
    Format.fprintf fmt "@[<hv 2>(@[<hov 1>%s %a@ (%a) %a@]@ @[<hov>%a@])@]"
      (if recursive then "define-fun-rec" else "define-fun")
      (term_cst env) f
      (list sorted_var env) params
      (ty env) (V.Term.ty body)
      (term env) body

  let define_fun = define_fun_aux ~recursive:false
  let define_fun_rec = define_fun_aux ~recursive:true

  let define_funs_rec env fmt l =
    let fun_body env fmt (_, _, body) = term env fmt body in
    let fun_decl env fmt (f, params, body) =
      let env = List.fold_left Env.Term_var.bind env params in
      Format.fprintf fmt "(%a (%a) %a)"
        (term_cst env) f
        (list sorted_var env) params
        (ty env) (V.Term.ty body)
    in
    Format.fprintf fmt
      "@[<v 2>(define-funs-rec@ @[<v 2>(%a)@]@ @[<v 2>(%a)@])@]"
      (list fun_decl env) l
      (list fun_body env) l

  let assert_ env fmt t =
    Format.fprintf fmt "@[<hov 2>(assert %a)@]" (formula env) t

  let check_sat_assuming env fmt = function
    | [] -> check_sat env fmt ()
    | _ :: _ as l ->
      Format.fprintf fmt "@[<hov >(check-sat-assuming (%a))"
        (list prop_literal env) l

end

