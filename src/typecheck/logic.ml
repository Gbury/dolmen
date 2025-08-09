
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

module B = Dolmen_std.Builtin


(* Smtlib2 logics *)
(* ************************************************************************ *)

module Smtlib2 = struct

  type theory = [
    | `Core
    | `Arrays
    | `Bitvectors
    | `Floats
    | `String
    | `Ints
    | `Reals
    | `Reals_Ints
  ]

  let print_theory fmt th =
    match (th : theory) with
    | `Core -> Format.fprintf fmt "core"
    | `Arrays -> Format.fprintf fmt "arrays"
    | `Bitvectors -> Format.fprintf fmt "bitv"
    | `Floats -> Format.fprintf fmt "floats"
    | `String -> Format.fprintf fmt "string"
    | `Ints -> Format.fprintf fmt "int"
    | `Reals -> Format.fprintf fmt "real"
    | `Reals_Ints -> Format.fprintf fmt "int+real"

  let print_theories fmt l =
    let pp_sep fmt () = Format.fprintf fmt ",@ " in
    Format.pp_print_list ~pp_sep print_theory fmt l

  type features = {
    free_sorts      : bool;
    free_functions  : bool;
    datatypes       : bool;
    quantifiers     : bool;
    arithmetic      : Arith.Smtlib2.config;
    arrays          : Arrays.Smtlib2.config;
  }

  let print_features fmt { free_sorts; free_functions;
                           datatypes; quantifiers; arithmetic; arrays; } =
    Format.fprintf fmt
      "{ @[<hv>\
       free_sorts : %b;@ \
       free_functions : %b;@ \
       datatypes : %b;@ \
       quantifiers : %b;@ \
       arithmetic : %a;@ \
       arrays : %a; \
       }@]"
      free_sorts
      free_functions
      datatypes
      quantifiers
      Arith.Smtlib2.print_config arithmetic
      Arrays.Smtlib2.print_config arrays

  type t = {
    theories      : theory list;
    features      : features;
  }

  let print fmt { theories; features; } =
    Format.fprintf fmt "@[<hov 2>{ theories: @[<hov>%a@];@ features: %a;@ }]}"
      print_theories theories print_features features

  let all = {
    theories = [ `Core; `Arrays; `Bitvectors; `Floats; `String; `Reals_Ints ];
    features = {
      free_sorts = true;
      free_functions = true;
      datatypes = true;
      quantifiers = true;
      arrays = All;
      arithmetic = Regular;
    };
  }

  (*
  QF to disable the quantifier feature
  A or AX for the theory ArraysEx
  BV for the theory FixedSizeBitVectors
  FP (forthcoming) for the theory FloatingPoint
  IA for the theory Ints (Integer Arithmetic)
  RA for the theory Reals (Real Arithmetic)
  IRA for the theory Reals_Ints (mixed Integer Real Arithmetic)
  IDL for Integer Difference Logic (Ints theory + difference logic arithmetic)
  RDL for Reals Difference Logic (Reals theory + difference logic arithmetic)
  L before IA, RA, or IRA for the linear fragment of those arithmetics
  N before IA, RA, or IRA for the non-linear fragment of those arithmetics
  UF for the extension allowing free sort and function symbols
  *)
  let parse s =
    let default = {
      theories = [ `Core ];
      features = {
        free_sorts = false;
        free_functions = false;
        datatypes = false;
        quantifiers = true;
        arrays = All;
        arithmetic = Regular;
      };
    } in
    let add_theory t c = { c with theories = t :: c.theories } in
    let set_features c f = { c with features = f c.features } in
    let set_uf c = set_features c (fun f ->
        { f with free_sorts = true; free_functions = true; }) in
    let set_qf c = set_features c (fun f -> { f with quantifiers = false}) in
    let set_dt c = set_features c (fun f -> { f with datatypes = true}) in
    let set_idl c = set_features c (fun f -> { f with arithmetic = Difference `IDL}) in
    let set_rdl c = set_features c (fun f -> { f with arithmetic = Difference `RDL}) in
    let set_la c = set_features c (fun f -> { f with arithmetic = Linear `Strict}) in
    (* Entry-point for a best effort at parsing a logic name into a
       structured representation of what theories the logic includes and
       what restrictions it imposes. *)
    let rec parse_logic c l = parse_all c l
    (* The 'ALL' logic is described in the SMTlib language standard as
       a logic including all that is supported by the solver. *)
    and parse_all c = function
      | 'A'::'L'::'L'::l -> parse_end all l
      | l -> parse_qf c l
    (* The QF theory can only appear as a prefix "QF_" *)
    and parse_qf c = function
      | [] -> Some c
      | 'Q'::'F'::'_'::l -> parse_array (set_qf c) l
      | l -> parse_array c l
    (* The Array theory is specified first after an optional QF_, and
       can be one of two forms:
       - either 'A' followed by some other theories
       - either 'AX' followed by no theory *)
    and parse_array c = function
      | 'A'::'X'::l -> parse_end (add_theory `Arrays c) l
      | 'A'::[] -> None (* "QF_A" and "A" are not valid logics *)
      | 'A'::l  -> parse_uf (add_theory `Arrays c) l
      | l -> parse_uf c l
    (* The UF theory can be specified after the array theory *)
    and parse_uf c = function
      | 'U'::'F'::l -> parse_bv (set_uf c) l
      | l -> parse_bv c l
    (* After the QF, Array and UF theories have been specified,
       BV can be specified *)
    and parse_bv c = function
      | 'B'::'V'::l -> parse_dt_or_fp (add_theory `Bitvectors c) l
      | l -> parse_dt_or_fp c l
    (* DT and FP do not have clear ordering, so we allow to specify them in
       any order. *)
    and parse_dt_or_fp c = function
      | 'D'::'T'::l -> parse_fp (set_dt c) l
      | 'F'::'P'::l -> parse_dt (add_theory `Floats c) l
      | l -> parse_str c l
    (* DT *)
    and parse_dt c = function
      | 'D'::'T'::l -> parse_str (set_dt c) l
      | l -> parse_str c l
    (* FP theory *)
    and parse_fp c = function
      | 'F'::'P'::l -> parse_str (add_theory `Floats c) l
      | l -> parse_str c l
    (* String theory *)
    and parse_str c = function
      | 'S' :: l -> parse_arith (add_theory `String c) l
      | l -> parse_arith c l
    (* Some logics include both BV and arithmetic (e.g. AUFBVDTLIA) *)
    and parse_arith c = function
      | 'I'::'D'::'L'::l -> parse_end (add_theory `Ints (set_idl c)) l
      | 'R'::'D'::'L'::l -> parse_end (add_theory `Reals (set_rdl c)) l
      | 'L'::'I'::'A'::l -> parse_end (add_theory `Ints (set_la c)) l
      | 'L'::'R'::'A'::l -> parse_end (add_theory `Reals (set_la c)) l
      | 'L'::'I'::'R'::'A'::l -> parse_end (add_theory `Reals_Ints (set_la c)) l
      | 'N'::'I'::'A'::l -> parse_end (add_theory `Ints c) l
      | 'N'::'R'::'A'::l -> parse_end (add_theory `Reals c) l
      | 'N'::'I'::'R'::'A'::l -> parse_end (add_theory `Reals_Ints c) l
      | l -> parse_end c l
    (* End of list *)
    and parse_end c = function
      | [] -> Some c
      | _l ->
        (*
        let pp_sep fmt () = Format.fprintf fmt "; " in
        Format.eprintf "%a" (Format.pp_print_list ~pp_sep Format.pp_print_char) l;
        *)
        None
    in
    (* Parse the logic name *)
    let res = parse_logic default (Misc.Strings.to_list s) in
    (* Return *)
    match res with
    | None -> res
    | Some res ->
      (* Some special cases *)
      let res =
        match s with
        (* QF_AX allows free sort and **constant** symbols (not functions) *)
        | "QF_AX"
          -> set_features res (fun f -> { f with
                                          free_sorts = true; })
        (* QF_ABV has some array restrictions *)
        | "QF_ABV" | "QF_AUFBV"
          -> set_features res (fun f -> { f with
                                          arrays = Only_bitvec; })
        (* {QF_}AUFLIRA has specific array restrictions *)
        | "QF_AUFLIRA" | "AUFLIRA"
          -> set_features res (fun f -> { f with
                                          arrays = Only_ints_real; })
        (* {QF_}AUFLIA has some different arithmetic and array restrictions *)
        | "QF_AUFLIA" | "AUFLIA"
          -> set_features res (fun f -> { f with
                                          arrays = Only_int_int;
                                          arithmetic = Linear `Large; })
        (* QF_ALIA has the large arithmetic semantics *)
        | "QF_ALIA" | "ALIA"
          -> set_features res (fun f -> { f with
                                          arithmetic = Linear `Large})
        (* QF_UFIDL has a different spec for integer difference logic... *)
        | "QF_UFIDL" | "UFIDL"
          -> set_features res (fun f -> { f with
                                          arithmetic = Difference `UFIDL})
        (* Default case (for non-special cases) *)
        | _ -> res
      in
      Some res

  let to_string l =
    (* *)
    let b = Buffer.create 13 in
    assert (List.mem `Core l.theories);
    let n_th = List.length l.theories in
    (* TODO: these computations could all be shared by doing a single loop
       over the [l.theories] list, but performance should not matter here *)
    let arrays = List.mem `Arrays l.theories in
    let bitvs = List.mem `Bitvectors l.theories in
    let floats = List.mem `Floats l.theories in
    let strings = List.mem `String l.theories in
    let ints = List.mem `Ints l.theories in
    let reals = List.mem `Reals l.theories in
    let reals_ints = List.mem `Reals_Ints l.theories in
    (* QF *)
    if not l.features.quantifiers
    then Buffer.add_string b "QF_";
    (* Arrays *)
    if arrays then
      if n_th >= 3
      then Buffer.add_string b "A"
      else Buffer.add_string b "AX";
    (* UF *)
    if l.features.free_sorts || l.features.free_functions
    then Buffer.add_string b "UF";
    (* Bitvectors *)
    if bitvs
    then Buffer.add_string b "BV";
    (* Datatypes *)
    if l.features.datatypes
    then Buffer.add_string b "DT";
    (* Floats *)
    if floats
    then Buffer.add_string b "FP";
    (* Strings *)
    if strings
    then Buffer.add_string b "S";
    (* Arithmetic *)
    if ints
    then begin match l.features.arithmetic with
      | Difference `IDL -> Buffer.add_string b "IDL"
      | Linear _ -> Buffer.add_string b "LIA"
      | _ -> Buffer.add_string b "NIA"
    end else if reals
    then begin match l.features.arithmetic with
      | Difference `RDL -> Buffer.add_string b "RDL"
      | Linear _ -> Buffer.add_string b "LRA"
      | _ -> Buffer.add_string b "NRA"
    end else if reals_ints
    then begin match l.features.arithmetic with
      | Linear _ -> Buffer.add_string b "LIRA"
      | _ -> Buffer.add_string b "NIRA"
    end;
    let s = Buffer.contents b in
    (* Take care of irregular logics *)
    match s, l.features.arrays, l.features.arithmetic with
    (* BV+Arith restrictions / correct cases *)
    | ("QF_ABV"| "QF_AUFBV"), (Only_bitvec | None), _ -> s
    | ("QF_AUFLIRA" | "AUFLIRA"), Only_ints_real, _ -> s
    | ("QF_AUFLIA" | "AUFLIA"), Only_int_int, _ -> s
    | ("QF_ALIA" | "ALIA"), _, _ -> s
    | ("QF_UFIDL" | "UFIDL"), _, Difference `UFIDL -> s

    (* Incorrect cases *)
    | _, _, (Linear `Large | Difference `UFIDL) ->
      (* TODO: try and find a fallback logic ? *)
      assert false
    | _, (All | Only_int_int | Only_ints_real | Only_bitvec), arith_config ->
      Format.eprintf "Logic: %s / %a ?!@." s Arith.Smtlib2.print_config arith_config;
      (* TODO: try and find a fallback logic ? *)
      assert false


    (* in case we have quantifiers and only constants,
       the logic might end up empty. in that case, we default to UF *)
    | "", _, _ -> "UF"
    (* QF_ is not a typical logic, in that case we'll consider
       QF_UF to be the smallest logic *)
    | "QF_", _, _ -> "QF_UF"
    (* regular cases *)
    | _, _, _ -> s


  module Scan(V : Dolmen_intf.View.TFF.S) = struct

    (* Accumulator and helpers *)

    exception Unknown_ty_builtin of V.ty_cst
    exception Unknown_term_builtin of V.term_cst

    type arith_config =
      | Non_linear
      | No_constraint
      | Linear of [ `Strict | `Large ]
      | Difference of [ `Normal | `UFIDL ]

    type acc = {
      need_univ       : bool;
      need_unit       : bool;
      free_sorts      : bool;
      free_functions  : bool;
      quantifiers : bool;
      datatypes   : bool;
      bitvectors  : bool;
      bitv_lits   : bool;
      floats      : bool;
      strings     : bool;
      arrays      : Arrays.Smtlib2.config;
      ints        : bool;
      int_lits    : bool;
      reals       : bool;
      arith       : arith_config;
    }

    (* simple helpers *)

    let add_univ acc =
      if acc.need_univ then acc else { acc with need_univ = true; }
    let add_unit acc =
      if acc.need_unit then acc else { acc with need_unit = true; }
    let add_free_sort acc =
      if acc.free_sorts then acc else { acc with free_sorts = true; }
    let add_free_funs acc =
      if acc.free_functions then acc else { acc with free_functions = true; }
    let add_quants acc =
      if acc.quantifiers then acc else { acc with quantifiers = true; }
    let add_datatypes acc =
      if acc.datatypes then acc else { acc with datatypes = true; }
    let add_bitvs acc =
      if acc.bitvectors then acc else { acc with bitvectors = true; }
    let add_bitv_lits acc =
      if acc.bitv_lits then acc else { acc with bitv_lits = true; }
    let add_floats acc =
      if acc.floats then acc else { acc with floats = true; }
    let add_string acc =
      if acc.strings then acc else { acc with strings = true; }

    (* arithmetic helpers *)

    type arith = [ `Int | `Real ]

    let add_int_lits acc =
      if acc.int_lits then acc else { acc with int_lits = true; }

    let add_arith kind acc =
      match kind with
      | `Int -> if acc.ints then acc else { acc with ints = true; }
      | `Real -> if acc.reals then acc else { acc with reals = true; }

    let add_dl_arith kind acc =
      let acc = add_arith kind acc in
      match acc.arith with
      | Non_linear -> acc
      | Difference _ -> acc
      | No_constraint -> { acc with arith = Difference `Normal; }
      | Linear _ -> acc

    let _add_ufidl_arith kind acc =
      let acc = add_arith kind acc in
      match acc.arith with
      | Non_linear | Difference `UFIDL -> acc
      | Difference `Normal -> { acc with arith = Difference `UFIDL; }
      | No_constraint -> { acc with arith = Difference `Normal; }
      | Linear _ -> acc

    let add_linear_arith_strict kind acc =
      let acc = add_arith kind acc in
      match acc.arith with
      | Non_linear | Linear (`Strict | `Large) -> acc
      | _ -> { acc with arith = Linear `Strict; }

    let add_linear_arith_large kind acc =
      let acc = add_arith kind acc in
      match acc.arith with
      | Non_linear | Linear `Large -> acc
      | _ -> { acc with arith = Linear `Large; }

    let add_non_linear_arith kind acc =
      let acc = add_arith kind acc in
      match acc.arith with
      | Non_linear -> acc
      | _ -> { acc with arith = Non_linear; }

    (* array helpers *)

    let add_any_array acc =
      match acc.arrays with
      | All -> acc
      | _ -> { acc with arrays = All; }

    let add_int_int_array acc =
      match acc.arrays with
      | None -> { acc with arrays = Only_int_int; }
      | All | Only_int_int -> acc
      | Only_ints_real | Only_bitvec ->
        { acc with arrays = All; }

    let add_int_real_array acc =
      match acc.arrays with
      | None -> { acc with arrays = Only_ints_real; }
      | All | Only_ints_real -> acc
      | Only_int_int | Only_bitvec ->
        { acc with arrays = All; }

    let add_bitv_array acc =
      match acc.arrays with
      | None -> { acc with arrays = Only_bitvec; }
      | All | Only_bitvec -> acc
      | Only_int_int | Only_ints_real ->
        { acc with arrays = All; }

    (* Type scanning
       Here, we are interested in the actual concrete types, hence
       we expand the types when viewing them. *)

    let rec ty_array_view ty =
      match V.Ty.view ~expand:true ty with
      | Var _ -> `Other
      | App (f, args) ->
        begin match V.Ty.Cst.builtin f with
          | B.Arith Int -> `Int
          | B.Arith Real -> `Real
          | B.Bitv T _ -> `Bitv
          | B.Array T ->
            begin match List.map ty_array_view args with
              | [indexes; elts] -> `Arrays (indexes, elts)
              | _ -> assert false (* incorrect use of builtin B.Array *)
            end
          | _ -> `Other
        end

    let rec scan_ty acc ty =
      match V.Ty.view ~expand:true ty with
      | Var _ -> acc
      | App (f, args) ->
        let aux acc = List.fold_left scan_ty acc args in
        begin match V.Ty.Cst.builtin f with
          | B.Base -> aux (add_free_sort acc)
          | B.Univ -> aux (add_univ acc)
          | B.Unit -> aux (add_unit acc)
          | B.Prop T -> aux acc
          | B.Arith Int -> aux (add_arith `Int acc)
          | B.Arith Real -> aux (add_arith `Real acc)
          | B.Bitv T _ -> aux (add_bitvs acc)
          | B.Float T _ | B.Float RoundingMode -> aux (add_floats acc)
          | B.Str T | B.Regexp T -> aux (add_string acc)
          | B.Array T ->
            begin match args with
              | [indexes; elts] ->
                begin match ty_array_view indexes, ty_array_view elts with
                  | `Int, `Int -> aux (add_int_int_array acc)
                  | `Int, `Real -> aux (add_int_real_array acc)
                  | `Int, `Array (`Int, `Real) -> aux (add_int_real_array acc)
                  | `Bitv, `Bitv -> aux (add_bitv_array acc)
                  | _ -> aux (add_any_array acc)
                end
              | _ -> assert false (* incorrect use of builtin B.Array *)
            end
          | _ ->
            raise (Unknown_ty_builtin f)
        end

    (* term decl *)

    let scan_term_decl ~in_adt acc c =
      let Signature (_, params_ty, ret) = V.Sig.view ~expand:true (V.Term.Cst.ty c) in
      let acc = List.fold_left scan_ty (scan_ty acc ret) params_ty in
      match params_ty with
      | _ :: _ when not in_adt -> add_free_funs acc
      | _ -> acc

    (* term scanning
       TODO: correctly handle let-bound variables by storing them in the env,
       and examining the defining expr instead of the let-bound variable. *)

    let rec term_arith_view t =
      match V.Term.view t with
      | Var v -> `Variable v
      | App (head, ty_args, t_args) ->
        begin match V.Term.Cst.builtin head, ty_args, t_args with
          | B.Base, [], [] -> `Constant head
          | B.Arith blt, _, _ ->
            begin match blt, ty_args, t_args with
              | Integer s, _, _ -> `Numeral s
              | Decimal s, _, _ -> `Decimal s
              | Minus #arith, _, [t'] -> `Negation t'
              | Add #arith, _, l -> `Addition l
              | Sub #arith, _, l -> `Substraction l
              | Div `Real, _, [x; y] -> `Division (x, y)
              | Lt #arith, _, _ | Leq #arith, _, _
              | Gt #arith, _, _ | Geq #arith, _, _
              | Mul #arith, _, _ | Pow #arith, _, _
              | Div_e #arith, _, _ | Div_t #arith, _, _ | Div_f #arith, _, _
              | Modulo_e #arith, _, _ | Modulo_t #arith, _, _ | Modulo_f #arith, _, _
              | Abs, _, _ | Divisible, _, _
              | Is_int #arith, _, _ | Is_rat #arith, _, _
              | Floor #arith, _, _ | Floor_to_int `Real, _, _
              | Ceiling #arith, _, _ | Truncate #arith, _, _
              | Round #arith, _, _
                -> `Complex_arith
              | _ ->
                assert false (* internal assumption: we should have already matched all cases that make sense *)
            end
          | B.Coercion, [src; dst], [t] ->
            begin match ty_array_view src, ty_array_view dst with
              | `Int, `Real -> term_arith_view t
              | _ -> `Top_symbol_not_in_arith
            end
          | _ -> `Top_symbol_not_in_arith
        end
      | _ -> `Top_symbol_not_in_arith

    let rec term_arith_classify t =
      match term_arith_view t with
      | `Numeral _ -> `Int_coef
      | `Decimal _ -> `Rat_coef
      | `Addition _ -> `Complex_arith
      | `Substraction _ -> `Complex_arith
      | `Negation t' ->
        begin match term_arith_view t' with
          | `Numeral _ -> `Int_coef
          | `Decimal _ -> `Rat_coef
          | _ -> `Complex_arith
        end
      | `Division (numerator, denominator) ->
        begin match term_arith_classify numerator, term_arith_view denominator with
          | `Int_coef, `Numeral s when s <> "0" -> `Rat_coef
          | _ -> `Complex_arith
        end
      | `Variable _ | `Constant _ -> `Var_or_cst
      | `Complex_arith -> `Complex_arith
      | `Top_symbol_not_in_arith -> `Top_symbol_not_in_arith

    let rec term_arith_difference_count t =
      match term_arith_view t with
      | `Variable v -> `Ok (`Var v, 1)
      | `Constant c -> `Ok (`Cst c, 1)
      | `Addition l -> term_arith_difference_count_list `Init l
      | _ -> `Nope

    and term_arith_difference_count_list acc = function
      | [] ->
        begin match acc with
          | `Ok _ as res -> res
          | `Init -> assert false (* incorrect use of B.Add (empty list of args) *)
        end
      | h :: r ->
        begin match term_arith_difference_count h, acc with
          | `Nope, _ -> `Nope
          | `Ok _ as acc, `Init -> term_arith_difference_count_list acc r
          | `Ok (s, n), `Ok (s', n') ->
            begin match s, s' with
              | `Var v, `Var v' when V.Term.Var.equal v v' ->
                term_arith_difference_count_list (`Ok ((`Var v), n + n')) r
              | `Cst c, `Cst c' when V.Term.Cst.equal c c' ->
                term_arith_difference_count_list (`Ok ((`Cst c), n + n')) r
              | _ -> `Nope
            end
        end

    let rec scan_term acc t =
      match V.Term.view t with
      | Var _ -> acc
      | App (head, ty_args, t_args) -> scan_app acc head ty_args t_args
      | Match (scrutinee, cases) -> scan_match acc scrutinee cases
      | Binder (Exists { type_vars = _; term_vars; triggers = _; }, body) ->
        scan_quant acc term_vars body
      | Binder (Forall { type_vars = _; term_vars; triggers = _; }, body) ->
        scan_quant acc term_vars body
      | Binder ((Map term_vars), body) ->
        (* TODO: add a field to track whether we need HO-CORE ? *)
        scan_quant acc term_vars body
      | Binder ((Letand l | Letin l), body) ->
        scan_let acc l body

    and scan_match acc scrutinee cases =
      let acc = add_datatypes acc in
      let acc = scan_term acc scrutinee in
      scan_arms acc cases

    and scan_arms acc = function
      | [] -> acc
      | (_pat, arm) :: r -> scan_arms (scan_term acc arm) r

    and scan_quant acc term_vars body =
      let acc = add_quants acc in
      let acc = List.fold_left (fun acc v ->
          let ty = V.Term.Var.ty v in
          scan_ty acc ty
        ) acc term_vars
      in
      scan_term acc body

    and scan_let acc l body =
      let acc = List.fold_left (fun acc (_v, defining_expr) ->
          scan_term acc defining_expr
        ) acc l
      in
      scan_term acc body

    and scan_app acc f ty_args t_args =
      let aux_terms acc =
        List.fold_left scan_term acc t_args
      in
      let aux acc =
        aux_terms (List.fold_left scan_ty acc ty_args)
      in
      match (V.Term.Cst.builtin f) with

      | B.Base -> aux acc

      (* int->real conversions on literals/coefs do not bring in the Int theory *)
      | B.Coercion ->
        begin match List.map ty_array_view ty_args with
          | [`Int; `Real] ->
            begin match List.map term_arith_view t_args with
              | [`Numeral _] -> add_int_lits acc
              (* TODO: add reals_ints since to_real was used *)
              | _ -> aux acc
            end
          | _ -> aux acc
        end

      (* Datatypes *)
      | B.Adt (Constructor _ | Destructor _ | Tester _) ->
        aux (add_datatypes acc)

      (* Core *)
      | B.Prop (True | False
               | Neg | And | Or | Nor | Xor
               | Imply | Implied | Ite | Equiv)
        -> aux acc

      | B.Equal | B.Distinct
        (* In this case, we don't really want the type of the values being
           compared to influence the logics. In particular, the FP theory
           can create and compare values of bitvector type without the
           BV theory. *)
        -> aux_terms acc

      (* Arrays
         for these, we try and accurately track exactly what kind
         of arrays are used (e.g. only ints-ints, ints-reals, etc..).
         To do that, we only need to look at the type of arrays used,
         for which we only need to look at the type of the first argument.
      *)
      | B.(Array (Store | Select)) ->
        begin match t_args with
          | arr :: _ -> aux (scan_ty acc (V.Term.ty arr))
          | _ -> assert false (* incorrect use of the B.Store/B.Select builtins *)
        end

      (* Arithmetic
         We have to try and track smtlib2's insane list of specifications.
         To do this, we replicate most of the logic that is present in the [Arith]
         module for typechecking. *)
      | B.Arith blt ->
        begin match blt with
          | Int | Rat | Real -> assert false (* cannot occur in terms *)
          | Integer _ -> aux (add_int_lits acc)
          | Decimal _ -> aux (add_arith `Real acc)

          | Abs -> aux (add_arith `Int acc)

          | Floor_to_int `Real ->
            aux (add_arith `Real (add_arith `Int acc))


          | Is_int (`Real as k)
          | Lt (#arith as k) | Leq (#arith as k)
          | Gt (#arith as k) | Geq (#arith as k)
            -> aux (add_arith k acc)

          | Minus (#arith as k) ->
            begin match t_args with
              | [t'] ->
                begin match term_arith_view t' with
                  | `Numeral _ | `Decimal _ -> add_dl_arith k acc
                  | _ -> aux (add_linear_arith_strict k acc)
                end
              | _ -> assert false (* incorrect use of B.Minus *)
            end

          | Add (`Int as k) -> add_linear_arith_strict k acc
          | Add (`Real as k) ->
            begin match term_arith_difference_count_list `Init t_args with
              | `Ok _ -> add_dl_arith k acc
              | `Nope -> aux (add_linear_arith_strict k acc)
            end

          | Sub (`Int as k) ->
            begin match t_args with
              | [a; b] ->
                begin match term_arith_classify a, term_arith_classify b with
                  | `Var_or_cst, `Var_or_cst -> add_dl_arith k acc
                  | _ -> aux (add_linear_arith_strict k acc)
                end
              | _ -> aux (add_linear_arith_strict k acc)
            end

          | Sub (`Real as k) ->
            begin match t_args with
              | [a; b] ->
                begin match term_arith_difference_count a, term_arith_difference_count b with
                  | `Ok (_, n), `Ok (_, n') when n = n' && n > 1 -> add_dl_arith k acc
                  | _ -> aux (add_linear_arith_strict k acc)
                end
              | _ -> aux (add_linear_arith_strict k acc)
            end

          | Div (`Real as k)
          | Div_e (`Int as k)
          | Modulo_e (`Int as k)->
            begin match t_args with
              | [a; b] ->
                begin match term_arith_classify a, term_arith_view b with
                  | `Int_coef, `Numeral s when s <> "0" -> add_linear_arith_strict k acc
                  | _ -> aux (add_non_linear_arith k acc)
                end
              | _ -> assert false (* incorrect use of B.Div/B.Div_e/B.Modulo_e *)
            end

          | Divisible -> aux (add_non_linear_arith `Int acc)

          | Mul (#arith as k) ->
            begin match t_args with
              | [a; b] ->
                begin match term_arith_classify a, term_arith_classify b with
                  | (`Int_coef | `Rat_coef), `Var_or_cst
                  | `Var_or_cst, (`Int_coef | `Rat_coef)
                    -> aux (add_linear_arith_strict k acc)
                  | (`Int_coef | `Rat_coef), `Top_symbol_not_in_arith
                  | `Top_symbol_not_in_arith, (`Int_coef | `Rat_coef)
                    -> aux (add_linear_arith_large k acc)
                  | (`Int_coef | `Rat_coef), (`Int_coef | `Rat_coef)
                  | (`Int_coef | `Rat_coef), `Complex_arith
                  | `Complex_arith, (`Int_coef | `Rat_coef)
                    -> (* NOTE: this is annoying, but it is the spec... *)
                    aux (add_non_linear_arith k acc)
                  | _ -> aux (add_non_linear_arith k acc)
                end
              | _ -> assert false (* Incorrect use of B.Mul *)
            end

          | Rational _
          | Floor_to_int `Rat
          | Is_int (`Int | `Rat)
          | Lt `Rat | Leq `Rat | Gt `Rat | Geq `Rat
          | Minus `Rat | Add `Rat | Sub `Rat | Mul `Rat | Div `Rat
          | Div_e (`Rat | `Real) | Modulo_e (`Rat | `Real)
          | Pow _ | Div_t _ | Modulo_t _ | Div_f _ | Modulo_f _
          | Is_rat _ | Floor _ | Ceiling _ | Truncate _ | Round _
            (* These builtins cannot be printed in smtlib scripts.
               TODO: should this be an error instead ? *)
            -> acc

        end

      (* Bitvectors *)
      | B.Bitv blt ->
        begin match blt with
          | T _ -> assert false (* cannot occur in terms *)
          | Binary_lit _ -> aux (add_bitv_lits acc)
          | Not _ | And _ | Or _
          | Nand _ | Nor _
          | Xor _ | Xnor _
          | Comp _
          | Neg _ | Add _ | Sub _ | Mul _
          | Udiv _ | Urem _
          | Sdiv _ | Srem _ | Smod _
          | Shl _ | Lshr _ | Ashr _
          | Ult _ | Ule _
          | Ugt _ | Uge _
          | Slt _ | Sle _
          | Sgt _ | Sge _ | Concat _
          | Repeat _ | Zero_extend _ | Sign_extend _
          | Rotate_right _ | Rotate_left _ | Extract _
          | Overflow_neg _
          | Overflow_add _ | Overflow_sub _
          | Overflow_mul _ | Overflow_div _
            -> aux (add_bitvs acc)
          (* 2.6 BVconv extension, 2.7 regular *)
          | To_int _ | Of_int _
            -> aux (add_bitvs (add_arith `Int acc))
        end

      (* Floats *)
      | B.Float _ -> aux (add_floats acc)

      (* B.Strings and regular languages *)
      | B.Str _ | B.Regexp _ -> aux (add_string acc)

      | b ->
        (* non-smtlib builtin *)
        let name = Obj.Extension_constructor.(name (of_val b)) in
        failwith (Format.asprintf "unknown_builtin : %s" name)


    (* Acc <-> logic conversion *)
    let to_logic acc =
      (* features *)
      let features = {
        free_sorts = acc.free_sorts;
        free_functions = acc.free_functions;
        datatypes = acc.datatypes;
        quantifiers = acc.quantifiers;
        arithmetic =
          begin match acc.arith with
            | No_constraint ->
              (* by default, we prefer linear arith (over DL) if there is no constraint *)
              Linear `Strict
            | Non_linear -> Regular
            | Linear k -> Linear k
            | Difference `Normal ->
              if acc.ints && not acc.reals then
                if not acc.free_sorts && not acc.free_functions
                then Difference `IDL
                else Difference `UFIDL
              else if not acc.ints && acc.reals &&
                      not acc.free_sorts && not acc.free_functions
              then Difference `RDL
              else Linear `Strict
            | Difference `UFIDL -> Difference `UFIDL
          end;
        arrays = acc.arrays
      } in
      (* theories *)
      let add b (th : theory) l = if b then th :: l else l in
      let theories =
        [ `Core ]
        |> add (acc.bitvectors || (acc.bitv_lits && not acc.floats)) `Bitvectors
        |> add acc.floats `Floats
        |> add acc.strings `String
        |> add (match acc.arrays with None -> false | _ -> true) `Arrays
        |> add ((acc.int_lits || acc.ints) && not acc.reals) `Ints
        |> add (acc.reals && not acc.ints) `Reals
        |> add (acc.ints && acc.reals) `Reals_Ints
      in
      { theories; features; }

    let need_univ acc = acc.need_univ
    let need_unit acc = acc.need_unit

    let nothing = {
      need_univ = false;
      need_unit = false;
      free_sorts = false;
      free_functions = false;
      quantifiers = false;
      datatypes = false;
      bitvectors = false;
      bitv_lits = false;
      floats = false;
      strings = false;
      arrays = None;
      ints = false;
      int_lits = false;
      reals = false;
      arith = No_constraint;
    }

  end

end

(* All logics *)
(* ************************************************************************ *)

type t =
  | Auto (* Default case for languages which do not have logic *)
  | Smtlib2 of Smtlib2.t

let print fmt = function
  | Auto -> Format.fprintf fmt "auto"
  | Smtlib2 smtlib2 -> Smtlib2.print fmt smtlib2


