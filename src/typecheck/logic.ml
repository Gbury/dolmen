
(* Smtlib2 logics *)
(* ************************************************************************ *)

module Smtlib2 = struct

  type theory = [
    | `Core
    | `Arrays
    | `Bitvectors
    | `Floats
    | `Ints
    | `Reals
    | `Reals_Ints
  ]

  type features = {
    free_sorts      : bool;
    free_functions  : bool;
    datatypes       : bool;
    quantifiers     : bool;
    arithmetic      : [ `Linear_large | `Linear_strict
                      | `Difference | `Regular ];
  }

  type t = {
    theories      : theory list;
    features      : features;
  }

  let all = {
    theories = [ `Core; `Arrays; `Bitvectors; `Floats; `Reals_Ints ];
    features = {
      free_sorts = true;
      free_functions = true;
      datatypes = true;
      quantifiers = true;
      arithmetic = `Regular;
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
        arithmetic = `Regular;
      };
    } in
    let add_theory t c = { c with theories = t :: c.theories } in
    let set_features c f = { c with features = f c.features } in
    let set_uf c = set_features c (fun f ->
        { f with free_sorts = true; free_functions = true; }) in
    let set_qf c = set_features c (fun f -> { f with quantifiers = false}) in
    let set_dt c = set_features c (fun f -> { f with datatypes = true}) in
    let set_dl c = set_features c (fun f -> { f with arithmetic = `Difference}) in
    let set_la c = set_features c (fun f -> { f with arithmetic = `Linear_strict}) in
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
      | 'U'::'F'::l -> parse_arith (set_uf c) l
      | l -> parse_arith c l
    (* After the QF, Array and UF theories have been specified,
       one of the BV or some arithmetic theory can be specified. *)
    and parse_arith c = function
      | 'B'::'V'::l -> parse_dt (add_theory `Bitvectors c) l
      | 'I'::'D'::'L'::l -> parse_dt (add_theory `Ints (set_dl c)) l
      | 'R'::'D'::'L'::l -> parse_dt (add_theory `Reals (set_dl c)) l
      | 'L'::'I'::'A'::l -> parse_dt (add_theory `Ints (set_la c)) l
      | 'L'::'R'::'A'::l -> parse_dt (add_theory `Reals (set_la c)) l
      | 'L'::'I'::'R'::'A'::l -> parse_dt (add_theory `Reals_Ints (set_la c)) l
      | 'N'::'I'::'A'::l -> parse_dt (add_theory `Ints c) l
      | 'N'::'R'::'A'::l -> parse_dt (add_theory `Reals c) l
      | 'N'::'I'::'R'::'A'::l -> parse_dt (add_theory `Reals_Ints c) l
      | l -> parse_dt c l
    (* TODO: where can the DT theory appear ? *)
    and parse_dt c = function
      | 'D'::'T'::l -> parse_fp (set_dt c) l
      | l -> parse_fp c l
    (* TODO: where cna the FP theory appear ? *)
    and parse_fp c = function
      | 'F'::'P'::l -> parse_end (add_theory `Floats c) l
      | l -> parse_end c l
    (* End of list *)
    and parse_end c = function
      | [] -> Some c
      | _ -> None
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
          ->
          set_features res (fun f -> { f with free_sorts = true; })
        (* Some logics allow more linear expressions than others *)
        | "QF_AUFLIA" | "AUFLIA"
        | "QF_ALIA" | "ALIA"
          ->
          set_features res (fun f -> { f with arithmetic = `Linear_large})
        (* Default case (for non-special cases) *)
        | _ -> res
      in
      Some res

end

(* All logics *)
(* ************************************************************************ *)

type t =
  | Auto (* Default case for languages which do not have logic *)
  | Smtlib2 of Smtlib2.t

