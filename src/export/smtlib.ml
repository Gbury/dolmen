(* This module contains the pretty-printer functions for smtlib *)

open Dolmen_std

open Term

(* auxiliar functions *)
let pp_builtins = function
  | Wildcard -> Pretty.mk "$_" Pretty.Prefix
  | Ttype -> Pretty.mk "Type" Pretty.Prefix
  | Prop -> Pretty.mk "$o" Pretty.Prefix
  | True -> Pretty.mk "true" Pretty.Prefix
  | False -> Pretty.mk "false" Pretty.Prefix
  | Eq -> Pretty.mk "=" Pretty.Infix
  | Distinct -> Pretty.mk "$distinct" Pretty.Prefix
  | Ite -> Pretty.mk "$ite" Pretty.Prefix
  | Sequent -> Pretty.mk "-->" Pretty.Infix
  | Int -> Pretty.mk "$int" Pretty.Prefix
  | Minus -> Pretty.mk "$uminus" Pretty.Prefix
  | Add -> Pretty.mk "$sum" Pretty.Prefix
  | Sub -> Pretty.mk "$difference" Pretty.Prefix
  | Mult -> Pretty.mk "$product" Pretty.Prefix
  | Lt -> Pretty.mk "$less" Pretty.Prefix
  | Leq -> Pretty.mk "$lesseq" Pretty.Prefix
  | Gt -> Pretty.mk "$greater" Pretty.Prefix
  | Geq -> Pretty.mk "$greatereq" Pretty.Prefix
  | Subtype -> Pretty.mk "<<" Pretty.Infix
  | Product -> Pretty.mk "*" Pretty.Infix
  | Union -> Pretty.mk "+" Pretty.Infix
  | Not -> Pretty.mk "~" Pretty.Prefix
  | And -> Pretty.mk "&" Pretty.Infix ~assoc:Pretty.Left
  | Or -> Pretty.mk "|" Pretty.Infix ~assoc:Pretty.Left
  | Nand -> Pretty.mk "~&" Pretty.Infix
  | Xor -> Pretty.mk "<~>" Pretty.Infix
  | Nor -> Pretty.mk "~|" Pretty.Infix
  | Imply -> Pretty.mk "=>" Pretty.Infix ~assoc:Pretty.Right
  | Implied -> Pretty.mk "<=" Pretty.Infix
  | Equiv -> Pretty.mk "<=>" Pretty.Infix
;;

let pp_binder = function
  | All -> Pretty.mk "forall" Pretty.Prefix
  | Ex -> Pretty.mk "exists" Pretty.Prefix
  | Pi -> assert false
  | Arrow -> assert false
  | Let -> assert false
  | Fun -> assert false
  | Choice -> assert false
  | Description -> assert false
;;

let pp_descr = function
  | Symbol id -> Pretty.mk "%symb%" Pretty.Prefix
  | Builtin builtin -> assert false
  | Colon (term1, term2) -> assert false
  | App (term1, term2) -> assert false
  | Binder (binder, term_list1, term_list2) -> assert false
  | Match (term, term_pair_list) -> assert false
;;

let pp = function
  | { term = (Symbol _) as d; _ }
  | { term = (Builtin _) as d; _ } -> pp_descr d
  | e -> ( match (pp_descr e.term) with { name=n; _} -> Pretty.mk ("("^n^")") Pretty.Prefix )

open Statement

let pp_descr = function
  | Pack term_list -> assert false
  | Pop n -> assert false
  | Push n -> assert false
  | Reset_assertions -> assert false
  | Plain term -> assert false
  | Prove term_list -> assert false
  | Clause term_list -> assert false
  | Antecedent term -> assert false
  | Consequent term -> assert false
  | Include str -> assert false
  | Set_logic str -> assert false
  | Get_info str -> assert false
  | Set_info term -> assert false
  | Get_option str -> assert false
  | Set_option term -> assert false
  | Def (id, term) -> assert false
  | Decl (id, term) -> assert false
  | Inductive inductive -> assert false
  | Get_proof -> assert false
  | Get_unsat_core -> assert false
  | Get_unsat_assumptions -> assert false
  | Get_model -> assert false
  | Get_value term_list -> assert false
  | Get_assignment -> assert false
  | Get_assertions -> assert false
  | Echo str -> assert false
  | Reset -> assert false
  | Exit -> assert false
;;

let pp_variables = assert false

(* terms *)
let pp_terms = assert false

(* statements *)
let pp_statements = assert false
