(* This module contains the pretty-printer functions for smtlib *)

open Dolmen_std
open Pretty
open Term

(* auxiliar functions *)
let print_builtins = function
  | Wildcard -> assert false
  | Ttype -> assert false
  | Prop -> assert false
  | True -> Pretty.mk "true" Pretty.Prefix
  | False -> Pretty.mk "false" Pretty.Prefix
  | Eq -> Pretty.mk "=" Pretty.Prefix
  | Distinct -> assert false
  | Ite -> Pretty.mk "ite" Pretty.Prefix
  | Sequent -> assert false
  | Int -> assert false
  | Minus -> Pretty.mk "-" Pretty.Prefix
  | Add -> Pretty.mk "+" Pretty.Prefix
  | Sub -> Pretty.mk "-" Pretty.Prefix
  | Mult -> Pretty.mk "*" Pretty.Prefix
  | Lt -> Pretty.mk "<" Pretty.Prefix
  | Leq -> Pretty.mk "<=" Pretty.Prefix
  | Gt -> Pretty.mk ">" Pretty.Prefix
  | Geq -> Pretty.mk ">=" Pretty.Prefix
  | Subtype -> assert false
  | Product -> assert false
  | Union -> assert false
  | Not -> Pretty.mk "not" Pretty.Prefix
  | And -> Pretty.mk "and" Pretty.Prefix
  | Or -> Pretty.mk "or" Pretty.Prefix
  | Nand -> assert false
  | Xor -> assert false
  | Nor -> assert false
  | Imply -> Pretty.mk "=>" Pretty.Prefix
  | Implied -> assert false
  | Equiv -> assert false
;;

let print_binder = function
  | All -> Pretty.mk "forall" Pretty.Prefix
  | Ex -> Pretty.mk "exists" Pretty.Prefix
  | Pi -> assert false
  | Arrow -> assert false
  | Let -> assert false
  | Fun -> Pretty.mk "fun" Pretty.Prefix
  | Choice -> assert false
  | Description -> assert false
;;

let rec print_descr = function
  | Symbol id -> Pretty.mk (Dolmen_std.Id.full_name id) Pretty.Prefix
  | Builtin builtin -> assert false
  | Colon (term1, term2) ->
    Pretty.mk ((print_terms_ term1).name ^ " " ^ (print_terms_ term2).name) Pretty.Prefix
  | App (term1, term2) -> assert false
  | Binder (binder, term_list, term) ->
    Pretty.mk
      ((print_fold_terms term_list).name ^ " " ^ (print_terms_ term).name)
      Pretty.Infix
  | Match (term, term_pair_list) -> assert false

and print_fold_terms t =
  List.fold_left
    (fun a b -> {a with name = a.name ^ (print_terms_ b).name})
    (Pretty.mk "" Pretty.Prefix)
    t

and print_variables v = assert false

(* terms *)
and print_terms_ = function
  | {term = Symbol _ as d; _} | {term = Builtin _ as d; _} -> print_descr d
  | e -> Pretty.mk ("(" ^ (print_descr e.term).name ^ ")") Pretty.Prefix
;;

let print_terms fmt t = Format.fprintf fmt "%s" (print_terms_ t).name

open Statement

(* statements *)
let print_statements fmt = function
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
  | Set_logic str -> Format.fprintf fmt "(set-logic %s)" str
  | Get_info str -> assert false
  | Set_info term -> Format.fprintf fmt "(set-info %a)" print_terms term
  | Get_option str -> assert false
  | Set_option term -> assert false
  | Def (id, ({term = Binder (binder, _, _)} as term)) ->
    Format.fprintf
      fmt
      "(define-%s %s %a)"
      (print_binder binder).name
      (Dolmen_std.Id.full_name id)
      print_terms
      term
  | Def (id, _) -> assert false
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

let print fmt = function {descr; _} -> Format.fprintf fmt "%a" print_statements descr
