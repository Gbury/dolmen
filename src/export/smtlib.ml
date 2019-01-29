(* This module contains the pretty-printer functions for smtlib *)

open Dolmen_std
open Pretty
open Term

(* auxiliar functions *)
let pretty_builtin = function
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

let print_builtin fmt builtin =
  Format.fprintf fmt "%s" (pretty_builtin builtin).name

let print_binder = function
  | All -> "forall"
  | Ex -> "exists"
  | Pi -> assert false
  | Arrow -> "->"
  | Let -> assert false
  | Fun -> ""
  | Choice -> assert false
  | Description -> assert false

let rec print_descr fmt = function
  | Symbol id -> Id.print fmt id
  | Builtin builtin -> print_builtin fmt builtin
  | Colon (term1, term2) ->
      Format.fprintf fmt "%a %a" print_term term1 print_term term2
  | App (term, term_list) ->
      Format.fprintf fmt "(%a %a)" print term
        (Misc.print_list ~print_sep:Format.fprintf ~sep:" " ~print:print_term)
        term_list
  | Binder (binder, term_list, term) when term_list <> [] ->
      Format.fprintf fmt "%s ((%a)) @ %a" (print_binder binder)
        (Misc.print_list ~print_sep:Format.fprintf ~sep:") (" ~print:print_term)
        term_list print_term term
  | Binder (binder, term_list, term) when term_list = [] ->
      Format.fprintf fmt "%s () @ %a" (print_binder binder) print_term term
  | Match (term, term_pair_list) -> assert false

and print_variables v = assert false

(* terms *)
and print_term fmt = function
  | {term= Symbol _ as d; _} | {term= Builtin _ as d; _} -> print_descr fmt d
  | e -> Format.fprintf fmt "%a" print_descr e.term

open Statement

(* statements *)
let rec print_statement fmt = function
  | Pack statement_list -> List.iter (fun a -> print fmt a) statement_list
  | Pop n -> assert false
  | Push n -> assert false
  | Reset_assertions -> assert false
  | Plain term -> assert false
  | Prove term_list ->
      Format.fprintf fmt "(check-sat %a) @."
        (Misc.print_list ~print_sep:Format.fprintf ~sep:" @ " ~print:print_term)
        term_list
  | Clause term_list -> assert false
  | Antecedent term ->
      Format.fprintf fmt "@[<hov 2>(assert@ %a)@] @." print_term term
  | Consequent term -> assert false
  | Include str -> assert false
  | Set_logic str -> Format.fprintf fmt "(set-logic %s) @." str
  | Get_info str -> Format.fprintf fmt "@[<hov 2>(get-info@ %s)@]" str
  | Set_info term -> Format.fprintf fmt "(set-info %a) @." print_term term
  | Get_option str -> assert false
  | Set_option term -> assert false
  | Def (id, ({term= Binder (binder, _, _)} as term)) ->
      Format.fprintf fmt "@[<hov 2>(define-%s @ %s @ %a)@] @."
        ( match id.ns with
        | Sort -> "sort"
        | Term -> if binder = Fun then "fun" else assert false
        | _ -> assert false )
        (* way to detect a sort or a binded term *)
        id.name print_term term
  | Def (id, _) -> assert false
  | Decl (id, term) ->
      Format.fprintf fmt "@[<hov 2>(declare-fun @ %a @ %a)@] @." Id.print id
        print_term term
  | Inductive i ->
      let induct fmt =
        let x =
          Misc.print_list ~print_sep:Format.fprintf ~sep:" "
            ~print:(fun fmt (cstr, l) ->
              match l with
              | [] -> Format.fprintf fmt "(%a)" Id.print cstr
              | l ->
                  Format.fprintf fmt "(%a (%a) )" Id.print cstr
                    (Misc.print_list ~print_sep:Format.fprintf ~sep:") ("
                       ~print:print_term)
                    l )
        in
        function
        | 0 -> Format.fprintf fmt "(%a)" x i.cstrs
        | _ ->
            Format.fprintf fmt "(par (%a) (%a) )"
              (Misc.print_list ~print_sep:Format.fprintf ~sep:" "
                 ~print:print_term)
              i.vars x i.cstrs
      in
      Format.fprintf fmt
        "@[<hov 2>(declare-datatypes @ ((%a %s)) @ (%a) ) @] @." Id.print i.id
        (string_of_int (List.length i.vars))
        induct (List.length i.vars)
  | Get_proof -> assert false
  | Get_unsat_core -> assert false
  | Get_unsat_assumptions -> assert false
  | Get_model -> Format.fprintf fmt "(get-model) @."
  | Get_value term_list -> assert false
  | Get_assignment -> assert false
  | Get_assertions -> assert false
  | Echo str -> assert false
  | Reset -> assert false
  | Exit -> assert false

and print fmt = function
  | {descr; _} -> Format.fprintf fmt "%a" print_statement descr
