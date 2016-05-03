
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

type location = ParseLocation.t

type builtin =
  | Wildcard
  | True | False
  | Eq | Distinct       (* Should all args be pairwise distinct or equal ? *)

  | Ite                 (* Condional *)
  | Sequent             (* Is the given sequent provable ? *)

  | Arrow | Subtype     (* Function type constructor and subtyping relation *)
  | Product | Union     (* Product and union of types (not set theory) *)

  | Not                 (* Propositional negation *)
  | And | Or            (* Conjunction and disjunction *)
  | Nand | Xor | Nor    (* Advanced propositional connectives *)
  | Imply | Implied     (* Implication and left implication *)
  | Equiv               (* Equivalence *)

type binder =
  | All | Ex | Pi | Let | Fun   (* Standard binders *)
  | Choice                      (* Indefinite description, or epsilon terms *)
  | Description                 (* Definite description *)

type descr =
  | Symbol of string
  | Builtin of builtin
  | Colon of t * t
  | App of t * t list
  | Binder of binder * t list * t

and t = {
  term : descr;
  attr : t list;
  loc : location option;
}

(* Printing info *)

let is_infix = function
  | Eq | And | Or
  | Nand | Xor | Nor
  | Imply | Implied | Equiv
  | Arrow | Product | Union
  | Sequent | Subtype
    -> true
  | _ -> false

(* Debug printing *)

let pp_builtin b = function
  | Wildcard -> Printf.bprintf b "_"
  | True -> Printf.bprintf b "⊤"
  | False -> Printf.bprintf b "⊥"
  | Eq -> Printf.bprintf b "=="
  | Distinct -> Printf.bprintf b "!="
  | Ite -> Printf.bprintf b "#ite"
  | Sequent -> Printf.bprintf b "⊢"
  | Arrow -> Printf.bprintf b "→"
  | Subtype -> Printf.bprintf b "⊂"
  | Product -> Printf.bprintf b "*"
  | Union -> Printf.bprintf b "∪"
  | Not -> Printf.bprintf b "¬"
  | And -> Printf.bprintf b "∧"
  | Or -> Printf.bprintf b "∨"
  | Nand -> Printf.bprintf b "⊼"
  | Xor -> Printf.bprintf b "⊻"
  | Nor -> Printf.bprintf b "V"
  | Imply -> Printf.bprintf b "⇒"
  | Implied -> Printf.bprintf b "⇐"
  | Equiv -> Printf.bprintf b "⇔"

let pp_binder b = function
  | All -> Printf.bprintf b "∀"
  | Ex -> Printf.bprintf b "∃"
  | Pi -> Printf.bprintf b "Π"
  | Let -> Printf.bprintf b "let"
  | Fun -> Printf.bprintf b "λ"
  | Choice -> Printf.bprintf b "ε"
  | Description -> Printf.bprintf b "@"

let rec pp_descr b = function
  | Symbol s -> Printf.bprintf b "%s" s
  | Builtin s -> pp_builtin b s
  | Colon (u, v) -> Printf.bprintf b "%a : %a" pp u pp v
  | App ({ term = Builtin sep}, l) when is_infix sep ->
    Misc.pp_list ~pp_sep:pp_builtin ~sep ~pp b l
  | App (f, l) ->
    Printf.bprintf b "%a(%a)" pp f
      (Misc.pp_list ~pp_sep:Buffer.add_string ~sep:"," ~pp) l
  | Binder (q, l, e) ->
    Printf.bprintf b "%a %a. %a" pp_binder q
      (Misc.pp_list ~pp_sep:Buffer.add_string ~sep:"," ~pp) l pp e

and pp b = function
  | { term = (Symbol _) as d }
  | { term = (Builtin _) as d } -> pp_descr b d
  | e -> Printf.bprintf b "(%a)" pp_descr e.term

(* Pretty-printing *)

let print_builtin fmt = function
  | Wildcard -> Format.fprintf fmt "_"
  | True -> Format.fprintf fmt "⊤"
  | False -> Format.fprintf fmt "⊥"
  | Eq -> Format.fprintf fmt "=="
  | Distinct -> Format.fprintf fmt "!="
  | Ite -> Format.fprintf fmt "#ite"
  | Sequent -> Format.fprintf fmt "⊢"
  | Arrow -> Format.fprintf fmt "→"
  | Subtype -> Format.fprintf fmt "⊂"
  | Product -> Format.fprintf fmt "*"
  | Union -> Format.fprintf fmt "∪"
  | Not -> Format.fprintf fmt "¬"
  | And -> Format.fprintf fmt "∧"
  | Or -> Format.fprintf fmt "∨"
  | Nand -> Format.fprintf fmt "⊼"
  | Xor -> Format.fprintf fmt "⊻"
  | Nor -> Format.fprintf fmt "V"
  | Imply -> Format.fprintf fmt "⇒"
  | Implied -> Format.fprintf fmt "⇐"
  | Equiv -> Format.fprintf fmt "⇔"

let print_binder fmt = function
  | All -> Format.fprintf fmt "∀"
  | Ex -> Format.fprintf fmt "∃"
  | Pi -> Format.fprintf fmt "Π"
  | Let -> Format.fprintf fmt "let"
  | Fun -> Format.fprintf fmt "λ"
  | Choice -> Format.fprintf fmt "ε"
  | Description -> Format.fprintf fmt "@"

let rec print_descr fmt = function
  | Symbol s -> Format.fprintf fmt "%s" s
  | Builtin s -> print_builtin fmt s
  | Colon (u, v) -> Format.fprintf fmt "%a :@ %a" print u print v
  | App ({ term = Builtin sep}, l) when is_infix sep ->
    Misc.print_list ~print_sep:print_builtin ~sep ~print fmt l
  | App (f, []) ->
    Format.fprintf fmt "%a" print f
  | App (f, l) ->
    Format.fprintf fmt "%a@ %a" print f
      (Misc.print_list ~print_sep:Format.fprintf ~sep:"@ " ~print) l
  | Binder (q, l, e) ->
    Format.fprintf fmt "%a@ %a.@ %a" print_binder q
      (Misc.print_list ~print_sep:Format.fprintf ~sep:"@ " ~print) l print e

and print fmt = function
  | { term = (Symbol _) as d }
  | { term = (Builtin _) as d } -> print_descr fmt d
  | e -> Format.fprintf fmt "@[<hov 2>(%a)@]" print_descr e.term


(* Make a term from its description *)
let make ?loc ?(attr=[]) term = { term; attr; loc; }

(* Internal shortcut to make a formula with bound variables. *)
let mk_bind binder ?loc vars t =
  make ?loc (Binder (binder, vars, t))

(* Attach an attribute list to a term *)
let attr ?loc t l =
  { t with attr = t.attr @ l }

(* Create a constant and/or variable, that is a leaf
   of the term AST. *)
let const ?loc s = make ?loc (Symbol s)

(* Apply a term to a list of terms. *)
let apply ?loc f args = make ?loc (App (f, args))

(* Juxtapose two terms, usually a term and its type.
   Used mainly for typed variables, or type annotations. *)
let colon ?loc t t' = make ?loc (Colon (t, t'))


let eq_t        = make (Builtin Eq)
let neq_t       = make (Builtin Distinct)
let not_t       = make (Builtin Not)
let or_t        = make (Builtin Or)
let and_t       = make (Builtin And)
let xor_t       = make (Builtin Xor)
let nor_t       = make (Builtin Nor)
let nand_t      = make (Builtin Nand)
let equiv_t     = make (Builtin Equiv)
let implies_t   = make (Builtin Imply)
let implied_t   = make (Builtin Implied)

let true_       = make (Builtin True)
let false_      = make (Builtin False)
let wildcard    = make (Builtin Wildcard)

let ite_t       = make (Builtin Ite)
let sequent_t   = make (Builtin Sequent)

let arrow_t     = make (Builtin Arrow)
let union_t     = make (Builtin Union)
let product_t   = make (Builtin Product)
let subtype_t   = make (Builtin Subtype)

let tType       = make (Symbol "$tType")
let prop        = make (Symbol "$o")
let data_t      = make (Symbol "$data")


(* {2 Usual functions} *)

let arrow ?loc arg ret = apply ?loc arrow_t [ret; arg]
let fun_ty ?loc args ret = apply ?loc arrow_t (ret :: args)

let eq ?loc a b = apply ?loc eq_t [a; b]


(* {2 Logical connectives} *)

let not_ ?loc t = apply ?loc not_t [t]
let or_ ?loc l  = apply ?loc or_t l
let and_ ?loc l = apply ?loc and_t l
let imply ?loc p q = apply ?loc implies_t [p; q]
let equiv ?loc p q = apply ?loc equiv_t [p; q]


(* {2 Binders} *)

let pi = mk_bind Pi
let letin = mk_bind Let
let exists = mk_bind Ex
let forall = mk_bind All
let lambda = mk_bind Fun
let choice = mk_bind Choice
let description = mk_bind Description


(* {2 Wrappers for dimacs} *)

let atom ?loc i =
  let s = Printf.sprintf "#%d" i in
  if i >= 0 then const ?loc s
  else not_ ?loc (const ?loc (string_of_int (-i)))


(* {2 Wrappers for smtlib} *)

let int = const
let real = const
let hexa = const
let binary = const

let sexpr ?loc l = apply ?loc (make ?loc (Symbol "")) l

(* {2 Wrappers for tptp} *)

let var = const
let rat = const
let distinct = const

let ite ?loc a b c = apply ?loc ite_t [a; b; c]
let sequent ?loc hyps goals =
  let hyps_t = apply ?loc or_t hyps in
  let goals_t = apply ?loc and_t goals in
  apply ?loc sequent_t [hyps_t; goals_t]

let union ?loc a b = apply ?loc union_t [a; b]
let product ?loc a b = apply ?loc product_t [a; b]
let subtype ?loc a b = apply ?loc subtype_t [a; b]

