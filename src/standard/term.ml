
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

type location = ParseLocation.t

type builtin =
  | Wildcard
  | Ttype | Prop
  | True | False
  | Eq | Distinct       (* Should all args be pairwise distinct or equal ? *)

  | Ite                 (* Condional *)
  | Sequent             (* Is the given sequent provable ? *)

  | Int                 (* Arithmetic type for integers *)
  | Minus               (* arithmetic unary minus *)
  | Add | Sub | Mult    (* arithmetic operators *)
  | Lt | Leq            (* arithmetic comparisons *)
  | Gt | Geq            (* arithmetic comparisons *)

  | Subtype             (* Function type constructor and subtyping relation *)
  | Product | Union     (* Product and union of types (not set theory) *)

  | Not                 (* Propositional negation *)
  | And | Or            (* Conjunction and disjunction *)
  | Nand | Xor | Nor    (* Advanced propositional connectives *)
  | Imply | Implied     (* Implication and left implication *)
  | Equiv               (* Equivalence *)

type binder =
  | All | Ex
  | Pi | Arrow
  | Let | Fun           (* Standard binders *)
  | Choice              (* Indefinite description, or epsilon terms *)
  | Description         (* Definite description *)

type descr =
  | Symbol of Id.t
  | Builtin of builtin
  | Colon of t * t
  | App of t * t list
  | Binder of binder * t list * t
  | Match of t * (t * t) list

and t = {
  term : descr;
  attr : t list;
  loc : location option;
}

(* Printing info *)

let infix_builtin = function
  | Add | Sub
  | Lt | Leq
  | Gt | Geq
  | Eq | And | Or
  | Nand | Xor | Nor
  | Imply | Implied | Equiv
  | Product | Union
  | Sequent | Subtype
    -> true
  | _ -> false

let builtin_to_string = function
  | Wildcard -> "_"
  | Ttype -> "$tType"
  | Prop -> "$o"
  | True -> "⊤"
  | False -> "⊥"
  | Eq -> "=="
  | Distinct -> "!="
  | Ite -> "#ite"
  | Sequent -> "⊢"
  | Int -> "$int"
  | Minus -> "-"
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "×"
  | Lt -> "<"
  | Leq -> "≤"
  | Gt -> ">"
  | Geq -> "≥"
  | Subtype -> "⊂"
  | Product -> "*"
  | Union -> "∪"
  | Not -> "¬"
  | And -> "∧"
  | Or -> "∨"
  | Nand -> "⊼"
  | Xor -> "⊻"
  | Nor -> "V"
  | Imply -> "⇒"
  | Implied -> "⇐"
  | Equiv -> "⇔"

let binder_to_string = function
  | All -> "∀"
  | Ex -> "∃"
  | Pi -> "Π"
  | Arrow -> "→"
  | Let -> "let"
  | Fun -> "λ"
  | Choice -> "ε"
  | Description -> "@"

(* Debug printing *)

let pp_builtin b builtin =
  Printf.bprintf b "%s" (builtin_to_string builtin)

let pp_binder b binder =
  Printf.bprintf b "%s" (binder_to_string binder)

let rec pp_descr b = function
  | Symbol id -> Id.pp b id
  | Builtin s -> pp_builtin b s
  | Colon (u, v) -> Printf.bprintf b "%a : %a" pp u pp v
  | App ({ term = Builtin sep}, l) when infix_builtin sep ->
    Misc.pp_list ~pp_sep:pp_builtin ~sep ~pp b l
  | App (f, l) ->
    Printf.bprintf b "%a(%a)" pp f
      (Misc.pp_list ~pp_sep:Buffer.add_string ~sep:"," ~pp) l
  | Binder (Arrow as q, l, e) ->
    Printf.bprintf b "%a %a %a"
      (Misc.pp_list ~pp_sep:Buffer.add_string ~sep:" → " ~pp) l pp_binder q pp e
  | Binder (q, l, e) ->
    Printf.bprintf b "%a %a. %a" pp_binder q
      (Misc.pp_list ~pp_sep:Buffer.add_string ~sep:"," ~pp) l pp e
  | Match (t, l) ->
    Printf.bprintf b "match %a with %a"
      pp t (Misc.pp_list ~pp_sep:Buffer.add_string ~sep:" | " ~pp:pp_match_case) l

and pp_match_case b (pattern, branch) =
  Printf.bprintf b "%a => %a" pp pattern pp branch

and pp b = function
  | { term = (Symbol _) as d }
  | { term = (Builtin _) as d } -> pp_descr b d
  | e -> Printf.bprintf b "(%a)" pp_descr e.term

(* Pretty-printing *)

let print_builtin fmt builtin =
  Format.fprintf fmt "%s" (builtin_to_string builtin)

let print_binder fmt binder =
  Format.fprintf fmt "%s" (binder_to_string binder)

let rec print_descr fmt = function
  | Symbol id -> Id.print fmt id
  | Builtin s -> print_builtin fmt s
  | Colon (u, v) -> Format.fprintf fmt "%a :@ %a" print u print v
  | App ({ term = Builtin sep}, l) when infix_builtin sep ->
    Misc.print_list ~print_sep:print_builtin ~sep ~print fmt l
  | App (f, []) ->
    Format.fprintf fmt "%a" print f
  | App (f, l) ->
    Format.fprintf fmt "%a@ %a" print f
      (Misc.print_list ~print_sep:Format.fprintf ~sep:"@ " ~print) l
  | Binder (Arrow as q, l, e) ->
    Format.fprintf fmt "%a %a@ %a"
      (Misc.print_list ~print_sep:Format.fprintf ~sep:"→@ " ~print) l
      print_binder q print e
  | Binder (q, l, e) ->
    Format.fprintf fmt "%a@ %a.@ %a" print_binder q
      (Misc.print_list ~print_sep:Format.fprintf ~sep:"@ " ~print) l print e
  | Match (t, l) ->
    Format.fprintf fmt "match %a with %a" print t
      (Misc.print_list ~print_sep:Format.fprintf ~sep:" | " ~print:print_match_case) l

and print_match_case fmt (pattern, branch) =
  Format.fprintf fmt "%a => %a" print pattern print branch

and print fmt = function
  | { term = (Symbol _) as d }
  | { term = (Builtin _) as d } -> print_descr fmt d
  | e -> Format.fprintf fmt "@[<hov 2>(%a)@]" print_descr e.term

(* Comparison *)
let _descr = function
  | Symbol _  -> 1
  | Builtin _ -> 2
  | Colon _   -> 3
  | App _     -> 4
  | Binder _  -> 5
  | Match _   -> 6

let rec compare_list cmp l l' =
  match l, l' with
  | [], [] -> 0
  | _ :: _, [] -> 1
  | [], _ :: _ -> -1
  | t :: r, t' :: r' ->
    begin match cmp t t' with
      | 0 -> compare_list cmp r r'
      | x -> x
    end

let rec compare t t' =
  match t.term, t'.term with
  | Symbol s, Symbol s' -> Id.compare s s'
  | Builtin b, Builtin b' -> Pervasives.compare b b'
  | Colon (t1, t2), Colon (t1', t2') ->
    compare_list compare [t1; t2] [t1'; t2']
  | App (f, l), App(f', l') ->
    compare_list compare (f :: l) (f' :: l')
  | Binder (b, vars, t), Binder (b', vars', t') ->
    begin match Pervasives.compare b b' with
      | 0 ->
        begin match compare_list compare vars vars' with
          | 0 -> compare t t'
          | x -> x
        end
      | x -> x
    end
  | Match (t, l), Match (t', l') ->
    begin match compare t t' with
      | 0 -> compare_list compare_pair l l'
      | x -> x
    end
  | u, v -> (_descr u) - (_descr v)

and compare_pair (a, b) (c, d) =
  match compare a c with
  | 0 -> compare b d
  | x -> x


let equal t t' = compare t t' = 0

(* Add an attribute *)
let add_attr a t = { t with attr = a :: t.attr }

(* Make a term from its description *)
let make ?loc ?(attr=[]) term = { term; attr; loc; }

(* Internal shortcut to make a formula with bound variables. *)
let mk_bind binder ?loc vars t =
  make ?loc (Binder (binder, vars, t))

(* Attach an attribute list to a term *)
let annot ?loc t l =
  { t with attr = t.attr @ l }

(* Create a constant and/or variable, that is a leaf
   of the term AST. *)
let const ?loc id = make ?loc (Symbol id)

(* Apply a term to a list of terms. *)
let apply ?loc f args = make ?loc (App (f, args))

let match_ ?loc t l = make ?loc (Match (t, l))

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

let union_t     = make (Builtin Union)
let product_t   = make (Builtin Product)
let subtype_t   = make (Builtin Subtype)

let tType       = make (Builtin Ttype)
let prop        = make (Builtin Prop)
let data_t      = const Id.(mk Attr "$data")

let ty_int      = make (Builtin Int)
let uminus_t    = make (Builtin Minus)
let add_t       = make (Builtin Add)
let sub_t       = make (Builtin Sub)
let mult_t      = make (Builtin Mult)
let lt_t        = make (Builtin Lt)
let leq_t       = make (Builtin Leq)
let gt_t        = make (Builtin Gt)
let geq_t       = make (Builtin Geq)

let nary t = (fun ?loc l -> apply ?loc t l)
let unary t = (fun ?loc x -> apply ?loc t [x])
let binary t = (fun ?loc x y -> apply ?loc t [x; y])

(* {2 Usual functions} *)

let eq = binary eq_t

(* {2 Logical connectives} *)

let not_  = unary not_t
let or_   = nary or_t
let and_  = nary and_t
let imply = binary implies_t
let equiv = binary equiv_t

(** {2 Arithmetic} *)

let uminus  = unary uminus_t
let add     = binary add_t
let sub     = binary sub_t
let mult    = binary mult_t
let lt      = binary lt_t
let leq     = binary leq_t
let gt      = binary gt_t
let geq     = binary geq_t

(* {2 Binders} *)

let pi = mk_bind Pi
let letin = mk_bind Let
let exists = mk_bind Ex
let forall = mk_bind All
let lambda = mk_bind Fun
let choice = mk_bind Choice
let description = mk_bind Description

let fun_ty = mk_bind Arrow
let arrow ?loc arg ret = fun_ty ?loc [arg] ret

(* {2 Free variables} *)

module S = Set.Make(Id)

let rec free_vars acc t =
  match t.term with
  | Builtin _ -> acc
  | Colon (t, t') -> free_vars (free_vars acc t) t'
  | Symbol i -> if i.Id.ns = Id.Var then S.add i acc else acc
  | App (t, l) ->
    List.fold_left free_vars (free_vars acc t) l
  | Binder (_, l, t) ->
    let s = free_vars S.empty t in
    let bound = List.fold_left free_vars S.empty l in
    let s' = S.filter (fun x -> not (S.mem x bound)) s in
    S.union acc s'
  | Match (t, l) ->
    let acc = List.fold_left (fun acc (pattern, branch) ->
        let s = free_vars S.empty branch in
        let bound = free_vars S.empty pattern in
        let s' = S.filter (fun x -> not (S.mem x bound)) s in
        S.union s' acc
      ) acc l in
    free_vars acc t

let fv t =
  S.elements (free_vars S.empty t)

(* {2 Wrappers for dimacs} *)

let atom ?loc i =
  let s = Printf.sprintf "#%d" (abs i) in
  if i >= 0 then const ?loc Id.(mk Term s)
  else not_ ?loc (const ?loc Id.(mk Term s))


(* {2 Wrappers for smtlib} *)

let int ?loc s = const ?loc Id.(mk Term s)
let real = int
let hexa = int
let binary = int

let sexpr ?loc l = apply ?loc data_t l

(* {2 Wrappers for tptp} *)

let rat = int
let distinct = const

let var ?loc id = const ?loc { id with Id.ns = Id.Var }

let ite ?loc a b c = apply ?loc ite_t [a; b; c]
let sequent ?loc hyps goals =
  let hyps_t = apply ?loc or_t hyps in
  let goals_t = apply ?loc and_t goals in
  apply ?loc sequent_t [hyps_t; goals_t]

let union ?loc a b = apply ?loc union_t [a; b]
let product ?loc a b = apply ?loc product_t [a; b]
let subtype ?loc a b = apply ?loc subtype_t [a; b]

(* {2 Wrappers for Zf} *)

let quoted ?loc name =
  const ?loc Id.({ name; ns = Attr})

(* {2 Standard Attributes} *)
let rwrt_rule =
  const @@ Id.mk Id.Decl "rewrite_rule"

