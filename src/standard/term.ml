
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

type location = Loc.t

type builtin =
  | Wildcard
  | Ttype
  | Unit | Void
  | Prop | Bool
  | True | False
  | Eq | Distinct       (* Should all args be pairwise distinct or equal ? *)

  | Ite                 (* Condional *)
  | Sequent             (* Is the given sequent provable ? *)

  | Int                 (* Arithmetic type for integers *)
  | Real                (* Arithmetic type for reals *)
  | Minus               (* arithmetic unary minus *)
  | Add | Sub | Mult    (* arithmetic operators *)
  | Div | Mod           (* arithmetic division *)
  | Int_pow | Real_pow  (* arithmetic power (it's over 9000!) *)
  | Lt | Leq            (* arithmetic comparisons *)
  | Gt | Geq            (* arithmetic comparisons *)

  | Subtype             (* Function type constructor and subtyping relation *)
  | Product | Union     (* Product and union of types (not set theory) *)

  | Pi | Sigma          (* Higher-order constant to encode forall and exists quantifiers *)

  | Not                 (* Propositional negation *)
  | And | Or            (* Conjunction and disjunction *)
  | Nand | Xor | Nor    (* Advanced propositional connectives *)
  | Imply | Implied     (* Implication and left implication *)
  | Equiv               (* Equivalence *)

  | Bitv of int         (* Bitvector type (with given length) *)
  | Bitv_extract of int * int (* Bitvector extraction *)
  | Bitv_concat         (* Bitvector concatenation *)

  | Array_get
  | Array_set           (* array operations *)

  | Adt_check
  | Adt_project         (* adt operations *)

  | Record
  | Record_with
  | Record_access       (* record operations *)

  | Multi_trigger       (* multi-triggers *)
  | Maps_to
  | In_interval of bool * bool
  | Check | Cut         (* alt-ergo builtins *)

  | Sexpr               (* smtlib builtin for s-exprs *)

type binder =
  | All | Ex
  | Pi | Arrow
  | Let_seq             (* sequential let-binding *)
  | Let_par             (* parrallel let-binding *)
  | Fun                 (* function parameter binding *)
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
  loc : location;
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
  | Adt_check | Adt_project | Record_access
    -> true
  | Distinct -> true
  | _ -> false

let builtin_to_string = function
  | Wildcard -> "_"
  | Ttype -> "Ttype"
  | Unit -> "unit"
  | Void -> "()"
  | Prop -> "Prop"
  | Bool -> "Bool"
  | True -> "⊤"
  | False -> "⊥"
  | Eq -> "=="
  | Distinct -> "!="
  | Ite -> "ite"
  | Sequent -> "⊢"
  | Int -> "ℤ"
  | Real -> "ℝ"
  | Minus -> "-"
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "×"
  | Div -> "÷"
  | Mod -> "mod"
  | Int_pow -> "**"
  | Real_pow -> "**."
  | Lt -> "<"
  | Leq -> "≤"
  | Gt -> ">"
  | Geq -> "≥"
  | Subtype -> "⊂"
  | Product -> "*"
  | Union -> "∪"
  | Pi -> "Π"
  | Sigma -> "Σ"
  | Not -> "¬"
  | And -> "∧"
  | Or -> "∨"
  | Nand -> "⊼"
  | Xor -> "⊻"
  | Nor -> "V"
  | Imply -> "⇒"
  | Implied -> "⇐"
  | Equiv -> "⇔"
  | Bitv i -> Printf.sprintf "bitv(%d)" i
  | Bitv_extract (i, j) -> Printf.sprintf "bitv_extract(%d;%d)" i j
  | Bitv_concat -> "bitv_concat"
  | Array_get -> "get"
  | Array_set -> "set"
  | Adt_check -> "?"
  | Adt_project -> "#"
  | Record -> "record"
  | Record_with -> "record_with"
  | Record_access -> "."
  | Multi_trigger -> "multi"
  | Maps_to -> "↦"
  | In_interval (b, b') ->
    let bracket = function true -> "]" | false -> "[" in
    Printf.sprintf "in_interval%s%s" (bracket b) (bracket (not b'))
  | Check -> "check"
  | Cut -> "cut"
  | Sexpr -> "sexpr"

let binder_to_string = function
  | All -> "∀"
  | Ex -> "∃"
  | Pi -> "Π"
  | Arrow -> ""
  | Let_seq
  | Let_par -> "let"
  | Fun -> "λ"
  | Choice -> "ε"
  | Description -> "@"

let binder_sep_string = function
  | All
  | Ex
  | Pi
  | Let_seq
  | Fun
  | Choice
  | Description -> ""
  | Let_par -> " and"
  | Arrow -> " →"

let binder_end_string = function
  | All
  | Ex
  | Pi
  | Choice
  | Description -> "."
  | Fun -> "=>"
  | Let_seq
  | Let_par -> "in"
  | Arrow -> "→"

(*
(* Debug printing *)

let pp_builtin b builtin =
  Printf.bprintf b "%s" (builtin_to_string builtin)

let pp_binder b binder =
  Printf.bprintf b "%s" (binder_to_string binder)

let rec pp_descr b = function
  | Symbol id -> Id.pp b id
  | Builtin s -> pp_builtin b s
  | Colon (u, v) -> Printf.bprintf b "%a : %a" pp u pp v
  | App ({ term = Builtin sep ; _ }, l) when infix_builtin sep ->
    Misc.pp_list ~pp_sep:pp_builtin ~sep ~pp b l
  | App (f, l) ->
    Printf.bprintf b "%a(%a)" pp f
      (Misc.pp_list ~pp_sep:Buffer.add_string ~sep:"," ~pp) l
  | Binder (q, l, e) ->
    let sep = binder_sep_string q ^ " " in
    Printf.bprintf b "%a %a %s %a" pp_binder q
      (Misc.pp_list ~pp_sep:Buffer.add_string ~sep ~pp) l
      (binder_end_string q) pp e
  | Match (t, l) ->
    Printf.bprintf b "match %a with %a"
      pp t (Misc.pp_list ~pp_sep:Buffer.add_string ~sep:" | " ~pp:pp_match_case) l

and pp_match_case b (pattern, branch) =
  Printf.bprintf b "%a => %a" pp pattern pp branch

and pp b = function
  | { term = (Symbol _) as d; _ }
  | { term = (Builtin _) as d; _ } -> pp_descr b d
  | e -> Printf.bprintf b "(%a)" pp_descr e.term
*)

(* Pretty-printing *)

let print_locs = false

let print_loc fmt loc =
  if print_locs then Format.fprintf fmt "[%a]" Loc.print_compact loc

let print_builtin fmt builtin =
  Format.fprintf fmt "%s" (builtin_to_string builtin)

let print_binder fmt binder =
  Format.fprintf fmt "%s" (binder_to_string binder)

let rec print_descr fmt = function
  | Symbol id -> Id.print fmt id
  | Builtin s -> print_builtin fmt s
  | Colon (u, v) -> Format.fprintf fmt "%a :@ %a" print u print v
  | App ({ term = Builtin b ; _ }, l) when infix_builtin b ->
    let pp_sep fmt () = Format.fprintf fmt " %a@ " print_builtin b in
    Format.pp_print_list ~pp_sep print fmt l
  | App (f, []) ->
    Format.fprintf fmt "%a" print f
  | App (f, l) ->
    let pp_sep = Format.pp_print_space in
    Format.fprintf fmt "%a@ %a" print f (Format.pp_print_list ~pp_sep print) l
  | Binder (q, l, e) ->
    let pp_sep fmt () = Format.fprintf fmt "%s@ " (binder_sep_string q) in
    Format.fprintf fmt "%a@ %a@ %s@ %a" print_binder q
      (Format.pp_print_list ~pp_sep print) l (binder_end_string q) print e
  | Match (t, l) ->
    let pp_sep fmt () = Format.fprintf fmt " | " in
    Format.fprintf fmt "match %a with %a" print t
      (Format.pp_print_list ~pp_sep print_match_case) l

and print_match_case fmt (pattern, branch) =
  Format.fprintf fmt "%a => %a" print pattern print branch

and print_attr fmt = function
  | [] -> ()
  | a :: r ->
    Format.fprintf fmt "{%a}@,%a" print a print_attr r

and print fmt = function
  | { term = (Symbol _) as d ; attr; loc; }
  | { term = (Builtin _) as d ; attr; loc; } ->
    Format.fprintf fmt "@[<hov 2>%a@,%a@,%a@]"
      print_descr d
      print_loc loc
      print_attr attr
  | e ->
    Format.fprintf fmt "@[<hov 2>(%a)@,%a@,%a@]"
      print_descr e.term
      print_loc e.loc
      print_attr e.attr

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
  | Builtin b, Builtin b' -> Stdlib.compare b b'
  | Colon (t1, t2), Colon (t1', t2') ->
    compare_list compare [t1; t2] [t1'; t2']
  | App (f, l), App(f', l') ->
    compare_list compare (f :: l) (f' :: l')
  | Binder (b, vars, t), Binder (b', vars', t') ->
    begin match Stdlib.compare b b' with
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
let[@inline always] add_attr a t = { t with attr = a :: t.attr }

let[@inline always] add_attrs l t = { t with attr = l @ t.attr }

let[@inline always] set_attrs l t =
  assert (t.attr = []);
  { t with attr = l }

(* Make a term from its description *)
let[@inline always] make ?(loc=Loc.no_loc) ?(attr=[]) term = { term; attr; loc; }

let[@inline always] builtin b ?loc () = make ?loc (Builtin b)

(* Internal shortcut to make a formula with bound variables. *)
let[@inline always] mk_bind binder ?loc vars t =
  make ?loc (Binder (binder, vars, t))

(* Attach an attribute list to a term *)
let annot ?(loc=Loc.no_loc) t l =
  { t with attr = t.attr @ l; loc }

(* Create a constant and/or variable, that is a leaf
   of the term AST. *)
let[@inline always] const ?loc id = make ?loc (Symbol id)

(* Apply a term to a list of terms. *)
let[@inline always] apply ?loc f args = make ?loc (App (f, args))

let[@inline always] match_ ?loc t l = make ?loc (Match (t, l))

(* Juxtapose two terms, usually a term and its type.
   Used mainly for typed variables, or type annotations. *)
let[@inline always] colon ?loc t t' = make ?loc (Colon (t, t'))

let eq_t        = builtin Eq
let neq_t       = builtin Distinct
let not_t       = builtin Not
let or_t        = builtin Or
let and_t       = builtin And
let xor_t       = builtin Xor
let nor_t       = builtin Nor
let nand_t      = builtin Nand
let equiv_t     = builtin Equiv
let implies_t   = builtin Imply
let implied_t   = builtin Implied

let pi_t        = builtin Pi
let sigma_t     = builtin Sigma

let void        = builtin Void
let true_       = builtin True
let false_      = builtin False
let wildcard    = builtin Wildcard

let ite_t       = builtin Ite
let sequent_t   = builtin Sequent

let union_t     = builtin Union
let product_t   = builtin Product
let subtype_t   = builtin Subtype

let tType       = builtin Ttype
let prop        = builtin Prop
let bool        = builtin Bool
let data_t ?loc () = const ?loc Id.(mk Attr "$data")

let ty_unit     = builtin Unit
let ty_int      = builtin Int
let ty_real     = builtin Real
let uminus_t    = builtin Minus
let add_t       = builtin Add
let sub_t       = builtin Sub
let mult_t      = builtin Mult
let div_t       = builtin Div
let mod_t       = builtin Mod
let int_pow_t   = builtin Int_pow
let real_pow_t  = builtin Real_pow
let lt_t        = builtin Lt
let leq_t       = builtin Leq
let gt_t        = builtin Gt
let geq_t       = builtin Geq

let array_get_t = builtin Array_get
let array_set_t = builtin Array_set

let bitv_t i            = builtin (Bitv i)
let bitv_concat_t       = builtin Bitv_concat
let bitv_extract_t i j  = builtin (Bitv_extract (i, j))

let adt_check_t         = builtin Adt_check
let adt_project_t       = builtin Adt_project

let record_t            = builtin Record
let record_with_t       = builtin Record_with
let record_access_t     = builtin Record_access

let sexpr_t             = builtin Sexpr

let nary t = (fun ?loc l -> apply ?loc (t ?loc ()) l)
let unary t = (fun ?loc x -> apply ?loc (t ?loc ()) [x])
let binary t = (fun ?loc x y -> apply ?loc (t ?loc ()) [x; y])
let tertiary t = (fun ?loc x y z -> apply ?loc (t ?loc ()) [x; y; z])

(* {2 Usual functions} *)

let eq = binary eq_t
let neq = nary neq_t

(* {2 Logical connectives} *)

let not_  = unary not_t
let or_   = nary or_t
let and_  = nary and_t
let xor   = binary xor_t
let imply = binary implies_t
let equiv = binary equiv_t

(** {2 Arithmetic} *)

let uminus  = unary uminus_t
let add     = binary add_t
let sub     = binary sub_t
let mult    = binary mult_t
let div     = binary div_t
let mod_    = binary mod_t
let int_pow = binary int_pow_t
let real_pow = binary real_pow_t
let lt      = binary lt_t
let leq     = binary leq_t
let gt      = binary gt_t
let geq     = binary geq_t

(* {2 Arrays} *)

let array_get = binary array_get_t
let array_set = tertiary array_set_t

(* {2 Bitvectors} *)

let ty_bitv ?loc i =
  apply ?loc (bitv_t i ?loc ()) []

let bitv_extract ?loc t i j =
  apply ?loc (bitv_extract_t i j ?loc ()) [t]

let bitv_concat = binary bitv_concat_t

(* {2 ADTs} *)

let adt_check ?loc t id =
  let x = const ?loc id in
  apply ?loc (adt_check_t ?loc ()) [t; x]

let adt_project ?loc t id =
  let x = const ?loc id in
  apply ?loc (adt_project_t ?loc ()) [t; x]

(** {2 Records} *)

let record = nary record_t

let record_with ?loc t l =
  nary record_with_t ?loc (t :: l)

let record_access ?loc t id =
  let x = const ?loc id in
  binary record_access_t ?loc t x


(* {2 Binders} *)

let pi = mk_bind Pi
let letin = mk_bind Let_seq
let letand = mk_bind Let_par
let exists = mk_bind Ex
let forall = mk_bind All
let lambda = mk_bind Fun
let choice = mk_bind Choice
let description = mk_bind Description

let fun_ty = mk_bind Arrow
let arrow ?loc arg ret = fun_ty ?loc [arg] ret

(* {2 Free variables} *)

module S = Set.Make(Id)

let rec free_ids ~test acc t =
  match t.term with
  | Builtin _ -> acc
  | Colon (t, t') -> free_ids ~test (free_ids ~test acc t) t'
  | Symbol id -> if test id then S.add id acc else acc
  | App (t, l) ->
    List.fold_left (free_ids ~test) (free_ids ~test acc t) l
  | Binder (Arrow, l, t) ->
    List.fold_left (free_ids ~test) (free_ids ~test acc t) l
  | Binder (_, l, t) ->
    let s = free_ids ~test S.empty t in
    let bound, free =
      List.fold_left (free_ids_bound ~test) (S.empty, S.empty) l
    in
    let s' = S.filter (fun x -> not (S.mem x bound)) s in
    S.union acc (S.union s' free)
  | Match (t, l) ->
    let acc = List.fold_left (fun acc (pattern, branch) ->
        let s = free_ids ~test S.empty branch in
        let bound = free_ids ~test S.empty pattern in
        let s' = S.filter (fun x -> not (S.mem x bound)) s in
        S.union s' acc
      ) acc l in
    free_ids ~test acc t

and free_ids_bound ~test (bound, free) t =
  match t.term with
  | Colon (v, ty) -> free_ids ~test bound v, free_ids ~test free ty
  | _ -> free_ids ~test bound t, free

let fv t =
  let test id = id.Id.ns = Namespace.Var in
  S.elements (free_ids ~test S.empty t)


(* {2 Wrappers for alt-ergo} *)

let bitv ?loc s = const ?loc Id.(mk (Value Bitvector) s)

let cut = unary (builtin Cut)
let check = unary (builtin Check)

let trigger ?loc l =
  apply ?loc (builtin ?loc Multi_trigger ()) l

let triggers_t = Id.(mk Attr "triggers")
let triggers ?loc t = function
  | [] -> t
  | l ->
    let a = apply ?loc (const ?loc triggers_t) l in
    annot ?loc t [a]

let filters_t = Id.(mk Attr "filters")
let filters ?loc t = function
  | [] -> t
  | l ->
    let a = apply ?loc (const ?loc filters_t) l in
    annot ?loc t [a]

let tracked ?loc id t =
  let a = const ?loc id in
  annot ?loc t [a]

let maps_to ?loc id t =
  let a = const ?loc id in
  binary (builtin Maps_to) ?loc a t

let in_interval ?loc t (lb, ls) (rb, rs) =
  tertiary (builtin (In_interval (ls, rs))) ?loc t lb rb

(* {2 Wrappers for dimacs} *)

let atom ?loc i =
  let s = Printf.sprintf "#%d" (abs i) in
  if i >= 0 then const ?loc Id.(mk Term s)
  else not_ ?loc (const ?loc Id.(mk Term s))


(* {2 Wrappers for smtlib} *)

let str ?loc s = const ?loc Id.(mk (Value String) s)
let int ?loc s = const ?loc Id.(mk (Value Integer) s)
let real ?loc s = const ?loc Id.(mk (Value Real) s)
let hexa ?loc s = const ?loc Id.(mk (Value Hexadecimal) s)
let binary ?loc s = const ?loc Id.(mk (Value Binary) s)

let sexpr ?loc l = apply ?loc (sexpr_t ?loc ()) l

let par ?loc vars t =
  let vars = List.map (fun v -> colon ?loc v (tType ?loc ())) vars in
  forall ?loc vars t


(* {2 Wrappers for tptp} *)

let rat ?loc s = const ?loc Id.(mk (Value Rational) s)
let distinct = const

let var ?loc id = const ?loc { id with Id.ns = Var }

let ite ?loc a b c = apply ?loc (ite_t ?loc ()) [a; b; c]

let sequent ?loc hyps goals =
  let hyps_t = apply ?loc (or_t ?loc ()) hyps in
  let goals_t = apply ?loc (and_t ?loc ()) goals in
  apply ?loc (sequent_t ?loc ()) [hyps_t; goals_t]

let union ?loc a b = apply ?loc (union_t ?loc ()) [a; b]
let product ?loc a b = apply ?loc (product_t ?loc ()) [a; b]
let subtype ?loc a b = apply ?loc (subtype_t ?loc ()) [a; b]

(* {2 Wrappers for Zf} *)

let quoted ?loc name =
  const ?loc (Id.mk Attr name)

(* {2 Term traversal} *)

type 'a mapper = {
  symbol    : 'a mapper -> attr:t list -> loc:location -> Id.t -> 'a;
  builtin   : 'a mapper -> attr:t list -> loc:location -> builtin -> 'a;
  colon     : 'a mapper -> attr:t list -> loc:location -> t -> t -> 'a;
  app       : 'a mapper -> attr:t list -> loc:location -> t -> t list -> 'a;
  binder    : 'a mapper -> attr:t list -> loc:location -> binder -> t list -> t -> 'a;
  pmatch    : 'a mapper -> attr:t list -> loc:location -> t -> (t * t) list -> 'a;
}

let map mapper t =
  let wrap f = f mapper ~attr:t.attr ~loc:t.loc in
  match t.term with
  | Symbol id -> wrap mapper.symbol id
  | Builtin b -> wrap mapper.builtin b
  | Colon (u, v) -> wrap mapper.colon u v
  | App (f, args) -> wrap mapper.app f args
  | Binder (b, vars, body) -> wrap mapper.binder b vars body
  | Match (e, l) -> wrap mapper.pmatch e l

let id_mapper = {
  symbol = (fun m ~attr ~loc id ->
      set_attrs (List.map (map m) attr) @@ const ~loc id);
  builtin = (fun m ~attr ~loc b ->
      set_attrs (List.map (map m) attr) @@ builtin ~loc b ());
  colon = (fun m ~attr ~loc u v ->
      set_attrs (List.map (map m) attr) @@ colon ~loc (map m u) (map m v));
  app = (fun m ~attr ~loc f args ->
      set_attrs (List.map (map m) attr) @@ apply ~loc (map m f) (List.map (map m) args));
  binder = (fun m ~attr ~loc b vars body ->
      set_attrs (List.map (map m) attr) @@ mk_bind ~loc b (List.map (map m) vars) (map m body));
  pmatch = (fun m ~attr ~loc e l ->
      set_attrs (List.map (map m) attr) @@ match_ ~loc (map m e)
        (List.map (fun (pat, body) -> (map m pat, map m body)) l));
}

let unit_mapper = {
  symbol = (fun m ~attr ~loc:_ _ -> List.iter (map m) attr);
  builtin = (fun m ~attr ~loc:_ _ -> List.iter (map m) attr);
  colon = (fun m ~attr ~loc:_ u v ->
      List.iter (map m) attr; map m u; map m v);
  app = (fun m ~attr ~loc:_ f args ->
      List.iter (map m) attr; map m f; List.iter (map m) args);
  binder = (fun m ~attr ~loc:_ _ vars body ->
      List.iter (map m) attr; List.iter (map m) vars; map m body);
  pmatch = (fun m ~attr ~loc:_ e l ->
      List.iter (map m) attr; map m e;
      List.iter (fun (pat, body) -> map m pat; map m body) l);
}

