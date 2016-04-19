
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(** Terms *)

(** The type of builtins symbols for terms.
    Some languages have specific syntax for logical connectives
    (tptp's'&&' or '||' for isntance) whereas some don't
    (in smtlib for instance) *)
type builtin =
  | Wildcard
  | True | False
  | Eq | Distinct
  | Ite | Arrow
  | And | Or | Xor
  | Imply | Equiv | Not

(** The type of binders, these are prettymuch always builtin in all languages. *)
type binder =
  | All | Ex | Let

(** The AST for terms *)
type descr =
  | Symbol of string
  | Builtin of builtin
  | Column of t * t
  | App of t * t list
  | Binder of binder * t list * t

(** The type of terms. A record wontaining an optional location,
    and a description of the term. *)
and t = {
  loc : ParseLocation.t option;
  term : descr;
}

(** {2 Internal constructors} *)

(** Make a term from its description *)
let make ?loc term = { loc; term; }

(** Internal shortcut to make a formula with bound variables. *)
let mk_bind binder ?loc vars t =
  make ?loc (Binder (binder, vars, t))

(** {2 Symbols as expressions} *)

let eq_sym = make (Builtin Eq)
let neq_sym = make (Builtin Distinct)

let neg_sym = make (Builtin Not)
let or_sym = make (Builtin Or)
let and_sym = make (Builtin And)
let imply_sym = make (Builtin Imply)
let equiv_sym = make (Builtin Equiv)
let arrow_sym = make (Builtin Arrow)

let true_ = make (Builtin True)
let false_ = make (Builtin False)
let wildcard = make (Builtin Wildcard)

let tType = make (Symbol "$tType")
let prop = make (Symbol "$o")

(** {2 Base Constructors} *)

(** Add (or replace) the location of an existing term. *)
let at_loc ~loc t = { t with loc = Some loc; }

(** Create a constant and/or variable, that is a leaf
    of the term AST. *)
let const ?loc s = make ?loc (Symbol s)

(** Apply a term to a list of terms. *)
let app ?loc f args = make ?loc (App (f, args))

(** Juxtapose two terms, usually a term and its type.
    Used mainly for typed variables, or type annotations. *)
let column ?loc t t' = make ?loc (Column (t, t'))

(** Function type, represented as App(arrow, ret :: args). *)
let fun_ty ?loc args ret = app ?loc arrow_sym (ret :: args)

(** Equality of terms *)
let eq ?loc a b = app ?loc eq_sym [a; b]


(** {2 Logical connectives} *)

let not_ ?loc t = app ?loc neg_sym [t]
let or_ ?loc l = app ?loc or_sym l
let and_ ?loc l = app ?loc and_sym l
let imply ?loc p q = app ?loc imply_sym [p; q]
let equiv ?loc p q = app ?loc equiv_sym [p; q]


(** {2 Binders} *)

let letin = mk_bind Let
let exists = mk_bind Ex
let forall = mk_bind All


(** {2 Wrappers for dimacs} *)

let atom ?loc s =
  let i = int_of_string s in
  if i >= 0 then const ?loc s
  else not_ ?loc (const ?loc (string_of_int (-i)))


(** {2 Wrappers for smtlib} *)

let int = const
let real = const
let hexa = const
let binary = const

let typed = column

let attr ?loc t _ = t
let sexpr ?loc l = app ?loc (make ?loc (Symbol "")) l


(** {2 Wrappers for Zf} *)

let forall_ty = forall

