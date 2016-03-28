
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

type builtin =
  | Wildcard
  | True | False
  | Eq | Distinct
  | Ite | Arrow
  | And | Or | Xor
  | Imply | Equiv | Not

type binder =
  | All | Ex | Let

type descr =
  | Symbol of string
  | Builtin of builtin
  | Column of t * t
  | App of t * t list
  | Binder of binder * t list * t

and t = {
  loc : ParseLocation.t option;
  term : descr;
}


(* Internal constructors *)
let make ?loc term = { loc; term; }

let mk_bind binder ?loc vars t =
  make ?loc (Binder (binder, vars, t))


(* Add location after building a term *)
let at_loc ~loc t = { t with loc = Some loc; }


(* Constructors *)
let const ?loc s = make ?loc (Symbol s)
let app ?loc f args = make ?loc (App (f, args))
let column ?loc t t' = make ?loc (Column (t, t'))

let neg_sym = make (Builtin Not)
let or_sym = make (Builtin Or)
let and_sym = make (Builtin And)
let imply_sym = make (Builtin Imply)

let not_ ?loc t = app ?loc neg_sym [t]
let or_ ?loc l = app ?loc or_sym l
let and_ ?loc l = app ?loc and_sym l

let letin = mk_bind Let
let exists = mk_bind Ex
let forall = mk_bind All


(* Wrappers for dimacs *)
let atom ?loc s =
  let i = int_of_string s in
  if i >= 0 then const ?loc s
  else not_ ?loc (const ?loc (string_of_int (-i)))


(* Wrappers for smtlib *)
let int = const
let real = const
let hexa = const
let binary = const

let typed = column

let attr ?loc t _ = t
let sexpr ?loc l = app ?loc (make ?loc (Symbol "")) l


