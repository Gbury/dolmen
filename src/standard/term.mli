
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(** Standard implementation of terms *)

(** {2 Type definitions} *)

type location = ParseLocation.t

type builtin =
  | Wildcard
  (** Wildcard symbol, i.e placeholder for an expression to be inferred, typically
      during type-checking. *)
  | Ttype
    (** Builtin symbol for the type of Types. *)
  | Prop
    (** Builtin symbol for the type of propositions. *)
  | True
  (** The [true] propositional constant. *)
  | False
  (** The [false] propositional constant. *)

  | Eq
  (** Should all arguments be pariwise equal ? *)
  | Distinct
  (** Should all arguments be pairwise distinct ? *)

  | AC
  (** Attributes to denote symbols that are associative and commutative. *)

  | Ite
  (** Condional, usually applied to 3 terms (the condition, the then branch and the else branch). *)
  | Sequent
  (** Sequent as term, usually takes two argument (left side, and right side of the sequent),
      which are respectively a conjunction and a disjunction of propositional formulas. *)

  | Subtype
  (** Subtyping relation *)
  | Product
  (** Product type constructor *)
  | Union
  (** Union type constructor *)

  | Not
  (** Propositional negation *)
  | And
  (** Propositional conjunction *)
  | Or
  (** Propositional disjunction *)
  | Nand
  (** Propositional not-and connective *)
  | Xor
  (** Propositional exclusive disjunction *)
  | Nor
  (** Propositional not-or *)
  | Imply
  (** Propositional implication *)
  | Implied
  (** Propositional left implication (i.e implication with reversed arguments). *)
  | Equiv
  (** Propositional equivalence *)
(** The type of builtins symbols for terms.
    Some languages have specific syntax for logical connectives
    (tptp's'&&' or '||' for isntance) whereas some (smtlib for instance)
    don't and treat them as constants. *)

type binder =
  | All
  (** Universal quantification *)
  | Ex
  (** Existencial quantification *)
  | Pi
  (** Polymorphic type quantification in function type *)
  | Arrow
  (** The arrow binder, for function types *)
  | Let
  (** Let bindings (either propositional or for terms *)
  | Fun
  (* Lambda, i.e function abstraction binder *)
  | Choice
  (* Indefinite description, or epsilon terms *)
  | Description
  (* Definite description *)
(** The type of binders, these are pretty much always builtin in all languages. *)

type descr =
  | Symbol of Id.t
  (** Constants, variables, etc... any string-identified non-builtin atomic term. *)
  | Builtin of builtin
  (** Predefined builtins, i.e constants with lexical or syntaxic defintion in the source language. *)
  | Colon of t * t
  (** Juxtaposition of terms, usually used to annotate a term with its type (for quantified variables,
      functions arguments, etc... *)
  | App of t * t list
  (** Higher-order application *)
  | Binder of binder * t list * t
  (** Binder (quantifiers, local functions, ...) *)
(** The AST for terms *)

and t = {
  term : descr;
  attr : t list;
  loc : location option;
}
(** The type of terms. A record containing an optional location,
    and a description of the term. *)

(** {2 Implemented interfaces} *)

include Term_intf.Logic
  with type t := t
   and type id := Id.t
   and type location := location

(** {2 Additional functions} *)

val fun_ty : ?loc:location -> t list -> t -> t
(** Multi-arguments function type constructor. *)

val pp : Buffer.t -> t -> unit
val print : Format.formatter -> t -> unit
(** Printing functionson buffer and formatters. *)



