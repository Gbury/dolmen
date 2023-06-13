
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(** Standard implementation of terms *)

(** {2 Type definitions} *)

type location = Loc.t

type builtin =
  | Wildcard
  (** Wildcard symbol, i.e placeholder for an expression to be inferred, typically
      during type-checking. *)
  | Ttype
  (** Builtin symbol for the type of Types. *)
  | Unit
  (** Unit type. *)
  | Void
  (** Only inhabitant of the unit type. *)
  | Prop
  (** Builtin symbol for the type of propositions. *)
  | Bool
  (** The boolean type. *)
  | True
  (** The [true] propositional constant. *)
  | False
  (** The [false] propositional constant. *)

  | Eq
  (** Should all arguments be pariwise equal ? *)
  | Distinct
  (** Should all arguments be pairwise distinct ? *)

  | Ite
  (** Condional, usually applied to 3 terms (the condition, the then branch and the else branch). *)
  | Sequent
  (** Sequent as term, usually takes two argument (left side, and right side of the sequent),
      which are respectively a conjunction and a disjunction of propositional formulas. *)

  | Int
  (** Builtin integer type. Currently specific to Zipperposition and alt-ergo format;
      other languages might use constants with pre-defined name, such as tptp's "$int". *)
  | Real
  (** Builtin real type. Currently specific to Alt-ergo format; other languages
      might use constants with pre-defined name, such as smtlib's "real". *)
  | Minus
  (** Arithmetic unary minus. *)
  | Add
  (** Arithmetic addition. *)
  | Sub
  (** Arithmetic substraction. *)
  | Mult
  (** Arithmetic multiplication. *)
  | Div
  (** Arithmetic division quotient *)
  | Mod
  (** Arithmetic division modulo *)
  | Int_pow
  (** Integer exponentiation. *)
  | Real_pow
  (** Real exponentiation. *)
  | Lt
  (** Arithmetic "less than" comparison (strict). *)
  | Leq
  (** Arithmetic "lesser or equal" comparison. *)
  | Gt
  (** Arithmetic "greater than" comparison. *)
  | Geq
  (** Arithmetic "greater or equal" comparison. *)

  | Subtype
  (** Subtyping relation *)
  | Product
  (** Product type constructor *)
  | Union
  (** Union type constructor *)

  | Pi
  (** Pi: higher-order encoding of the forall quantifier as a constant. *)
  | Sigma
  (** Sigma: higher-order envoding of the exists quantifier of a constant. *)

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

  | Bitv of int
  (** Bitvector type (with given length) *)
  | Bitv_extract of int * int
  (** Bitvector extraction *)
  | Bitv_concat
  (** Bitvector concatenation *)

  | Array_get
  (** Array getter. *)
  | Array_set
  (** Array setter. *)

  | Adt_check
  (** Algebraic datatype head constructore checker. *)
  | Adt_project
  (** Algebraic datatype projection. *)

  | Record
  (** Record creation *)
  | Record_with
  (** Record "with" creation *)
  | Record_access
  (** Record field access *)

  | Multi_trigger
  (* Multi-triggers *)
  | Maps_to
  (** Mapping; used in Alt-ergo triggers. *)
  | In_interval of bool * bool
  (** Interval check; used in Alt-ergo triggers *)
  | Check
  (** Similar to cut, but does not introduce the proved term into the axioms. *)
  | Cut
  (** Insert a cut of the given term. *)
  | Sexpr
  (** Head symbol for s-exprs in smtlib. *)
(** The type of builtins symbols for terms.
    Some languages have specific syntax for logical connectives
    (tptp's'&&' or '||' for isntance) whereas some (smtlib for instance)
    don't and treat them as constants. *)

type binder =
  | All
  (** Universal quantification.
      Each term in the list of quantified terms should represent
      a variable (optionnally typed using the {!Colon} constructor. *)
  | Ex
  (** Existencial quantification
      Each term in the list of quantified terms should represent
      a variable (optionnally typed using the {!Colon} constructor. *)
  | Pi
  (** Polymorphic type quantification in function type
      Each term in the list of quantified terms should represent
      a variable (optionnally typed using the {!Colon} constructor. *)
  | Arrow
  (** The arrow binder, for function types. Allows for curified types, if wanted. *)
  | Let_seq
  (** Let bindings (either propositional or for terms).
      Term bound by a let can have many forms depending on the language, but usual
      shapes are:
      - an equality (using the builtin {!Eq}) between a variable
        (optionnally typed using the {!Colon} constructor),
        and a term (e.g. in tptp)
      - an equivalence (using the builtin {!Equiv}) between a variable
        (optionnally typed using the {!Colon} constructor),
        and a term/proposition (e.g. in tptp)
      - a variable and a term juxtaposed using the {!Colon} constructor (e.g. in smtlib)
  *)
  | Let_par
  (** Similar to [Let_seq]; except that the list of bindings should be considered all
      bound at the same time/level/scope.
      More precisely, for [Let_seq], the list of bindings is to be understood
      sequentially (i.e. [Let_seq (b1 :: b2 ...)] is semantically the same as
      [Let_seq b1 (Let_seq b2 (..))]. For [Let_par], the list of bindings all
      happen at the same time: the defining expressions of each binding cannot
      refer to other bindings in the same parralel let-binding. *)
  | Fun
  (** Lambda, i.e function abstraction binder.
      Bound terms are the variables bound by the lambda, optionnally typed
      using the {!Colon} constructor. *)
  | Choice
  (** Indefinite description, or epsilon terms.
      Likely to have its usual shape change following tptp's recent changes. *)
  | Description
  (** Definite description.
      Likely to have its usual shape change following tptp's recent changes. *)
(** The type of binders, these are pretty much always builtin in all languages. *)

type descr =
  | Symbol of Id.t
  (** Constants, variables, etc... any string-identified non-builtin atomic term. *)
  | Builtin of builtin
  (** Predefined builtins, i.e constants with lexical or syntaxic defintion
      in the source language. *)
  | Colon of t * t
  (** Juxtaposition of terms, usually used to annotate a term with its type
      (for quantified variables, functions arguments, etc...). *)
  | App of t * t list
  (** Higher-order application *)
  | Binder of binder * t list * t
  (** Binder (quantifiers, local functions, ...), see the {!binder} type for more
      information. *)
  | Match of t * (t * t) list
  (** Pattern matching, the list contains tuples of the form [(pattern,branch)]. *)
(** The AST for terms *)

and t = {
  term : descr;
  attr : t list;
  loc : location;
}
(** The type of terms. A record containing an optional location,
    and a description of the term. *)


(** {2 Standard functions} *)

val equal : t -> t -> bool
val compare : t -> t -> int
(** Equality and comparison *)

val print : Format.formatter -> t -> unit
val print_builtin : Format.formatter -> builtin -> unit
(** Printing functionson buffer and formatters. *)


(** {2 Implemented interfaces} *)

include Dolmen_intf.Term.Logic
  with type t := t
   and type id := Id.t
   and type location := location
(** Include the Logic interface. This interface defines almost all term building
    functions that you may want to use. *)


(** {2 Term constructor not in implemented interfaces} *)

val ite_t : ?loc:location -> unit -> t
(** The standalone term corresponding to the if-then-else builtin
    construction. *)


(** {2 Term inspection} *)

module S : Set.S with type elt = Id.t
(** Sets of Ids *)

val free_ids :
  test:(Id.t -> bool) ->
  S.t -> t -> S.t
(** Returns the set of identifiers that respect the test:predicate function,
    and occurs free in the term (i.e. not bound by a binder). *)

val fv : t -> Id.t list
(** Return the list of free variables (i.e currently, Ids that are in
    the [Var] namespace). *)


(** {2 Additional functions} *)

val builtin : builtin -> ?loc:location -> unit -> t
(** Make a builtin. *)

val fun_ty : ?loc:location -> t list -> t -> t
(** Multi-arguments function type constructor. *)

val add_attr : t -> t -> t
(** [add_attr attr term] rturns a term [t] equal to [term], but with
    [attr] added to the list of attributes. *)

val add_attrs : t list -> t -> t
(** Same as [add_attr] but adds a list of attributes. *)

val set_attrs : t list -> t -> t
(** Set the given list of terms as th attributes of the given term.
    Will fail (with an assertion) if the given term already have some assertion.
    In such cases, use add_attr instead. *)


(** {2 Term mapping}

    The main use of terms mapper is to map fuctions over some terms.
    Traditionally, a mapping will usually only care about a few syntax cases
    and leave all other untouched. In these cases, it is useful to override
    the identity mapper, redefining only the fields needed.
*)

type 'a mapper = {
  symbol    : 'a mapper -> attr:t list -> loc:location -> Id.t -> 'a;
  builtin   : 'a mapper -> attr:t list -> loc:location -> builtin -> 'a;
  colon     : 'a mapper -> attr:t list -> loc:location -> t -> t -> 'a;
  app       : 'a mapper -> attr:t list -> loc:location -> t -> t list -> 'a;
  binder    : 'a mapper -> attr:t list -> loc:location -> binder -> t list -> t -> 'a;
  pmatch    : 'a mapper -> attr:t list -> loc:location -> t -> (t * t) list -> 'a;
}
(** The type of a mapper on terms. *)

val id_mapper : t mapper
(** The identity mapper: maps any term to itself. *)

val unit_mapper : unit mapper
(** The unit mapper, i.e. an iterator. *)

val map : 'a mapper -> t -> 'a
(** Apply a mapper to a term. *)

