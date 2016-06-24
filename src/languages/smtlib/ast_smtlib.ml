
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(** AST requirement for the Smtlib format.
    The smtlib format is widely used among SMT solvers, and is the language
    of the smtlib benchmark library. Terms are expressed as s-expressions,
    and top-level directives include everything needed to use a prover
    in an interactive loop (so it includes directive for getting and setting options,
    getting information about the solver's internal model, etc...) *)

module type Id = sig

  type t
  (** The type of identifiers *)

  type namespace
  (** Namespace for identifiers *)

  val sort : namespace
  val term : namespace
  val attr : namespace
  (** The namespace for sorts (also called typee), terms
      and attributes, respectively. *)

  val mk : namespace -> string -> t
  (** Make an identifier from a name and namespace. *)

end

module type Term = sig

  type t
  (** The type of terms. *)

  type id
  (** The type of identifiers for constants. *)

  type location
  (** The type of locations. *)

  val const   : ?loc:location -> id -> t
  (** Constants, i.e non predefined symbols. This includes both constants
      defined by theories, defined locally in a problem, and also quantified variables. *)

  val int     : ?loc:location -> string -> t
  val real    : ?loc:location -> string -> t
  val hexa    : ?loc:location -> string -> t
  val binary  : ?loc:location -> string -> t
  (** Constants lexically recognised as numbers in different formats. According to the smtlib
      manual, these should not always be interpreted as numbers since their interpretation
      is actually dependent on the theory set by the problem. *)

  val colon   : ?loc:location -> t -> t -> t
  (** Juxtaposition of terms, used to annotate terms with their type. *)

  val apply   : ?loc:location -> t -> t list -> t
  (** Application. *)

  val letin   : ?loc:location -> t list -> t -> t
  (** Local bindings. The bindings are a list of terms built using the [colon] function. *)

  val forall  : ?loc:location -> t list -> t -> t
  (** Universal quantification. *)

  val exists  : ?loc:location -> t list -> t -> t
  (** Existencial quantification. *)

  val sexpr   : ?loc:location -> t list -> t
  (** S-expressions. Used in smtlib's annotations, *)

  val annot   : ?loc:location -> t -> t list -> t
  (** Attach a list of attributes (also called annotations) to a term. As written
      in the smtlib manual, "Term attributes have no logical meaning --
      semantically, [attr t l] is equivalent to [t]" *)

end
(** Implementation requirements for Smtlib terms. *)

module type Statement = sig

  type t
  (** The type of statements. *)

  type id
  (** The type of identifiers. *)

  type term
  (** The type of terms. *)

  type location
  (** The type of locations. *)

  val pop : ?loc:location -> int -> t
  (** Pop the given number of level on the stack of assertions. *)

  val push : ?loc:location -> int -> t
  (** Push the given number of new level on the stack of assertions. *)

  val assert_ : ?loc:location -> term -> t
  (** Add a proposition to the current set of assertions. *)

  val check_sat : ?loc:location -> unit -> t
  (** Solve the current set of assertions for satisfiability. *)

  val set_logic : ?loc:location -> string -> t
  (** Set the problem logic. *)

  val get_info : ?loc:location -> string -> t
  (** Get information (see smtlib manual). *)

  val set_info : ?loc:location -> string * term option -> t
  (** Set information (see smtlib manual). *)

  val get_option : ?loc:location -> string -> t
  (** Get the value of a prover option. *)

  val set_option : ?loc:location -> string * term option -> t
  (** Set the value of a prover option. *)

  val type_decl : ?loc:location -> id -> int -> t
  (** Declares a new type constructor with given arity. *)

  val type_def  : ?loc:location -> id -> id list -> term -> t
  (** Defines an alias for types. [type_def f args body] is such that
      later occurences of [f] applied to a list of arguments [l] should
      be replaced by [body] where the [args] have been substituted by
      their value in [l]. *)

  val fun_decl  : ?loc:location -> id -> term list -> term -> t
  (** Declares a new term symbol, and its type. [fun_decl f args ret]
      declares [f] as a new function symbol which takes arguments of types
      described in [args], and with return type [ret]. *)

  val fun_def   : ?loc:location -> id -> term list -> term -> term -> t
  (** Defines a new function. [fun_def f args ret body] is such that
      applications of [f] are equal to [body] (module substitution of the arguments),
      which should be of type [ret]. *)

  val get_proof : ?loc:location -> unit -> t
  (** Return the proof of the lastest [check_sat] if it returned unsat, else
      is undefined. *)

  val get_unsat_core : ?loc:location -> unit -> t
  (** Return the unsat core of the latest [check_sat] if it returned unsat,
      else is undefined. *)

  val get_value : ?loc:location -> term list -> t
  (** Return the value of the given terms in the current model of the solver. *)

  val get_assignment : ?loc:location -> unit -> t
  (** Return the values of asserted propositions which have been labelled using
      the ":named" attribute. *)

  val get_assertions : ?loc:location -> unit -> t
  (** Return the current set of assertions. *)

  val exit : ?loc:location -> unit -> t
  (** Exit the interactive loop. *)

end
(** implementation requirement for smtlib statements. *)

