
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
  (** Make an identifier from a name and namespace.
      Indexed identifiers (which are a list of names),
      are encoded into a single string with the ['\000']
      character as separator (since it cannot appear in any
      symbol from smtlib). *)

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

  val str     : ?loc:location -> string -> t
  (** Quoted strings. According to the smtlib manual, these can be interpreted as
      either string literals (when the String theory is used), or simply constants *)

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

  val par : ?loc:location -> t list -> t -> t
  (** universal quantification by type variables. *)

  val forall  : ?loc:location -> t list -> t -> t
  (** Universal quantification. *)

  val exists  : ?loc:location -> t list -> t -> t
  (** Existencial quantification. *)

  val match_ : ?loc:location -> t -> (t * t) list -> t
  (** Pattern matching. The first term is the term to match,
      and each tuple in the list is a match case, which is a pair
      of a pattern and a match branch. *)

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

  (** (Re)starting and terminating *)

  val reset : ?loc:location -> unit -> t
  (** Full reset of the prover state. *)

  val set_logic : ?loc:location -> string -> t
  (** Set the problem logic. *)

  val set_option : ?loc:location -> term -> t
  (** Set the value of a prover option. *)

  val exit : ?loc:location -> unit -> t
  (** Exit the interactive loop. *)


  (** Modifying the assertion stack *)

  val push : ?loc:location -> int -> t
  (** Push the given number of new level on the stack of assertions. *)

  val pop : ?loc:location -> int -> t
  (** Pop the given number of level on the stack of assertions. *)

  val reset_assertions : ?loc:location -> unit -> t
  (** Reset assumed assertions. *)


  (** Introducing new symbols *)

  val type_decl : ?loc:location -> id -> int -> t
  (** Declares a new type constructor with given arity. *)

  val type_def  : ?loc:location -> id -> id list -> term -> t
  (** Defines an alias for types. [type_def f args body] is such that
      later occurences of [f] applied to a list of arguments [l] should
      be replaced by [body] where the [args] have been substituted by
      their value in [l]. *)

  val datatypes : ?loc:location -> (id * term list * (id * term list) list) list -> t
  (** Inductive type definitions. *)

  val fun_decl  : ?loc:location -> id -> term list -> term list -> term -> t
  (** Declares a new term symbol, and its type. [fun_decl f ty_args args ret]
      declares [f] as a new function symbol which takes arguments of types
      described in [args], and with return type [ret]. *)

  val fun_def   : ?loc:location -> id -> term list -> term list -> term -> term -> t
  (** Defines a new function. [fun_def f ty_args args ret body] is such that
      applications of [f] are equal to [body] (module substitution of the arguments),
      which should be of type [ret]. *)

  val funs_def_rec : ?loc:location -> (id * term list * term list * term * term) list -> t
  (** Declare a list of mutually recursive functions. *)


  (** Asserting and inspecting formulas *)

  val assert_ : ?loc:location -> term -> t
  (** Add a proposition to the current set of assertions. *)

  val get_assertions : ?loc:location -> unit -> t
  (** Return the current set of assertions. *)


  (** Checking for satisfiablity *)

  val check_sat : ?loc:location -> term list -> t
  (** Solve the current set of assertions for satisfiability,
      under the local assumptions specified. *)


  (** Models *)

  val get_model : ?loc:location -> unit -> t
  (** Return the model found. *)

  val get_value : ?loc:location -> term list -> t
  (** Return the value of the given terms in the current model of the solver. *)

  val get_assignment : ?loc:location -> unit -> t
  (** Return the values of asserted propositions which have been labelled using
      the ":named" attribute. *)

  (** Proofs *)

  val get_proof : ?loc:location -> unit -> t
  (** Return the proof of the lastest [check_sat] if it returned unsat, else
      is undefined. *)

  val get_unsat_core : ?loc:location -> unit -> t
  (** Return the unsat core of the latest [check_sat] if it returned unsat,
      else is undefined. *)

  val get_unsat_assumptions : ?loc:location -> unit -> t
  (** Return a list of local assumptions (as givne in {!check_sat},
      that is enough to deduce unsat. *)

  (** Inspecting settings *)

  val get_info : ?loc:location -> string -> t
  (** Get information (see smtlib manual). *)

  val get_option : ?loc:location -> string -> t
  (** Get the value of a prover option. *)

  (** Scripts commands *)

  val echo : ?loc:location -> string -> t
  (** Print back as-is, including the double quotes. *)

  val set_info : ?loc:location -> term -> t
  (** Set information (see smtlib manual). *)

end
(** implementation requirement for smtlib statements. *)

