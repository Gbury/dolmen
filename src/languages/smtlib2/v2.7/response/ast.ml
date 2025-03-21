
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

  val indexed : namespace -> string -> string list -> t
  (** Create an indexed identifier. *)

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

  val letand   : ?loc:location -> t list -> t -> t
  (** Local parrallel bindings. The bindings are a list of terms built using
      the [colon] function. *)

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

  type defs
  (** Definition for model values *)

  val fun_def   : ?loc:location -> id -> term list -> term list -> term -> term -> defs
  (** Defines a new function. [fun_def f args ret body] is such that
      applications of [f] are equal to [body] (module substitution of the arguments),
      which should be of type [ret]. *)

  val funs_def_rec : ?loc:location -> (id * term list * term list * term * term) list -> defs
  (** Defines a list of mutually recursive functions. *)

  val sat : ?loc:location -> defs list option -> t
  (** Create a `SAT` answer with an (optional) model. *)

  val unsat : ?loc:location -> unit -> t
  (** Create an `UNSAT` answer. *)

  val error : ?loc:location -> string -> t
  (** Create an `ERROR` answer. *)

end
(** implementation requirement for smtlib statements. *)

