
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** Interfaces for Tags.
    Tags are used to attach arbitrary information to ndoes in an ast. *)

(** Base signature for tags. *)
module type S = sig

  type 'a t
  (** Polymorphic tags *)

  val create :
    ?print:('a Pretty.print) ->
    unit -> 'a t
  (** Create a new tag. *)

end

(** Minium required signature for tags to typecheck Alt-Ergo's core/base theory. *)
module type Ae_Base = sig

  type term
  (** The type of terms *)

  type 'a t
  (** Polymorphic tags *)

  val predicate : unit t
  (** A flag (i.e. unit tag), indicating that the declaration is a predicate *)

  val ac : unit t
  (** A flag (i.e. unit tag), indicating that the tagged term/formula
      is to be considered as a associative and commutative term. *)

  val named : string t
  (** A tag used to name formulas in alt-ergo. *)

  val triggers : term list t
  (** Multi-triggers that can be added to quantified formulas.
      Each term is a multi-trigger represented as a conjunction of triggers. *)

  val filters : term list t
  (** Filters that can be added to quantified formulas *)

end

(** Minium required signature for tags to typecheck smtlib's core/base theory. *)
module type Smtlib_Base = sig

    type term
    (** The type of terms *)

    type 'a t
    (** Polymorphic tags *)

    val named : string t
    (** A tag used to named formulas in smtlib.
        Should correspond to the `:named` attribute. *)

    val triggers : term list t
    (** Multi-triggers (typically annotated on the body of
        a quantified formula and not the quantified formula itself).
        Each term is a multi-trigger represented as a
        conjunction of triggers. *)

    val rwrt : unit t
    (** A flag (i.e. unit tag), indicating that the tagged term/formula
        is to be considered as a rewrite rule. *)

end

(** Tags *)
module type Zf_Base = sig

  type 'a t
  (** Polymorphic tags *)

  val rwrt : unit t
  (** A flag (i.e. unit tag), indicatgin that the tagged term/formula
      is to be considered as a rewrite rule. *)

  type name
  (** A type used to specify a name for printing identifiers *)

  val name : name t
  (** A tag used to specify what to print when printing an identifier *)

  val exact : string -> name
  (** Print the identifier with this exact string. *)

  type pos
  (** A type to indicate how to print identifiers *)

  val pos : pos t
  (** A tag to specify how to print identifiers*)

  val infix : pos
  (** The tagged identifier is an infix symbol *)

  val prefix : pos
  (** The tagged identifier is a prefix symbol *)

end
