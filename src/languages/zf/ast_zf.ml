
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

module type Term = sig

  type t
  (** The type of terms. *)

  type location
  (** The type of locations attached to terms. *)

  type namespace
  (** The type of namespaces attached to constants. *)

  val term : namespace
  (** The only namespace used in Zf. *)

  val tType     : t
  val wildcard  : t
  val prop      : t
  val true_     : t
  val false_    : t
  (** Standard pre-defined constants. *)

  val const   : ?loc:location -> ns:namespace -> string -> t
  (** Create a new constant. *)

  val apply   : ?loc:location -> t -> t list -> t
  (** Application of terms. *)

  val colon   : ?loc:location -> t -> t -> t
  (** Juxtaposition of terms, usually used for annotating terms with types. *)

  val arrow   : ?loc:location -> t -> t -> t
  (** Arow, i.e function type constructor, currifyed. *)

  val eq : ?loc:location -> t -> t -> t
  (** Make an equality between terms. *)

  val not_  : ?loc:location -> t -> t
  val or_   : ?loc:location -> t list -> t
  val and_  : ?loc:location -> t list -> t
  val imply : ?loc:location -> t -> t -> t
  val equiv : ?loc:location -> t -> t -> t
  (** Usual propositional functions. *)

  val pi     : ?loc:location -> t list -> t -> t
  (** Dependant product, or polymorphic type quantification.
      Used to build polymorphic function types such as,
      [Pi [a] (Arrow a a)]. *)

  val letin  : ?loc:location -> t list -> t -> t
  (** Local term binding. *)

  val forall : ?loc:location -> t list -> t -> t
  (** Universal propositional quantification. *)

  val exists : ?loc:location -> t list -> t -> t
  (** Existencial porpositional qantification. *)

end

module type Statement = sig

  type t
  (** The type of statements. *)

  type term
  (** The type of terms used in statements. *)

  type location
  (** The type of locations attached to statements. *)

  val attr : ?loc:location -> string -> term
  (** Attributes for statements. *)

  val data        : ?loc:location -> t list -> t
  val decl        : ?loc:location -> string -> term -> t
  val definition  : ?loc:location -> string -> term -> term -> t
  val inductive   : ?loc:location -> string -> term list -> (string * term list) list -> t

  val rewrite     : ?loc:location -> ?attr:term -> term -> t
  val goal        : ?loc:location -> ?attr:term -> term -> t
  val assume      : ?loc:location -> ?attr:term -> term -> t

end

