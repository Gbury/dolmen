
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

module type Id = sig

  type t
  (** The type of identifiers *)

  type namespace
  (** The type of namespaces for identifiers *)

  val term : namespace
  (** The naemspace for terms, types, and pretty much everything *)

  val mk : namespace -> string -> t
  (** Make identifiers from a namespace and a string. *)

end

module type Term = sig

  type t
  (** The type of terms. *)

  type id
  (** The type of identifiers *)

  type location
  (** The type of locations attached to terms. *)

  val ac : t
  (** Pre-defined constant to specify associative/commutative
      symbols in attributes. *)

  val tType     : t
  val wildcard  : t
  val prop      : t
  val true_     : t
  val false_    : t
  (** Standard pre-defined constants. *)

  val name    : ?loc:location -> id -> t
  (** Create a name for a statement. *)

  val const   : ?loc:location -> id -> t
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

  type id
  (** The type of identifiers *)

  type term
  (** The type of terms used in statements. *)

  type location
  (** The type of locations attached to statements. *)

  val import      : ?loc:location -> string -> t

  val data        : ?loc:location -> ?attrs:term list -> t list -> t
  val defs        : ?loc:location -> ?attrs:term list -> t list -> t

  val rewrite     : ?loc:location -> ?attrs:term list -> term -> t
  val goal        : ?loc:location -> ?attrs:term list -> term -> t
  val assume      : ?loc:location -> ?attrs:term list -> term -> t
  val lemma       : ?loc:location -> ?attrs:term list -> term -> t

  val decl        : ?loc:location -> ?attrs:term list ->
                      id -> term -> t
  val definition  : ?loc:location -> ?attrs:term list ->
                      id -> term -> term list -> t
  val inductive   : ?loc:location -> ?attrs:term list ->
                      id -> term list -> (id * term list) list -> t

end

