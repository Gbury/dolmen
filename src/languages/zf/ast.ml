
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

  val tType     : ?loc:location -> unit -> t
  val prop      : ?loc:location -> unit -> t
  val ty_int    : ?loc:location -> unit -> t
  val wildcard  : ?loc:location -> unit -> t
  val true_     : ?loc:location -> unit -> t
  val false_    : ?loc:location -> unit -> t
  (** Standard pre-defined constants. *)

  val quoted  : ?loc:location -> string -> t
  (** Create an attribute from a quoted string. *)

  val const   : ?loc:location -> id -> t
  (** Create a new constant. *)

  val int     : ?loc:location -> string -> t
  (** Create an integer constant from a string. *)

  val apply   : ?loc:location -> t -> t list -> t
  (** Application of terms. *)

  val colon   : ?loc:location -> t -> t -> t
  (** Juxtaposition of terms, usually used for annotating terms with types. *)

  val arrow   : ?loc:location -> t -> t -> t
  (** Arow, i.e function type constructor, currifyed. *)

  val eq : ?loc:location -> t -> t -> t
  (** Make an equality between terms. *)

  val neq : ?loc:location -> t list -> t
  (** Make an disequality between terms. *)

  val not_  : ?loc:location -> t -> t
  val or_   : ?loc:location -> t list -> t
  val and_  : ?loc:location -> t list -> t
  val imply : ?loc:location -> t -> t -> t
  val equiv : ?loc:location -> t -> t -> t
  (** Usual propositional functions. *)

  val ite    : ?loc:location -> t -> t -> t -> t
  (** Conditional construction. *)

  val pi     : ?loc:location -> t list -> t -> t
  (** Dependant product, or polymorphic type quantification.
      Used to build polymorphic function types such as,
      [Pi [a] (Arrow a a)]. *)

  val letin  : ?loc:location -> t list -> t -> t
  (** Local term binding. *)

  val forall : ?loc:location -> t list -> t -> t
  (** Universal propositional quantification. *)

  val exists : ?loc:location -> t list -> t -> t
  (** Existencial propositional qantification. *)

  val match_ : ?loc:location -> t -> (t * t) list -> t
  (** Pattern matching. The first term is the term to match,
      and each tuple in the list is a match case, which is a pair
      of a pattern and a match branch. *)

  val lambda : ?loc:location -> t list -> t -> t
  (** Create a lambda. *)

  val uminus : ?loc:location -> t -> t
  (** Arithmetic unary minus. *)

  val add    : ?loc:location -> t -> t -> t
  (** Arithmetic addition. *)

  val sub    : ?loc:location -> t -> t -> t
  (** Arithmetic substraction. *)

  val mult   : ?loc:location -> t -> t -> t
  (** Arithmetic multiplication. *)

  val lt     : ?loc:location -> t -> t -> t
  (** Arithmetic "lesser than" comparison (strict). *)

  val leq    : ?loc:location -> t -> t -> t
  (** Arithmetic "lesser or equal" comparison. *)

  val gt     : ?loc:location -> t -> t -> t
  (** Arithmetic "greater than" comparison (strict). *)

  val geq    : ?loc:location -> t -> t -> t
  (** Arithmetic "greater or equal" comparison. *)

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

