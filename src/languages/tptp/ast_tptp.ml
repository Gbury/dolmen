

module type Term = sig

  type t
  type location

  val typed : ?loc:location -> t -> t -> t

  val or_ : ?loc:location -> t -> t -> t
  val and_ : ?loc:location -> t -> t -> t
  val apply : ?loc:location -> t -> t -> t

  val ite_f : ?loc:location -> t -> t -> t -> t
  val let_tf : ?loc:location -> t list -> t -> t
  val let_ff : ?loc:location -> t list -> t -> t

  val prod : ?loc:location -> t -> t -> t
  val union : ?loc:location -> t -> t -> t
  val arrow : ?loc:location -> t -> t -> t
  val subtype : ?loc:location -> t -> t -> t

  val forall : ?loc:lcoation -> t list -> t -> t
  val exists : ?loc:lcoation -> t list -> t -> t
  val lambda : ?loc:lcoation -> t list -> t -> t

  val sequent : ?loc:location -> t list -> t list -> t

end

module type Phrase = sig

  type t

  type term
  type location
  type annotation

  val annot : string -> string -> annotation

  val tpi : ?loc:location -> ?annot:annotation -> string -> string -> term -> t
  val thf : ?loc:location -> ?annot:annotation -> string -> string -> term -> t
  val tff : ?loc:location -> ?annot:annotation -> string -> string -> term -> t
  val fof : ?loc:location -> ?annot:annotation -> string -> string -> term -> t
  val cnf : ?loc:location -> ?annot:annotation -> string -> string -> term -> t

end

