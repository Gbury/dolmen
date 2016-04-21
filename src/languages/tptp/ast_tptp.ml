

module type Term = sig

  type t
  type location

  val column : ?loc:location -> t -> t -> t

  val var : ?loc:location -> string -> t
  val const : ?loc:location -> string -> t
  val distinct : ?loc:location -> string -> t
  val integer : ?loc:location -> string -> t
  val rational : ?loc:location -> string -> t
  val real : ?loc:location -> string -> t

  val eq   : loc:location -> t
  val neq  : loc:location -> t
  val not_ : loc:location -> t
  val or_  : loc:location -> t
  val and_ : loc:location -> t
  val xor  : loc:location -> t
  val nor  : loc:location -> t
  val nand : loc:location -> t
  val equiv : loc:location -> t
  val implies : loc:location -> t
  val implied : loc:location -> t
  val data : loc:location -> t

  val ite : ?loc:location -> t -> t -> t -> t
  val apply : ?loc:location -> t -> t list -> t

  val prod    : ?loc:location -> t -> t -> t
  val union   : ?loc:location -> t -> t -> t
  val arrow   : ?loc:location -> t -> t -> t
  val subtype : ?loc:location -> t -> t -> t

  val letin  : ?loc:location -> t -> t -> t
  val pi     : ?loc:location -> t list -> t -> t
  val forall : ?loc:location -> t list -> t -> t
  val exists : ?loc:location -> t list -> t -> t
  val lambda : ?loc:location -> t list -> t -> t

  val sequent : ?loc:location -> t list -> t list -> t

end

module type Statement = sig

  type t

  type term
  type location
  type annotation

  val annot : ?loc:location -> term -> term list -> annotation

  val include_ : ?loc:location -> string -> term list -> t

  val tpi : ?loc:location -> ?annot:annotation -> term -> string -> term -> t
  val thf : ?loc:location -> ?annot:annotation -> term -> string -> term -> t
  val tff : ?loc:location -> ?annot:annotation -> term -> string -> term -> t
  val fof : ?loc:location -> ?annot:annotation -> term -> string -> term -> t
  val cnf : ?loc:location -> ?annot:annotation -> term -> string -> term -> t

end

