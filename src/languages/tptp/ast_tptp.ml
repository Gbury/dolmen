

module type Term = sig

  type t
  type location

  val eq_t      : t
  val neq_t     : t
  val not_t     : t
  val or_t      : t
  val and_t     : t
  val xor_t     : t
  val nor_t     : t
  val nand_t    : t
  val equiv_t   : t
  val implies_t : t
  val implied_t : t
  val data_t    : t

  val colon : ?loc:location -> t -> t -> t

  val var      : ?loc:location -> string -> t
  val const    : ?loc:location -> string -> t
  val distinct : ?loc:location -> string -> t
  val int      : ?loc:location -> string -> t
  val rat      : ?loc:location -> string -> t
  val real     : ?loc:location -> string -> t

  val ite   : ?loc:location -> t -> t -> t -> t
  val apply : ?loc:location -> t -> t list -> t

  val union   : ?loc:location -> t -> t -> t
  val product : ?loc:location -> t -> t -> t
  val arrow   : ?loc:location -> t -> t -> t
  val subtype : ?loc:location -> t -> t -> t

  val pi     : ?loc:location -> t list -> t -> t
  val letin  : ?loc:location -> t list -> t -> t
  val forall : ?loc:location -> t list -> t -> t
  val exists : ?loc:location -> t list -> t -> t
  val lambda : ?loc:location -> t list -> t -> t

  val choice : ?loc:location -> t list -> t -> t
  val description : ?loc:location -> t list -> t -> t

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

