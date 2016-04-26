
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

module type Full = sig

  type t
  type location

  (** Predefined terms *)
  val wildcard  : t

  val eq_t      : t
  val neq_t     : t

  val tType     : t
  val prop      : t

  val true_     : t
  val false_    : t

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

  (** Juxtaposition of two terms *)
  val colon : ?loc:location -> t -> t -> t

  (** Variables/Constants constructors *)
  val var      : ?loc:location -> string -> t
  val atom     : ?loc:location -> string -> t
  val const    : ?loc:location -> string -> t
  val distinct : ?loc:location -> string -> t

  val int      : ?loc:location -> string -> t
  val rat      : ?loc:location -> string -> t
  val real     : ?loc:location -> string -> t
  val hexa     : ?loc:location -> string -> t
  val binary   : ?loc:location -> string -> t

  (** Term construction *)
  val eq    : ?loc:location -> t -> t -> t

  val not_  : ?loc:location -> t -> t
  val or_   : ?loc:location -> t list -> t
  val and_  : ?loc:location -> t list -> t
  val imply : ?loc:location -> t -> t -> t
  val equiv : ?loc:location -> t -> t -> t

  (** Application and conditional *)
  val ite   : ?loc:location -> t -> t -> t -> t
  val apply : ?loc:location -> t -> t list -> t

  (** Binders constructors *)
  val pi     : ?loc:location -> t list -> t -> t
  val letin  : ?loc:location -> t list -> t -> t
  val forall : ?loc:location -> t list -> t -> t
  val exists : ?loc:location -> t list -> t -> t
  val lambda : ?loc:location -> t list -> t -> t

  (** Type constructors *)
  val arrow   : ?loc:location -> t -> t -> t
  val union   : ?loc:location -> t -> t -> t
  val product : ?loc:location -> t -> t -> t
  val subtype : ?loc:location -> t -> t -> t

  (** Descriptions *)
  val choice : ?loc:location -> t list -> t -> t
  val description : ?loc:location -> t list -> t -> t

  (** Sequents as terms *)
  val sequent : ?loc:location -> t list -> t list -> t

  (** S-expressions (for smtlib attributes) *)
  val sexpr   : ?loc:location -> t list -> t

end
