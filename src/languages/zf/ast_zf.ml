
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

module type Term = sig

  type t
  type location

  val tType     : t
  val wildcard  : t
  val prop      : t
  val true_     : t
  val false_    : t

  val const   : ?loc:location -> string -> t

  val apply   : ?loc:location -> t -> t list -> t
  val colon   : ?loc:location -> t -> t -> t
  val arrow   : ?loc:location -> t -> t -> t

  val eq : ?loc:location -> t -> t -> t

  val not_  : ?loc:location -> t -> t
  val or_   : ?loc:location -> t list -> t
  val and_  : ?loc:location -> t list -> t
  val imply : ?loc:location -> t -> t -> t
  val equiv : ?loc:location -> t -> t -> t

  val pi     : ?loc:location -> t list -> t -> t
  val letin  : ?loc:location -> t list -> t -> t
  val forall : ?loc:location -> t list -> t -> t
  val exists : ?loc:location -> t list -> t -> t

end

module type Statement = sig

  type t
  type term
  type location

  val default_attr : term
  val attr : ?loc:location -> string -> term

  val data        : ?loc:location -> t list -> t
  val decl        : ?loc:location -> string -> term -> t
  val definition  : ?loc:location -> string -> term -> term -> t
  val inductive   : ?loc:location -> string -> term list -> (string * term list) list -> t

  val rewrite     : ?loc:location -> attr:term -> term -> t
  val goal        : ?loc:location -> attr:term -> term -> t
  val assume      : ?loc:location -> attr:term -> term -> t

end

