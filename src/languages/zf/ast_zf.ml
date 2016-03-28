
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

module type Term = sig

  type t
  type location

  val tType : t
  val wildcard : t

  val true_ : t
  val false_ : t

  val at_loc : t -> location -> t

  val const : ?loc:location -> string -> t
  val app : ?loc:location -> t -> t list -> t
  val typed : ?loc:location -> t -> t -> t

  val eq : ?loc:location -> t -> t -> t

  val not_ : ?loc:location -> t -> t
  val or_ : ?loc:location -> t -> t list -> t
  val and_ : ?loc:location -> t -> t list -> t
  val imply : ?loc:location -> t -> t -> t
  val equiv : ?loc:location -> t -> t -> t

  val letin : ?loc:location -> t list -> t -> t
  val forall : ?loc:location -> t list -> t -> t
  val exists : ?loc:location -> t list -> t -> t

end

module type Statement = sig

  type t
  type attr
  type term
  type location

  val default_attr : attr
  val attr : ?loc:location -> string -> attr

  val decl : ?loc:location -> string -> term -> t
  val def : ?loc:location -> string -> term -> term -> t
  val type_def : ?loc:location -> string -> term list -> (term * term) list -> t

  val data : ?loc:location -> t list -> t
  val goal : ?loc:location -> attr:attr -> term -> t
  val assume : ?loc:location -> attr:attr -> term -> t

end

