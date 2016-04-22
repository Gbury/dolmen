
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

module type Term = sig

  type t

  type location

  val const   : ?loc:location -> string -> t

  val apply   : ?loc:location -> t -> t list -> t

  val colon   : ?loc:location -> t -> t -> t

  val letin   : ?loc:location -> t list -> t -> t
  val forall  : ?loc:location -> t list -> t -> t
  val exists  : ?loc:location -> t list -> t -> t

  val sexpr   : ?loc:location -> t list -> t
  val attr    : ?loc:location -> t -> (string * t option) list -> t

  val int     : ?loc:location -> string -> t
  val real    : ?loc:location -> string -> t
  val hexa    : ?loc:location -> string -> t
  val binary  : ?loc:location -> string -> t

end

module type Statement = sig

  type t

  type term

  type location

  val pop : ?loc:location -> int -> t
  val push : ?loc:location -> int -> t

  val assert_ : ?loc:location -> term -> t
  val check_sat : ?loc:location -> unit -> t

  val set_logic : ?loc:location -> string -> t

  val get_info : ?loc:location -> string -> t
  val set_info : ?loc:location -> string * term option -> t

  val get_option : ?loc:location -> string -> t
  val set_option : ?loc:location -> string * term option -> t

  val new_type : ?loc:location -> string -> int -> t
  val type_alias : ?loc:location -> string -> term list -> term -> t
  val type_cstr : ?loc:location -> string -> term list -> term -> t
  val fun_def : ?loc:location -> string -> term list -> term -> term -> t

  val get_proof : ?loc:location -> unit -> t
  val get_unsat_core : ?loc:location -> unit -> t
  val get_value : ?loc:location -> term list -> t
  val get_assignment : ?loc:location -> unit -> t
  val get_assertions : ?loc:location -> unit -> t

  val exit : ?loc:location -> unit -> t

end

