
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

module type Term = sig

  type t

  type location

  val atom : ?loc:location -> string -> t

end

module type Statement = sig

  type t

  type atom

  type location

  val clause : ?loc:location -> atom list -> t

end
