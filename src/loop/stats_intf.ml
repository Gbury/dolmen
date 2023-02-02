
(* This file is free software, part of Dolmen. See file "LICENSE" for more details. *)

module type S = sig

  type state
  (** The type of state for a whole pipeline *)

  type input
  (** A type to track input files *)

  type counter
  (** Time counter *)

  val start_counter : state -> counter option
  (** Start a counter. *)

  val new_input : state -> string -> int -> input * state
  (** Record a new input source, with the given name and size (in bytes). *)

  val record_parsed : state -> input option -> counter option -> Dolmen.Std.Loc.t -> state
  (** *)

  val record_typed : state -> counter option -> Dolmen.Std.Loc.t -> 'a -> state
  (** *)

  val record_checked : state -> counter option -> Dolmen.Std.Loc.t -> [ `Add of 'a | `Set of 'a] -> state
  (** *)

end
