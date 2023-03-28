
(* This file is free software, part of Dolmen. See file "LICENSE" for more details. *)

module type S = sig

  type state
  (* Global state. *)

  type 'a key
  (* Keys into the state *)

  val init :
    flow_check:bool ->
    state -> state

  val inspect :
    state -> Dolmen.Std.Statement.t -> state * Dolmen.Std.Statement.t
  (** Inspect statements *)

  val finalise : state -> state
  (** Finalise the flow check *)

end
