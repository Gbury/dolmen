
(* This file is free software, part of Dolmen. See file "LICENSE" for more details. *)

(** This module provides convenient pipes for parsing and dealing with includes. *)
module Pipe
    (Expr : Expr_intf.S)
    (State : State_intf.Parser_pipe
     with type term := Expr.term)
  : sig

  (** {2 Pipes} *)

  val parse :
    Dolmen.Std.Statement.t list -> State.t ->
    State.t * (State.t -> Dolmen.Std.Statement.t option)
  (** Parsing function. Reads a list of prelude statements, and the state and
      returns a tuple of the new state (including the detected input language),
      together with a statement generator. *)

  val expand : State.t -> Dolmen.Std.Statement.t ->
    State.t * [ `Ok | `Gen of (State.t -> State.t -> State.t) * Dolmen.Std.Statement.t Gen.t ]
  (** Expand statements (such as includes). Returns the new state, and either:
      - [ `Ok ], which means the statement can be propagated as is
      - [ `Gen (flat, g) ], if the statement expands into a generator [g]. The bool [flat]
        indicates wether the statements in [g] should be treated as a single group of
        statements (with regards to timeouts, etc...), or as a list of independant statements
        (each with its own timeout...). *)

end
