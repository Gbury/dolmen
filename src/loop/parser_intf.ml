
(* This file is free software, part of Dolmen. See file "LICENSE" for more details. *)

module type S = sig

  type state

  type 'a key

  val syntax_error_ref : bool key

  val parse_logic :
    Dolmen.Std.Statement.t list -> state -> Logic.language State.file ->
    state * (state -> state * Dolmen.Std.Statement.t option)
  (** Parsing function. Reads a list of prelude statements, and the state and
      returns a tuple of the new state (including the detected input language),
      together with a statement generator. *)

  val parse_response :
    Dolmen.Std.Answer.t list -> state -> Response.language State.file ->
    state * (state -> state * Dolmen.Std.Answer.t option)
  (** Parsing function. Reads a list of prelude statements, and the state and
      returns a tuple of the new state (including the detected input language),
      together with a statement generator. *)

  val expand : state -> Dolmen.Std.Statement.t ->
    state * [ `Ok | `Gen of (state -> state -> state) *
                              (state -> state * Dolmen.Std.Statement.t option) ]
  (** Expand statements (such as includes). Returns the new state, and either:
      - [ `Ok ], which means the statement can be propagated as is
      - [ `Gen (flat, g) ], if the statement expands into a generator [g]. The bool [flat]
        indicates wether the statements in [g] should be treated as a single group of
        statements (with regards to timeouts, etc...), or as a list of independant statements
        (each with its own timeout...). *)

end
