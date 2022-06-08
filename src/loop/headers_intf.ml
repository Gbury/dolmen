
module type S = sig

  type state
  (* Global state. *)
  type 'a key
  (* Keys into the state *)
  type header_state
  (** type of the local state for header checking *)

  val header_check : bool key
  val header_state : header_state key
  val header_licenses : string list key
  val header_lang_version : string option key

  val check : state -> state
  (** Check a state for the required headers (once a whole pipeline
      has been completed *)

  val inspect :
    state -> Dolmen.Std.Statement.t -> state * Dolmen.Std.Statement.t
  (** Check the headers *)

end
