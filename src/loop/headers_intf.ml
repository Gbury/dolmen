
(* This file is free software, part of Dolmen. See file "LICENSE" for more details. *)

module type S = sig

  type state
  (* Global state. *)

  type 'a key
  (* Keys into the state *)

  type header_state
  (** type of the local state for header checking *)

  val header_check : bool key
  (** Key for deciding whether to do any check on headers. *)

  val header_state : header_state key
  (** Key for the local header state. *)

  val header_licenses : string list key
  (** Key for the list of allowed licenses in headers. *)

  val header_lang_version : string option key
  (** Key for the allowed language version in headers. *)

  val init :
    header_check:bool ->
    ?header_state:header_state ->
    header_licenses:string list ->
    header_lang_version:string option ->
    state -> state
  (** Init a state with all the relevant keys for this pipeline. *)

  val inspect :
    state -> Dolmen.Std.Statement.t -> state * Dolmen.Std.Statement.t
  (** Check the headers *)

  val check : state -> state
  (** Check a state for the required headers (once a whole pipeline
      has been completed *)

end
