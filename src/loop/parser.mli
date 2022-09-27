
(* This file is free software, part of Dolmen. See file "LICENSE" for more details. *)

(** {2 Errors} *)

val extension_not_found : string Report.error
(** Error for when an extension is not found. *)

val file_not_found : (string * string) Report.error
(** Error for a missing file *)

val lexing_error : string Report.error
(** Lexing errors *)

val parsing_error : (bool *
  [ `Regular of Dolmen.Intf.Msg.t
  | `Advanced of string *
                 Dolmen.Intf.Msg.t *
                 Dolmen.Intf.Msg.t *
                 Dolmen.Intf.Msg.t
  ]) Report.error
(** Parsing errors *)

module type S = Parser_intf.S

(** This module provides convenient pipes for parsing and dealing with includes. *)
module Make(State : State.S) : S with type state := State.t and type 'a key := 'a State.key

