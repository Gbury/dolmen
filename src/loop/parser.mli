
(* This file is free software, part of Dolmen. See file "LICENSE" for more details. *)

(** {2 Errors} *)

val extension_not_found : string Report.error
(** Error for when an extension is not found. *)

val file_not_found : (string * string) Report.error
(** Error for a missing file *)

val lexing_error : string Report.error
(** Lexing errors *)

val parsing_error :
  [ `Regular of Dolmen.Intf.Msg.t
  | `Advanced of Dolmen.Intf.Msg.t *
                 Dolmen.Intf.Msg.t *
                 Dolmen.Intf.Msg.t
  ] Report.error
(** Parsing errors *)

module type Pipe_res = Parser_intf.Pipe_res

(** This module provides convenient pipes for parsing and dealing with includes. *)
module Pipe
    (Expr : Expr_intf.S)
    (State : State_intf.Parser_pipe
     with type term := Expr.term)
  : Pipe_res with type state := State.t

