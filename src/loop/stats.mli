
(* This file is free software, part of Dolmen. See file "LICENSE" for more details. *)

val file_size : string -> int
(** File size in bytes. *)

(** Interface *)
module type S = Stats_intf.S

(** This module provides convenient pipes for parsing and dealing with includes. *)
module Make(State : State.S) : S with type state := State.t

