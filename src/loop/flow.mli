
(* This file is free software, part of Dolmen. See file "LICENSE" for more details. *)

val code : Code.t
(** The code for warning/errors related to flow checking. *)

module type S = Flow_intf.S

module Make(S : State.S) : S with type state := S.t
                              and type 'a key := 'a S.key

