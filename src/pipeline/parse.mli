(* This file is free software, part of Dolmen. See file "LICENSE" for more details. *)

(** This is an instanciation of the Logic class with the standard
    implementation of parsed terms and statements of Dolmen. *)
include Dolmen.Logic.S with type statement := Dolmen.Statement.t

