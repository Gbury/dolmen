(* This file is free software, part of Dolmen. See file "LICENSE" for more details. *)

(* The Main Dolmen library is used to parse input languages *)
(* ************************************************************************ *)

module P = Dolmen.Logic.Make
    (Dolmen.ParseLocation)
    (Dolmen.Id)
    (Dolmen.Term)
    (Dolmen.Statement)

include (P : Dolmen.Logic.S with type statement := Dolmen.Statement.t)

