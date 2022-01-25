
(* This file is free software, part of Dolmen. See file "LICENSE" for more details. *)

(* The Main Dolmen library is used to parse input languages *)
(* ************************************************************************ *)

module P = Dolmen.Class.Response.Make
    (Dolmen.Std.Loc)
    (Dolmen.Std.Id)
    (Dolmen.Std.Term)
    (Dolmen.Std.Answer)

include (P : Dolmen.Class.Response.S with type statement := Dolmen.Std.Answer.t
                                      and type file := Dolmen.Std.Loc.file)
