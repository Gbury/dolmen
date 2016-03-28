
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

type term = Term.t

type t =
  | Cnf of term

let clause ?loc l = Cnf (Term.or_ ?loc l)

