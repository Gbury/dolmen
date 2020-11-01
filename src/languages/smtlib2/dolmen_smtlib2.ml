
(* This file is free software, part of dolmen. See file "LICENSE" formore information *)

type version = [
  | `Latest
  | `V2_6
  | `Poly
]

(* Alias the sub-libraries *)
module V2_6 = Dolmen_smtlib2_v6
module Poly = Dolmen_smtlib2_poly

(* Alias for the latest module *)
module Latest = V2_6

