
(* This file is free software, part of dolmen. See file "LICENSE" formore information *)

type version = [
  | `Latest
  | `V6_3_0
]

(* Alias the sub-libraries *)
module V6_3_0 = Dolmen_tptp_v6_3_0

(* Alias for the latest module *)
module Latest = V6_3_0

