
(* This file is free software, part of dolmen. See file "LICENSE" formore information *)

module Response = struct

  type version = [
    | `Latest
    | `V2_6
  ]

  (* Alias to the sub-libraries *)
  module V2_6 = Dolmen_smtlib2_v6.Response

  (* Alias for the latest module *)
  module Latest = V2_6

end

module Script = struct

  type version = [
    | `Latest
    | `V2_6
    | `Poly
  ]

  (* Alias the sub-libraries *)
  module V2_6 = Dolmen_smtlib2_v6.Script
  module Poly = Dolmen_smtlib2_poly

  (* Alias for the latest module *)
  module Latest = V2_6

end

type version = [
  | `Script of Script.version
  | `Response of Response.version
]
