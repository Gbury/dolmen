
(* This file is free software, part of dolmen. See file "LICENSE" formore information *)

module Response = struct

  type version = [
    | `Latest
    | `V2_6
    | `V2_7
  ]

  (* Alias to the sub-libraries *)
  module V2_6 = Dolmen_smtlib2_v6.Response
  module V2_7 = Dolmen_smtlib2_v7.Response

  (* Alias for the latest module *)
  module Latest = V2_7

end

module Script = struct

  type version = [
    | `Latest
    | `V2_6
    | `V2_7
    | `Poly
  ]

  (* Alias the sub-libraries *)
  module V2_6 = Dolmen_smtlib2_v6.Script
  module V2_7 = Dolmen_smtlib2_v7.Script
  module Poly = Dolmen_smtlib2_poly

  (* Alias for the latest module *)
  module Latest = V2_7

end

type version = [
  | `Script of Script.version
  | `Response of Response.version
]
