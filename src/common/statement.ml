
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

type term = Term.t

type t =
  | Set_logic of string
  | Set_option of string * term option
  | Set_info of string * term option

let set_logic s = Set_logic s


