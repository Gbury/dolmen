
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

type term = Term.t

type descr =
  | Pop of int
  | Push of int

  | Assume of term
  | Check_sat

  | Set_logic of string

  | Get_info of string
  | Set_info of string * term option

  | Get_option of string
  | Set_option of string * term option

  | New_type of term * int
  | Type_alias of term * term list * term
  | Type_def of term * term list * term
  | Fun_def of term * term list * term * term

  | Get_proof
  | Get_unsat_core
  | Get_value of term list
  | Get_assignment
  | Get_assertions

  | Exit

and t = {
  descr : descr;
  loc : ParseLocation.t option;
}

let mk ?loc descr = { descr; loc; }

let pop ?loc i = mk ?loc (Pop i)
let push ?loc i = mk ?loc (Push i)

let assume ?loc t = mk ?loc (Assume t)
let check_sat ?loc () = mk ?loc Check_sat

let set_logic ?loc s = mk ?loc (Set_logic s)

let get_info ?loc s = mk ?loc (Get_info s)
let set_info ?loc (s, t) = mk ?loc (Set_info (s, t))

let get_option ?loc s = mk ?loc (Get_option s)
let set_option ?loc (s, t) = mk ?loc (Set_option (s, t))

let new_type ?loc t n = mk ?loc (New_type (t, n))
let type_alias ?loc t l t' = mk ?loc (Type_alias (t, l, t'))
let type_def ?loc t l t' = mk ?loc (Type_def (t, l, t'))
let fun_def ?loc t l t' t'' = mk ?loc (Fun_def (t, l, t', t''))

let get_proof ?loc () = mk ?loc Get_proof
let get_unsat_core ?loc () = mk ?loc Get_unsat_core
let get_value ?loc l = mk ?loc (Get_value l)
let get_assignment ?loc () = mk ?loc Get_assignment
let get_assertions ?loc () = mk ?loc Get_assertions

let exit ?loc () = mk ?loc Exit

