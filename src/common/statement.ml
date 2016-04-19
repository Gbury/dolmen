
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(** Statement for Smtlib *)

(** Terms *)
type term = Term.t

type attr = string

(** Description of statements. *)
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

  | Def of string * term
  | Alias of string * term list * term

  | Get_proof
  | Get_unsat_core
  | Get_value of term list
  | Get_assignment
  | Get_assertions

  | Exit

(** Statements are wrapped in a record to have a location. *)
and t = {
  descr : descr;
  attr : attr option;
  loc : ParseLocation.t option;
}

(** Internal shortcut. *)
let mk ?loc ?attr descr = { descr; loc; attr; }

let pop ?loc i = mk ?loc (Pop i)
let push ?loc i = mk ?loc (Push i)

let assume ?loc t = mk ?loc (Assume t)
let check_sat ?loc () = mk ?loc Check_sat

let set_logic ?loc s = mk ?loc (Set_logic s)

let get_info ?loc s = mk ?loc (Get_info s)
let set_info ?loc (s, t) = mk ?loc (Set_info (s, t))

let get_option ?loc s = mk ?loc (Get_option s)
let set_option ?loc (s, t) = mk ?loc (Set_option (s, t))

let alias ?loc s l t = mk ?loc (Alias (s, l, t))

let new_type ?loc t n =
  let ty = Term.fun_ty ?loc (Misc.replicate n Term.tType) Term.tType in
  mk ?loc (Def (t, ty))

let type_alias ?loc t l t' =
  mk ?loc (Alias (t, l, t'))

let type_def ?loc t l t' =
  let ty = Term.fun_ty ?loc l t' in
  mk ?loc (Def (t, ty))

let fun_def ?loc t l _ t' =
  mk ?loc (Alias (t, l, t'))

let get_proof ?loc () = mk ?loc Get_proof
let get_unsat_core ?loc () = mk ?loc Get_unsat_core
let get_value ?loc l = mk ?loc (Get_value l)
let get_assignment ?loc () = mk ?loc Get_assignment
let get_assertions ?loc () = mk ?loc Get_assertions

let exit ?loc () = mk ?loc Exit

(* Wrappers for Zf *)

let decl = alias

let def ?loc s ty term =
  let t = Term.column term ty in
  mk ?loc (Def (s, t))



