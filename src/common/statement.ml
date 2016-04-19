
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(** Statement for Smtlib *)

(** Terms *)
type term = Term.t

type attr = string

type inductive = {
  name : string;
  vars : term list;
  cstrs : (string * term list) list;
  loc : ParseLocation.t option;
}

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
  | Decl of string * term
  | Inductive of inductive list

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

(** Attributes *)
let attr ?loc:_ msg = msg

let default_attr = attr ""

(** Internal shortcut. *)
let mk ?loc ?attr descr = { descr; loc; attr; }

(* Push/Pop *)
let pop ?loc i = mk ?loc (Pop i)
let push ?loc i = mk ?loc (Push i)

(* Assumptions and fact checking *)
let check_sat ?loc () = mk ?loc Check_sat
let assume ?loc ~attr t = mk ?loc ~attr (Assume t)

(* Options statements *)
let set_logic ?loc s = mk ?loc (Set_logic s)

let get_info ?loc s = mk ?loc (Get_info s)
let set_info ?loc (s, t) = mk ?loc (Set_info (s, t))

let get_option ?loc s = mk ?loc (Get_option s)
let set_option ?loc (s, t) = mk ?loc (Set_option (s, t))

(* Declarations, i.e given identifier has given type *)
let decl ?loc s ty = mk ?loc (Decl (s, ty))

(* Definitions, i.e given identifier, with arguments,
   is equal to given term *)
let def ?loc s t = mk ?loc (Def (s, t))

(* Inductive types, i.e polymorphic variables, and
   a list of constructors. *)
let inductive ?loc name vars cstrs =
  mk ?loc (Inductive [{name; vars; cstrs; loc; }])

let data ?loc l =
  let aux = function
    | { descr = Inductive [ d ] } -> d
    | _ -> raise (Invalid_argument "Statement.data")
  in
  mk ?loc (Inductive (List.map aux l))

(* Return values *)
let get_proof ?loc () = mk ?loc Get_proof
let get_unsat_core ?loc () = mk ?loc Get_unsat_core
let get_value ?loc l = mk ?loc (Get_value l)
let get_assignment ?loc () = mk ?loc Get_assignment
let get_assertions ?loc () = mk ?loc Get_assertions

(* End statement *)
let exit ?loc () = mk ?loc Exit


(* Smtlib wrappers *)
let assert_ ?loc t = mk ?loc (Assume t)

let new_type ?loc s n =
  let ty = Term.fun_ty ?loc (Misc.replicate n Term.tType) Term.tType in
  decl ?loc s ty

let type_cstr ?loc s l t' =
  let ty = Term.fun_ty ?loc l t' in
  decl ?loc s ty

let type_alias ?loc s args body =
  let t = Term.mk_fun args body in
  def ?loc s t

let fun_def ?loc s args ty_ret body =
  let t = Term.mk_fun args (Term.column body ty_ret) in
  def ?loc s t

(* Wrappers for Zf *)
let definition ?loc s ty term =
  let t = Term.column term ty in
  def ?loc s t

let goal ?loc ~attr t = assume ?loc ~attr (Term.not_ t)


