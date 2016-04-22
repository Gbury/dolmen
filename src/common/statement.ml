
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(** Statement for Smtlib *)

(** Terms *)
type term = Term.t
type attr = Term.t

type annotation = attr

type inductive = {
  name : string;
  vars : term list;
  cstrs : (string * term list) list;
  loc : ParseLocation.t option;
}

(** Description of statements. *)
type descr =
  | Pack of t list

  | Pop of int
  | Push of int

  | Prove
  | Consequent of term
  | Antecedent of term

  | Include of string
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
  name : string;
  descr : descr;
  attr : attr option;
  loc : ParseLocation.t option;
}

(** Attributes *)
let attr = Term.const

let default_attr = attr ""

let annot = Term.apply

(** Internal shortcut. *)
let mk ?(name="") ?loc ?attr descr = { name; descr; loc; attr; }

(* Push/Pop *)
let pop ?loc i = mk ?loc (Pop i)
let push ?loc i = mk ?loc (Push i)

(* Assumptions and fact checking *)
let prove ?loc () = mk ?loc Prove
let antecedent ?loc ?attr t = mk ?loc ?attr (Antecedent t)
let consequent ?loc ?attr t = mk ?loc ?attr (Consequent t)

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
let check_sat = prove
let assert_ ?loc t = antecedent ?loc t

let new_type ?loc s n =
  let ty = Term.fun_ty ?loc (Misc.replicate n Term.tType) Term.tType in
  decl ?loc s ty

let type_cstr ?loc s l t' =
  let ty = Term.fun_ty ?loc l t' in
  decl ?loc s ty

let type_alias ?loc s args body =
  let t = Term.lambda args body in
  def ?loc s t

let fun_def ?loc s args ty_ret body =
  let t = Term.lambda args (Term.colon body ty_ret) in
  def ?loc s t


(* Wrappers for Zf *)
let definition ?loc s ty term =
  let t = Term.colon term ty in
  def ?loc s t

let rewrite ?loc ~attr t = antecedent ?loc ~attr t

let assume ?loc ~attr t = antecedent ?loc ~attr t

let goal ?loc ~attr t =
  mk ?loc ~attr (Pack [
      consequent t;
      prove ();
    ])


(* Wrappers for tptp *)
let include_ ?loc s l =
  let attr = Term.apply ?loc Term.and_t l in
  mk ?loc ~attr (Include s)

let tptp ?loc ?annot name_t role t =
  let aux t =
    match annot with
    | None -> t
    | Some t' -> Term.colon t t'
  in
  let attr = aux (Term.const role) in
  let name =
    match name_t with
    | { Term.term = Term.Symbol s } -> Some s
    | _ -> None
  in
  let descr = match role with
    | "axiom"
    | "hypothesis"
    | "definition"
    | "lemma"
    | "theorem"
      -> Antecedent t
    | "assumption"
    | "conjecture"
      -> Pack [
          push 1;
          consequent ~attr:(Term.false_) t;
          prove ();
          pop 1;
          antecedent ~attr:(Term.true_) t;
         ]
    | "negated_conjecture"
      -> Pack [
          push 1;
          antecedent ~attr:(Term.false_) t;
          prove ();
          pop 1;
          antecedent ~attr:(Term.true_) (Term.not_ t);
        ]
    | "type"
      -> begin match t with
          | { Term.term = Term.Colon ({ Term.term = Term.Symbol s }, ty )} ->
            Decl (s, ty)
          | _ ->
            Format.eprintf "WARNING: unexpected type declaration@.";
            Pack []
        end
    | "plain"
    | "unknown"
    | "fi_domain"
    | "fi_functors"
    | "fi_predicates"
      -> Pack []
    | _ ->
      Format.eprintf "WARNING: unknown tptp formula role: '%s'@." role;
      Pack []
  in
  mk ?name ?loc ~attr descr

let tpi = tptp
let thf = tptp
let tff = tptp
let fof = tptp
let cnf = tptp



