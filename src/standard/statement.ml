
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(* Type definitions *)
type term = Term.t
type location = ParseLocation.t

type inductive = {
  id : Id.t;
  vars : term list;
  cstrs : (Id.t * term list) list;
  loc : location option;
}

(* Description of statements. *)
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

  | Def of Id.t * term
  | Decl of Id.t * term
  | Inductive of inductive

  | Get_proof
  | Get_unsat_core
  | Get_value of term list
  | Get_assignment
  | Get_assertions

  | Exit

(* Statements are wrapped in a record to have a location. *)
and t = {
  id : Id.t;
  descr : descr;
  attr : term option;
  loc : location option;
}

(* Debug printing *)

let rec pp_descr b = function
  | Pack l ->
    Printf.bprintf b "pack(%d):\n" (List.length l);
    Misc.pp_list ~pp_sep:Buffer.add_char ~sep:'\n' ~pp b l

  | Pop i -> Printf.bprintf b "pop: %d" i
  | Push i -> Printf.bprintf b "push: %d" i

  | Prove -> Printf.bprintf b "Prove"
  | Consequent t -> Printf.bprintf b "consequent: %a" Term.pp t
  | Antecedent t -> Printf.bprintf b "antecedent: %a" Term.pp t

  | Include f -> Printf.bprintf b "include: %s" f
  | Set_logic s -> Printf.bprintf b "set-logic: %s" s

  | Get_info s -> Printf.bprintf b "get-info: %s" s
  | Set_info (s, o) ->
    Printf.bprintf b "set-info: %s <- %a" s (Misc.pp_opt Term.pp) o

  | Get_option s -> Printf.bprintf b "get-option: %s" s
  | Set_option (s, o) ->
    Printf.bprintf b "set-option: %s <- %a" s (Misc.pp_opt Term.pp) o

  | Def (id, t) -> Printf.bprintf b "def: %a = %a" Id.pp id Term.pp t
  | Decl (id, t) -> Printf.bprintf b "decl: %a : %a" Id.pp id Term.pp t
  | Inductive i ->
    Printf.bprintf b "Inductive(%d): %a, %a\n"
      (List.length i.cstrs) Id.pp i.id
      (Misc.pp_list ~pp_sep:Buffer.add_string ~sep:" " ~pp:Term.pp) i.vars;
    Misc.pp_list ~pp_sep:Buffer.add_string ~sep:"\n"
      ~pp:(fun b (cstr, l) -> Printf.bprintf b "%a: %a"
              Id.pp cstr
              (Misc.pp_list ~pp_sep:Buffer.add_string ~sep:" " ~pp:Term.pp) l
          ) b i.cstrs

  | Get_proof -> Printf.bprintf b "get-proof"
  | Get_unsat_core -> Printf.bprintf b "get-unsat-core"
  | Get_value l ->
    Printf.bprintf b "get-value(%d):\n" (List.length l);
    Misc.pp_list ~pp_sep:Buffer.add_string ~sep:"\n" ~pp:Term.pp b l
  | Get_assignment -> Printf.bprintf b "get-assignment"
  | Get_assertions -> Printf.bprintf b "get-assertions"

  | Exit -> Printf.bprintf b "exit"

and pp b = function { descr } ->
  Printf.bprintf b "%a" pp_descr descr

(* Pretty printing *)

let rec print_descr fmt = function
  | Pack l ->
    Format.fprintf fmt "@[<hov 2>pack(%d):@ %a@]" (List.length l)
    (Misc.print_list ~print_sep:Format.fprintf ~sep:"@ " ~print) l

  | Pop i -> Format.fprintf fmt "pop: %d" i
  | Push i -> Format.fprintf fmt "push: %d" i

  | Prove ->
    Format.fprintf fmt "Prove"
  | Consequent t ->
    Format.fprintf fmt "@[<hov 2>consequent:@ %a@]" Term.print t
  | Antecedent t ->
    Format.fprintf fmt "@[<hov 2>antecedent:@ %a@]" Term.print t

  | Include f ->
    Format.fprintf fmt "@[<hov 2>include:@ %s@]" f
  | Set_logic s ->
    Format.fprintf fmt "@[<hov 2>set-logic:@ %s@]" s

  | Get_info s ->
    Format.fprintf fmt "@[<hov 2>get-info:@ %s@]" s
  | Set_info (s, o) ->
    Format.fprintf fmt "@[<hov 2>set-info:@ %s <-@ %a@]"
      s (Misc.print_opt Term.print) o

  | Get_option s ->
    Format.fprintf fmt "@[<hov 2>get-option:@ %s@]" s
  | Set_option (s, o) ->
    Format.fprintf fmt "@[<hov 2>set-option:@ %s <-@ %a@]"
      s (Misc.print_opt Term.print) o

  | Def (id, t) ->
    Format.fprintf fmt "@[<hov 2>def:@ %a =@ %a@]" Id.print id Term.print t
  | Decl (id, t) ->
    Format.fprintf fmt "@[<hov 2>decl:@ %a :@ %a@]" Id.print id Term.print t
  | Inductive i ->
    Format.fprintf fmt "@[<hov 2>Inductive(%d) %a@ %a@\n%a@]"
      (List.length i.cstrs) Id.print i.id
      (Misc.print_list ~print_sep:Format.fprintf ~sep:"@ " ~print:Term.print) i.vars
      (Misc.print_list ~print_sep:Format.fprintf ~sep:"@\n"
         ~print:(fun fmt (cstr, l) ->
             Format.fprintf fmt "%a: %a" Id.print cstr (
               Misc.print_list ~print_sep:Format.fprintf ~sep:"@ " ~print:Term.print
           ) l)) i.cstrs

  | Get_proof -> Format.fprintf fmt "get-proof"
  | Get_unsat_core -> Format.fprintf fmt "get-unsat-core"
  | Get_value l ->
    Format.fprintf fmt "@[<hov 2>get-value(%d):@ %a@]" (List.length l)
      (Misc.print_list ~print_sep:Format.fprintf ~sep:"@ " ~print:Term.print) l
  | Get_assignment -> Format.fprintf fmt "get-assignment"
  | Get_assertions -> Format.fprintf fmt "get-assertions"

  | Exit -> Format.fprintf fmt "exit"

and print fmt = function { descr } ->
  Format.fprintf fmt "%a" print_descr descr


(* Attributes *)
let attr ?loc s = Term.const ?loc Id.(mk Attr s)

let annot = Term.apply

(* Internal shortcut. *)
let mk ?(id=Id.(mk (mod_name "") "")) ?loc ?attr descr =
  { id; descr; loc; attr; }

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
let decl ?loc id ty = mk ?loc (Decl (id, ty))

(* Definitions, i.e given identifier, with arguments,
   is equal to given term *)
let def ?loc id t = mk ?loc (Def (id, t))

(* Inductive types, i.e polymorphic variables, and
   a list of constructors. *)
let inductive ?loc id vars cstrs =
  mk ?loc (Inductive {id; vars; cstrs; loc; })

let data ?loc l = mk ?loc (Pack l)

(* Return values *)
let get_proof ?loc () = mk ?loc Get_proof
let get_unsat_core ?loc () = mk ?loc Get_unsat_core
let get_value ?loc l = mk ?loc (Get_value l)
let get_assignment ?loc () = mk ?loc Get_assignment
let get_assertions ?loc () = mk ?loc Get_assertions

(* End statement *)
let exit ?loc () = mk ?loc Exit




(* Dimacs wrapper *)
let clause ?loc l =
  let t = Term.or_ ?loc l in
  antecedent ?loc t

(* Smtlib wrappers *)
let check_sat = prove
let assert_ ?loc t = antecedent ?loc t

let type_decl ?loc id n =
  let ty = Term.fun_ty ?loc (Misc.replicate n Term.tType) Term.tType in
  mk ?loc (Decl (id, ty))

let fun_decl ?loc id l t' =
  let ty = Term.fun_ty ?loc l t' in
  mk ?loc (Decl (id, ty))

let type_def ?loc id args body =
  let l = List.map (fun id -> Term.colon (Term.const id) Term.tType) args in
  let t = Term.lambda l body in
  mk ?loc (Def (id, t))

let fun_def ?loc id args ty_ret body =
  let t = Term.lambda args (Term.colon body ty_ret) in
  mk ?loc (Def (id, t))


(* Wrappers for Zf *)
let definition ?loc s ty term =
  let t = Term.colon term ty in
  def ?loc s t

let rewrite ?loc ?attr t = antecedent ?loc ?attr t

let assume ?loc ?attr t = antecedent ?loc ?attr t

let goal ?loc ?attr t =
  mk ?loc ?attr (Pack [
      consequent t;
      prove ();
    ])


(* Wrappers for tptp *)
let include_ ?loc s l =
  let attr = Term.apply ?loc Term.and_t
      (List.map Term.const l) in
  mk ?loc ~attr (Include s)

let tptp ?loc ?annot id role t =
  let aux t =
    match annot with
    | None -> t
    | Some t' -> Term.colon t t'
  in
  let attr = aux (Term.const Id.(mk Attr role)) in
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
  mk ~id ?loc ~attr descr

let tpi = tptp
let thf = tptp
let tff = tptp
let fof = tptp
let cnf = tptp



