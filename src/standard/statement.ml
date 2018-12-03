
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
  | Reset_assertions

  | Plain of term

  | Prove of term list
  | Clause of term list
  | Antecedent of term
  | Consequent of term

  | Include of string
  | Set_logic of string

  | Get_info of string
  | Set_info of term

  | Get_option of string
  | Set_option of term

  | Def of Id.t * term
  | Decl of Id.t * term
  | Inductive of inductive

  | Get_proof
  | Get_unsat_core
  | Get_unsat_assumptions
  | Get_model
  | Get_value of term list
  | Get_assignment

  | Get_assertions

  | Echo of string
  | Reset
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
  | Reset_assertions -> Printf.bprintf b "reset assertions"

  | Plain t -> Printf.bprintf b "plain: %a" Term.pp t

  | Prove [] -> Printf.bprintf b "Prove"
  | Prove l ->
    Printf.bprintf b "Prove assuming: %a"
      (Misc.pp_list ~pp_sep:Buffer.add_string ~sep:" && " ~pp:Term.pp) l

  | Clause l ->
    Printf.bprintf b "clause: %a"
      (Misc.pp_list ~pp_sep:Buffer.add_string ~sep:" || " ~pp:Term.pp) l
  | Antecedent t -> Printf.bprintf b "antecedent: %a" Term.pp t
  | Consequent t -> Printf.bprintf b "consequent: %a" Term.pp t

  | Include f -> Printf.bprintf b "include: %s" f
  | Set_logic s -> Printf.bprintf b "set-logic: %s" s

  | Get_info s -> Printf.bprintf b "get-info: %s" s
  | Set_info t ->
    Printf.bprintf b "set-info: %a" Term.pp t

  | Get_option s -> Printf.bprintf b "get-option: %s" s
  | Set_option t ->
    Printf.bprintf b "set-option: %a" Term.pp t

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
  | Get_unsat_assumptions -> Printf.bprintf b "get-unsat-assumptions"
  | Get_model -> Printf.bprintf b "get-model"
  | Get_value l ->
    Printf.bprintf b "get-value(%d):\n" (List.length l);
    Misc.pp_list ~pp_sep:Buffer.add_string ~sep:"\n" ~pp:Term.pp b l
  | Get_assignment -> Printf.bprintf b "get-assignment"
  | Get_assertions -> Printf.bprintf b "get-assertions"

  | Echo s -> Printf.bprintf b "echo: %s" s
  | Reset -> Printf.bprintf b "reset"
  | Exit -> Printf.bprintf b "exit"

and pp b = function { descr; _ } ->
  Printf.bprintf b "%a" pp_descr descr

(* Pretty printing *)

let rec print_descr fmt = function
  | Pack l ->
    Format.fprintf fmt "@[<hov 2>pack(%d):@ %a@]" (List.length l)
    (Misc.print_list ~print_sep:Format.fprintf ~sep:"@ " ~print) l

  | Pop i -> Format.fprintf fmt "pop: %d" i
  | Push i -> Format.fprintf fmt "push: %d" i
  | Reset_assertions -> Format.fprintf fmt "reset assertions"

  | Plain t -> Format.fprintf fmt "@[<hov 2>plain: %a@]" Term.print t

  | Prove [] -> Format.fprintf fmt "Prove"
  | Prove l ->
    Format.fprintf fmt "Prove assuming: %a"
      (Misc.print_list ~print_sep:Format.fprintf ~sep:" &&@ " ~print:Term.print) l

  | Clause l ->
    Format.fprintf fmt "@[<hov 2>clause:@ %a@]"
      (Misc.print_list ~print_sep:Format.fprintf ~sep:" ||@ " ~print:Term.print) l
  | Antecedent t ->
    Format.fprintf fmt "@[<hov 2>antecedent:@ %a@]" Term.print t
  | Consequent t ->
    Format.fprintf fmt "@[<hov 2>consequent:@ %a@]" Term.print t

  | Include f ->
    Format.fprintf fmt "@[<hov 2>include:@ %s@]" f
  | Set_logic s ->
    Format.fprintf fmt "@[<hov 2>set-logic:@ %s@]" s

  | Get_info s ->
    Format.fprintf fmt "@[<hov 2>get-info:@ %s@]" s
  | Set_info t ->
    Format.fprintf fmt "@[<hov 2>set-info:@ %a@]" Term.print t

  | Get_option s ->
    Format.fprintf fmt "@[<hov 2>get-option:@ %s@]" s
  | Set_option t ->
    Format.fprintf fmt "@[<hov 2>set-option:@ %a@]" Term.print t

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
  | Get_unsat_assumptions -> Format.fprintf fmt "get-unsat-assumptions"
  | Get_model -> Format.fprintf fmt "get-model"
  | Get_value l ->
    Format.fprintf fmt "@[<hov 2>get-value(%d):@ %a@]" (List.length l)
      (Misc.print_list ~print_sep:Format.fprintf ~sep:"@ " ~print:Term.print) l
  | Get_assignment -> Format.fprintf fmt "get-assignment"
  | Get_assertions -> Format.fprintf fmt "get-assertions"

  | Echo s -> Format.fprintf fmt "echo: %s" s
  | Reset -> Format.fprintf fmt "reset"
  | Exit -> Format.fprintf fmt "exit"

and print fmt = function { descr; _ } ->
  Format.fprintf fmt "%a" print_descr descr

(** Annotations *)
let annot = Term.apply

(* Internal shortcut. *)
let mk ?(id=Id.(mk decl "")) ?loc ?attr descr =
  { id; descr; loc; attr; }

(* Pack *)
let pack ?id ?loc ?attr l =
  mk ?id ?loc ?attr (Pack l)

(* Push/Pop *)
let pop ?loc i = mk ?loc (Pop i)
let push ?loc i = mk ?loc (Push i)
let reset_assertions ?loc () = mk ?loc Reset_assertions

(* Assumptions and fact checking *)
let prove ?loc () = mk ?loc (Prove [])
let mk_clause ?loc ?attr l = mk ?loc ?attr (Clause l)
let consequent ?loc ?attr t = mk ?loc ?attr (Consequent t)
let antecedent ?loc ?attr t = mk ?loc ?attr (Antecedent t)

(* Options statements *)
let set_logic ?loc s = mk ?loc (Set_logic s)

let get_info ?loc s = mk ?loc (Get_info s)
let set_info ?loc t = mk ?loc (Set_info t)

let get_option ?loc s = mk ?loc (Get_option s)
let set_option ?loc t = mk ?loc (Set_option t)

(* Definitions, i.e given identifier, with arguments,
   is equal to given term *)

(* Return values *)
let get_proof ?loc () = mk ?loc Get_proof
let get_unsat_core ?loc () = mk ?loc Get_unsat_core
let get_unsat_assumptions ?loc () = mk ?loc Get_unsat_assumptions
let get_model ?loc () = mk ?loc Get_model
let get_value ?loc l = mk ?loc (Get_value l)
let get_assignment ?loc () = mk ?loc Get_assignment
let get_assertions ?loc () = mk ?loc Get_assertions

(* Scripts statement *)
let echo ?loc s = mk ?loc (Echo s)
let reset ?loc () = mk ?loc Reset
let exit ?loc () = mk ?loc Exit



(* Dimacs&iCNF wrappers *)
let p_cnf ?loc nbvar nbclause =
  let i = Term.int ?loc (string_of_int nbvar) in
  let j = Term.int ?loc (string_of_int nbclause) in
  let attr = Term.colon ?loc i j in
  mk ?loc ~attr (Set_logic "dimacs")

let clause ?loc l = mk_clause ?loc l

let assumption ?loc l =
  mk ?loc (Prove l)

(* Smtlib wrappers *)
let check_sat ?loc l = mk ?loc (Prove l)
let assert_ ?loc t = antecedent ?loc t

let type_decl ?loc id n =
  let ty = Term.fun_ty ?loc (Misc.replicate n @@ Term.tType ()) @@ Term.tType () in
  mk ?loc (Decl (id, ty))

let fun_decl ?loc id l t' =
  let ty = Term.fun_ty ?loc l t' in
  mk ?loc (Decl (id, ty))

let type_def ?loc id args body =
  let l = List.map (fun id -> Term.colon (Term.const id) @@ Term.tType ()) args in
  let t = Term.lambda l body in
  mk ?loc (Def (id, t))

let datatypes ?loc l =
  let l' = List.map (fun (id, vars, cstrs) ->
      mk ?loc (Inductive {id; vars; cstrs; loc; })
    ) l in
  pack ?loc l'

let fun_def ?loc id args ty_ret body =
  let t = Term.lambda args (Term.colon body ty_ret) in
  mk ?loc (Def (id, t))

let funs_def_rec ?loc l =
  let l' = List.map (fun (id, args, ty_ret, body) ->
      fun_def ?loc id args ty_ret body
    ) l in
  pack ?loc l'

(* Wrappers for Zf *)
let zf_attr ?loc = function
  | None | Some [] -> None
  | Some l -> Some (Term.apply ?loc (Term.and_t ()) l)

let import ?loc s = mk ?loc (Include s)

let data ?loc ?attrs l =
  let attr = zf_attr ?loc attrs in
  mk ?loc ?attr (Pack l)

let defs ?loc ?attrs l =
  let attr = zf_attr ?loc attrs in
  mk ?loc ?attr (Pack l)

let rewrite ?loc ?attrs t =
  let attr = zf_attr ?loc attrs in
  antecedent ?loc ?attr (Term.add_attr (Term.const Id.rwrt_rule) t)

let goal ?loc ?attrs t =
  let attr = zf_attr ?loc attrs in
  mk ?loc ?attr (Pack [
      consequent ?loc t;
      prove ?loc ();
    ])

let assume ?loc ?attrs t =
  let attr = zf_attr ?loc attrs in
  antecedent ?loc ?attr t

let lemma ?loc ?attrs t =
  let attr = zf_attr ?loc attrs in
  antecedent ?loc ?attr t

let decl ?loc ?attrs id ty =
  let attr = zf_attr ?loc attrs in
  mk ?loc ?attr (Decl (id, ty))

let definition ?loc ?attrs s ty l =
  let attr = zf_attr ?loc attrs in
  mk ?loc ?attr (Pack (
      decl ?loc s ty ::
      List.map (assume ?loc) l
    ))

let inductive ?loc ?attrs id vars cstrs =
  let attr = zf_attr ?loc attrs in
  mk ?loc ?attr (Inductive {id; vars; cstrs; loc; })



(* Wrappers for tptp *)
let include_ ?loc s l =
  let attr = Term.apply ?loc (Term.and_t ()) (List.map Term.const l) in
  mk ?loc ~attr (Include s)

let tptp ?loc ?annot id role body =
  let aux t =
    match annot with
    | None -> t
    | Some t' -> Term.colon t t'
  in
  let attr = aux (Term.apply
                    (Term.const Id.tptp_role)
                    [Term.const Id.(mk Attr role)])
  in
  let descr = match role with
    | "axiom"
    | "hypothesis"
    | "definition"
    | "lemma"
    | "theorem"
    | "assumption"
    | "negated_conjecture" ->
      begin match body with
        | `Term t -> Antecedent t
        | `Clause (_, l) -> Clause l
      end
    | "conjecture" ->
      begin match body with
        | `Term t -> Consequent t
        | `Clause _ ->
          Format.eprintf "WARNING: conjecture in a cnf context";
          Pack []
      end
    | "type" ->
      begin match body with
        | `Term { Term.term = Term.Colon ({ Term.term = Term.Symbol s; _ }, ty ) ; _ } ->
          Decl (s, ty)
        | _ ->
          Format.eprintf "WARNING: unexpected type declaration@.";
          Pack []
      end
    | "plain" ->
      begin match body with
        | `Term t | `Clause (t, _) -> Plain t
      end
    | "unknown"
    | "fi_domain"
    | "fi_functors"
    | "fi_predicates" -> Pack []
    | _ ->
      Format.eprintf "WARNING: unknown tptp formula role: '%s'@." role;
      Pack []
  in
  mk ~id ?loc ~attr descr

let tpi ?loc ?annot id role t = tptp ?loc ?annot id role (`Term t)
let thf ?loc ?annot id role t = tptp ?loc ?annot id role (`Term t)
let tff ?loc ?annot id role t = tptp ?loc ?annot id role (`Term t)
let fof ?loc ?annot id role t = tptp ?loc ?annot id role (`Term t)

let cnf ?loc ?annot id role t =
  let l =
    match t with
    | { Term.term = Term.App
            ({ Term.term = Term.Builtin Term.Or; _ }, l); _ } -> l
    | _ -> [t]
  in
  tptp ?loc ?annot id role (`Clause (t, l))

(* normalization *)
let normalize_inductive f i =
  { i with cstrs = List.map (fun (x, l) -> (x, List.map f l)) i.cstrs; }

let rec normalize_descr f = function
  | Pack l -> Pack (List.map (normalize f) l)

  | Plain t -> Plain (f t)

  | Prove l -> Prove (List.map f l)
  | Clause l -> Clause (List.map f l)
  | Antecedent t -> Antecedent (f t)
  | Consequent t -> Consequent (f t)

  | Set_info t -> Set_info (f t)

  | Set_option t -> Set_option (f t)

  | Def (id, t) -> Def (id, f t)
  | Decl (id, t) -> Decl (id, f t)
  | Inductive i -> Inductive (normalize_inductive f i)

  | Get_value l -> Get_value (List.map f l)

  | descr -> descr

and normalize f s =
  { s with
    attr = (match s.attr with
        | None -> None | Some t -> Some (f t));
    descr = normalize_descr f s.descr; }


