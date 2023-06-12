
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(* Type definitions *)
type term = Term.t
type location = Loc.t

type abstract = {
  id : Id.t;
  ty : term;
  loc : location;
  attrs : term list;
}

type inductive = {
  id : Id.t;
  vars : term list;
  cstrs : (Id.t * term list) list;
  loc : location;
  attrs : term list;
}

type record = {
  id : Id.t;
  vars : term list;
  fields : (Id.t * term) list;
  loc : location;
  attrs : term list;
}

type decl =
  | Abstract of abstract
  | Record of record
  | Inductive of inductive

type def = {
  id : Id.t;
  vars   : term list;
  params : term list;
  ret_ty : term;
  body   : term;
  loc : location;
  attrs : term list;
}

type 'a group = {
  contents : 'a list;
  recursive : bool;
}

type defs = def group
type decls = decl group

type local = {
  hyps: term list;
  goals: term list
}

(* Description of statements. *)
type descr =
  | Pack of t list

  | Pop of int
  | Push of int
  | Reset_assertions

  | Plain of term

  | Prove of local
  | Clause of term list
  | Antecedent of term
  | Consequent of term

  | Include of string
  | Set_logic of string

  | Get_info of string
  | Set_info of term

  | Get_option of string
  | Set_option of term

  | Defs of def group
  | Decls of decl group

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
  id : Id.t option;
  descr : descr;
  attrs : term list;
  loc : location;
}

(* Dummy location *)

let no_loc = Loc.no_loc

(*
(* Debug printing *)

let pp_abstract b (i : abstract) =
  Printf.bprintf b "Abstract %a: %a\n" Id.pp i.id Term.pp i.ty

let pp_inductive b (i : inductive) =
  Printf.bprintf b "Inductive(%d): %a, %a\n"
    (List.length i.cstrs) Id.pp i.id
    (Misc.pp_list ~pp_sep:Buffer.add_string ~sep:" " ~pp:Term.pp) i.vars;
  Misc.pp_list ~pp_sep:Buffer.add_string ~sep:"\n"
    ~pp:(fun b (cstr, l) -> Printf.bprintf b "%a: %a"
            Id.pp cstr
            (Misc.pp_list ~pp_sep:Buffer.add_string ~sep:" " ~pp:Term.pp) l
        ) b i.cstrs

let pp_record b (i : record) =
  Printf.bprintf b "Record: %a, %a:\n  { %a}\n" Id.pp i.id
    (Misc.pp_list ~pp_sep:Buffer.add_string ~sep:" " ~pp:Term.pp) i.vars
    (Misc.pp_list ~pp_sep:Buffer.add_string ~sep:";\n"
       ~pp:(fun b (id, ty) ->
           Printf.bprintf b "%a: %a" Id.pp id Term.pp ty
         )) i.fields

let pp_decl b = function
  | Abstract a -> pp_abstract b a
  | Record r -> pp_record b r
  | Inductive i -> pp_inductive b i

let pp_def b (d : def) =
  Printf.bprintf b "def: %a = %a" Id.pp d.id Term.pp d.body

let pp_group pp b (d: _ group) =
  let aux = Misc.pp_list ~pp_sep:Buffer.add_string ~sep:"\n" ~pp in
  if d.recursive then
    Printf.bprintf b "rec (\n%a)" aux d.contents
  else
    aux b d.contents

let rec pp_descr b = function
  | Pack l ->
    Printf.bprintf b "pack(%d):\n" (List.length l);
    Misc.pp_list ~pp_sep:Buffer.add_char ~sep:'\n' ~pp b l

  | Pop i -> Printf.bprintf b "pop: %d" i
  | Push i -> Printf.bprintf b "push: %d" i
  | Reset_assertions -> Printf.bprintf b "reset assertions"

  | Plain t -> Printf.bprintf b "plain: %a" Term.pp t

  | Prove [] -> Printf.bprintf b "Prove"
  | Prove { hyps; goals } ->
    Printf.bprintf b "Prove: %a, assuming: %a"
      (Misc.pp_list ~pp_sep:Buffer.add_string ~sep:" || " ~pp:Term.pp) goals
      (Misc.pp_list ~pp_sep:Buffer.add_string ~sep:" && " ~pp:Term.pp) hyps

  | Clause l ->
    Printf.bprintf b "clause: %a"
      (Misc.pp_list ~pp_sep:Buffer.add_string ~sep:" || " ~pp:Term.pp) l
  | Antecedent t -> Printf.bprintf b "antecedent: %a" Term.pp t
  | Consequent t -> Printf.bprintf b "consequent: %a" Term.pp t

  | Include f -> Printf.bprintf b "include: %s" f
  | Set_logic s -> Printf.bprintf b "set-logic: %s" s

  | Get_info s -> Printf.bprintf b "get-info: %s" s
  | Set_info t -> Printf.bprintf b "set-info: %a" Term.pp t

  | Get_option s -> Printf.bprintf b "get-option: %s" s
  | Set_option t -> Printf.bprintf b "set-option: %a" Term.pp t

  | Defs p -> pp_group pp_def b p
  | Decls p -> pp_group pp_decl b p

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
*)

(* Pretty printing *)

let print_attrs fmt = function
  | [] -> ()
  | l ->
    Format.fprintf fmt "@[<hov>{ %a }@]@ "
      (Format.pp_print_list Term.print) l

let print_abstract fmt ({ id; loc = _; attrs; ty; } : abstract) =
  Format.fprintf fmt "@[<hov 2>abstract%a:@ %a :@ %a@]"
    print_attrs attrs Id.print id Term.print ty

let print_inductive fmt ({ id; loc = _; attrs; vars; cstrs; } : inductive) =
  Format.fprintf fmt "@[<hv 2>Inductive(%d)%a@ %a(@[<hov>%a@]) =@ %a@]"
    (List.length cstrs) print_attrs attrs Id.print id
    (Misc.print_list ~print_sep:Format.fprintf ~sep:",@ " ~print:Term.print) vars
    (Misc.print_list ~print_sep:Format.fprintf ~sep:"@ "
       ~print:(fun fmt (cstr, l) ->
           Format.fprintf fmt "| %a : @[<hov>%a@]" Id.print cstr (
             Misc.print_list ~print_sep:Format.fprintf ~sep:"@ " ~print:Term.print
           ) l)) cstrs

let print_record fmt ({ id; loc = _; attrs; vars; fields; } : record) =
  Format.fprintf fmt "@[<hv 2>Record%a@ %a(%a) = {@ %a}@]"
    print_attrs attrs Id.print id
    (Misc.print_list ~print_sep:Format.fprintf ~sep:",@ " ~print:Term.print) vars
    (Misc.print_list ~print_sep:Format.fprintf ~sep:";@ " ~print:(fun fmt (f, ty) ->
         Format.fprintf fmt "%a : %a" Id.print f Term.print ty
       )) fields

let print_decl fmt = function
  | Abstract a -> print_abstract fmt a
  | Record r -> print_record fmt r
  | Inductive i -> print_inductive fmt i

let print_def fmt ({ id; loc = _; attrs; vars; params; body; ret_ty = _; } : def) =
  match vars @ params with
  | [] ->
    Format.fprintf fmt "@[<hov 2>def:@ %a@ %a =@ %a@]"
      print_attrs attrs
      Id.print id
      Term.print body
  | l ->
    Format.fprintf fmt "@[<hov 2>def:@ %a@ %a(%a) =@ %a@]"
      print_attrs attrs
      Id.print id
      (Misc.print_list ~print_sep:Format.fprintf ~sep:",@ " ~print:Term.print) l
      Term.print body

let print_group print fmt ({ contents; recursive; } : _ group) =
  let aux = Misc.print_list ~print_sep:Format.fprintf ~sep:"@ " ~print in
  if recursive then
    Format.fprintf fmt "@[<v 2>rec@ %a@]" aux contents
  else
    aux fmt contents

let rec print_descr fmt = function
  | Pack l ->
    Format.fprintf fmt "@[<hov 2>pack(%d):@ %a@]" (List.length l)
    (Misc.print_list ~print_sep:Format.fprintf ~sep:"@ " ~print) l

  | Pop i -> Format.fprintf fmt "pop: %d" i
  | Push i -> Format.fprintf fmt "push: %d" i
  | Reset_assertions -> Format.fprintf fmt "reset assertions"

  | Plain t -> Format.fprintf fmt "@[<hov 2>plain: %a@]" Term.print t

  | Prove { hyps = []; goals = [] } -> Format.fprintf fmt "prove"
  | Prove { hyps = []; goals } ->
      Format.fprintf fmt "@[<hov 2>prove:@ %a@]"
        (Misc.print_list ~print_sep:Format.fprintf ~sep:" ||@ " ~print:Term.print) goals
  | Prove { hyps; goals = [] } ->
    Format.fprintf fmt "@[<hov 2>prove-assuming:@ %a@]"
      (Misc.print_list ~print_sep:Format.fprintf ~sep:" &&@ " ~print:Term.print) hyps
  | Prove { hyps; goals } ->
    Format.fprintf fmt "@[<hov 2>prove:@ %a@ assuming:@ %a@]"
      (Misc.print_list ~print_sep:Format.fprintf ~sep:" ||@ " ~print:Term.print) goals
      (Misc.print_list ~print_sep:Format.fprintf ~sep:" &&@ " ~print:Term.print) hyps

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

  | Defs d -> print_group print_def fmt d
  | Decls d -> print_group print_decl fmt d

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

and print fmt = function { descr; attrs; _ } ->
  Format.fprintf fmt "%a%a" print_attrs attrs print_descr descr

(** Annotations *)
let annot = Term.apply

(* Internal shortcut. *)
let mk ?id ?(loc=Loc.no_loc) ?(attrs=[]) descr =
  { id; descr; loc; attrs; }

(* Pack *)
let pack ?id ?loc ?attrs l =
  mk ?id ?loc ?attrs (Pack l)

(* Push/Pop *)
let pop ?loc i = mk ?loc (Pop i)
let push ?loc i = mk ?loc (Push i)
let reset_assertions ?loc () = mk ?loc Reset_assertions

(* Assumptions and fact checking *)
let prove ?loc () = mk ?loc (Prove { hyps = []; goals = [] })
let mk_clause ?loc ?attrs l = mk ?loc ?attrs (Clause l)
let consequent ?loc ?attrs t = mk ?loc ?attrs (Consequent t)
let antecedent ?loc ?attrs t = mk ?loc ?attrs (Antecedent t)

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

(* decl/def *)
let group ~recursive contents =
  { recursive; contents; }

let def ?(loc=no_loc) id ~vars ~params ret_ty body =
  { id; loc; attrs = []; vars; params; ret_ty; body; }

let abstract ?(loc=no_loc) ?(attrs=[]) id ty =
  Abstract { loc; attrs; id; ty; }

let record ?(attrs=[]) ?(loc=no_loc) id vars fields =
  Record { id; vars; fields; loc; attrs; }

let inductive ?(attrs=[]) ?(loc=no_loc) id vars cstrs =
  Inductive { id; vars; cstrs; loc; attrs; }

(* grouping of decls/defs *)
let mk_decls ?loc ?attrs ~recursive decls =
  mk ?loc ?attrs (Decls { recursive; contents = decls; })

let group_decls ?loc ?attrs ~recursive l =
  let decls, others = List.fold_left (fun (decls, others) s ->
      match s with
      | { descr = Decls d; _ } ->
        List.rev_append d.contents decls, others
      | _ -> decls, s :: others
    ) ([], []) l in
  let new_decls = mk_decls ?loc ?attrs ~recursive (List.rev decls) in
  match others with
  | [] -> new_decls
  | l -> pack ?loc (new_decls :: List.rev l)

let mk_defs ?loc ?attrs ~recursive defs =
  mk ?loc ?attrs (Defs { recursive; contents = defs; })

let group_defs ?loc ?attrs ~recursive l =
  let defs, others = List.fold_left (fun (defs, others) s ->
      match s with
      | { descr = Defs d; _ } ->
        List.rev_append d.contents defs, others
      | _ -> defs, s :: others
    ) ([], []) l in
  let new_defs = mk_defs ?loc ?attrs ~recursive (List.rev defs) in
  match others with
  | [] -> new_defs
  | l -> pack ?loc (new_defs :: List.rev l)


(* Alt-ergo wrappers *)
let logic ?loc ~ac ids ty =
  let attrs = if ac then [Term.const ?loc Id.ac_symbol] else [] in
  let ty = match Term.fv ty with
    | [] -> ty
    | vars ->
      let l = List.map (fun x ->
          Term.colon ?loc (Term.const ?loc x) (Term.tType ?loc ())
        ) vars in
      Term.pi ?loc l ty
  in
  let l = List.map (fun id -> abstract ?loc id ty) ids in
  mk_decls ?loc ~attrs ~recursive:true l

let abstract_type ?loc id vars =
  let ty = Term.pi ?loc vars (Term.tType ?loc ()) in
  mk_decls ?loc ~recursive:false [abstract ?loc id ty]

let record_type ?loc id vars fields =
  mk_decls ?loc ~recursive:false [ record ?loc id vars fields]

let algebraic_type ?loc id vars cstrs =
  mk_decls ?loc ~recursive:false [inductive ?loc id vars cstrs]

let rec_types ?loc l =
  group_decls ?loc ~recursive:true l

let axiom ?loc id t =
  mk ~id ?loc (Antecedent t)

let case_split ?loc id t =
  let attrs = [Term.const ?loc Id.case_split] in
  mk ~id ?loc ~attrs (Antecedent t)

let prove_goal ?loc id t =
  mk ~id ?loc (Prove { hyps = []; goals = [t]; })

let prove_sat ?loc ~name hyps =
  mk ?loc ~id:name (Prove { hyps; goals = []; })

let rewriting ?loc id l =
  mk ~id ?loc @@ Pack (List.map (fun t ->
      antecedent ?loc (Term.add_attr (Term.const Id.rwrt_rule) t)
    ) l)

let theory ?loc id extends l =
  let attrs = [
    Term.colon ?loc (Term.const ?loc Id.theory_decl)
      (Term.colon ?loc (Term.const ?loc id) (Term.const ?loc extends))
  ] in
  mk ?loc ~attrs (Pack l)

(* Dimacs&iCNF wrappers *)
let p_cnf ?loc nbvar nbclause =
  let i = Term.int ?loc (string_of_int nbvar) in
  let j = Term.int ?loc (string_of_int nbclause) in
  let attrs = [Term.colon ?loc i j] in
  mk ?loc ~attrs (Set_logic "dimacs")

let p_inccnf ?loc () =
  mk ?loc (Set_logic "icnf")

let clause ?loc l = mk_clause ?loc l

let assumption ?loc l =
  mk ?loc (Prove { hyps = l; goals = []; })

(* Smtlib wrappers *)
let assert_ ?loc t = antecedent ?loc t

let check_sat ?loc hyps =
  mk ?loc (Prove { hyps; goals = []; })

let type_decl ?loc id n =
  let ty = Term.fun_ty ?loc (Misc.replicate n @@ Term.tType ()) @@ Term.tType () in
  mk_decls ?loc ~recursive:false [abstract ?loc id ty]

let fun_decl ?loc id vars l t' =
  let ty = Term.fun_ty ?loc l t' in
  let ty = match vars with
    | [] -> ty
    | vars -> Term.pi ?loc vars ty  in
  mk_decls ?loc ~recursive:false [abstract ?loc id ty]

let type_def ?loc id vars body =
  let vars = List.map (fun id -> Term.colon (Term.const id) @@ Term.tType ()) vars in
  let ret_ty = Term.tType ?loc () in
  mk_defs ?loc ~recursive:false [def ?loc id ~vars ~params:[] ret_ty body]

let datatypes ?loc l =
  let l' = List.map (fun (id, vars, cstrs) ->
      inductive ?loc id vars cstrs
    ) l in
  mk_decls ?loc ~recursive:true l'

let fun_def ?loc id vars params ret_ty body =
  mk_defs ?loc ~recursive:false [
    def ?loc id ~vars ~params ret_ty body
  ]

let pred_def ?(loc=no_loc) id vars params body =
  let attrs = [Term.const ~loc Id.predicate_def] in
  let ret_ty = Term.prop ~loc () in
  mk_defs ~loc ~recursive:false [
    { loc; attrs; id; vars; params; ret_ty; body; }
  ]

let funs_def_rec ?loc l =
  let contents = List.map (fun (id, vars, params, ret_ty, body) ->
      def ?loc id ~vars ~params ret_ty body
    ) l in
  mk_defs ?loc ~recursive:true contents


(* Wrappers for Zf *)
let import ?loc s = mk ?loc (Include s)

let defs ?loc ?attrs l =
  group_defs ?loc ?attrs ~recursive:true l

let rewrite ?loc ?attrs t =
  antecedent ?loc ?attrs (Term.add_attr (Term.const Id.rwrt_rule) t)

let goal ?loc ?attrs t =
  mk ?loc ?attrs (Pack [
      consequent ?loc t;
      prove ?loc ();
    ])

let assume ?loc ?attrs t =
  antecedent ?loc ?attrs t

let lemma ?loc ?attrs t =
  antecedent ?loc ?attrs t

let decl ?loc ?attrs id ty =
  mk_decls ?loc ?attrs ~recursive:true [abstract ?loc id ty]

let definition ?loc ?attrs s ty l =
  mk ?loc ?attrs (Pack (
      decl ?loc s ty ::
      List.map (assume ?loc) l
    ))

let inductive ?loc ?attrs id vars cstrs =
  mk_decls ?loc ~recursive:true [inductive ?loc ?attrs id vars cstrs]

let data ?loc ?attrs l =
  (* this is currently only used for mutually recursive datatypes *)
  group_decls ?loc ?attrs ~recursive:true l


(* Wrappers for tptp *)
let include_ ?loc s l =
  let attrs = List.map Term.const l in
  mk ?loc ~attrs (Include s)

let tptp ?loc ?annot kind id role body =
  let attrs =
    Term.apply (Term.const Id.tptp_role) [Term.const Id.(mk Attr role)] ::
    Term.apply (Term.const Id.tptp_kind) [Term.const Id.(mk Attr kind)] ::
    match annot with
    | None -> [] | Some t -> [t]
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
          (* Format.eprintf "WARNING: conjecture in a cnf context"; *)
          Pack []
      end
    | "type" ->
      begin match body with
        | `Term { Term.term = Term.Colon ({ Term.term = Term.Symbol s; _ }, ty ) ; _ } ->
          Decls { recursive = false;
                  contents = [abstract ?loc s ty]; }
        | _ ->
          (* Format.eprintf "WARNING: unexpected type declaration@."; *)
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
      (* Format.eprintf "WARNING: unknown tptp formula role: '%s'@." role; *)
      Pack []
  in
  mk ~id ?loc ~attrs descr

let tpi ?loc ?annot id role t = tptp ?loc ?annot "tpi" id role (`Term t)
let thf ?loc ?annot id role t = tptp ?loc ?annot "thf" id role (`Term t)
let tff ?loc ?annot id role t = tptp ?loc ?annot "tff" id role (`Term t)
let fof ?loc ?annot id role t = tptp ?loc ?annot "fof" id role (`Term t)

let cnf ?loc ?annot id role t =
  let l =
    match t with
    | { Term.term = Term.App
            ({ Term.term = Term.Builtin Term.Or; _ }, l); _ } -> l
    | _ -> [t]
  in
  tptp ?loc ?annot "cnf" id role (`Clause (t, l))


