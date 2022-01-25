
(* This file is free software, part of dolmen. See file "LICENSE" for more details. *)

module type Arg = Dolmen_intf.Id.Escape

module Make
    (Type_Var_Map : Dolmen_intf.Map.S)
    (Type_Cst_Map : Dolmen_intf.Map.S)
    (Term_Var_Map : Dolmen_intf.Map.S)
    (Term_Cst_Map : Dolmen_intf.Map.S)
= struct

  type type_var = Type_Var_Map.key
  type type_cst = Type_Cst_Map.key
  type term_var = Term_Var_Map.key
  type term_cst = Term_Cst_Map.key

  type id = [
    | `Type_var of type_var
    | `Type_cst of type_cst
    | `Term_var of term_var
    | `Term_cst of term_cst
  ]

  type conf = {
    sanitize : Name.t -> Name.t;
    rename : Name.t list -> Name.t;
    type_var_name : type_var -> Name.t;
    type_cst_name : type_cst -> Name.t;
    term_var_name : term_var -> Name.t;
    term_cst_name : term_cst -> Name.t;
  }

  type t = {
    conf : conf;
    in_scope : id Name.Map.t;
    type_vars : Name.t Type_Var_Map.t;
    type_csts : Name.t Type_Cst_Map.t;
    term_vars : Name.t Term_Var_Map.t;
    term_csts : Name.t Term_Cst_Map.t;
  }

  let mk
      ~type_var_name ~type_cst_name
      ~term_var_name ~term_cst_name
      ~sanitize ~rename =
    let conf = {
      sanitize; rename;
      type_var_name; type_cst_name;
      term_var_name; term_cst_name;
    } in
    let in_scope = Name.Map.empty in
    let type_vars = Type_Var_Map.empty in
    let type_csts = Type_Cst_Map.empty in
    let term_vars = Term_Var_Map.empty in
    let term_csts = Term_Cst_Map.empty in
    { conf; in_scope; type_vars; type_csts; term_vars; term_csts; }

  let find_opt t id =
    match id with
    | `Type_var v -> Type_Var_Map.find_opt v t.type_vars
    | `Type_cst c -> Type_Cst_Map.find_opt c t.type_csts
    | `Term_var v -> Term_Var_Map.find_opt v t.term_vars
    | `Term_cst c -> Term_Cst_Map.find_opt c t.term_csts

  

  let rec add t id =
    match find_opt t id with
    | None -> add_or_replace t id

    | Some name ->
      begin match Name.Map.find_opt name t.in_scope with
        | None -> assert false (* internal assumption broken *)
        | Some prev_id ->
          assert false (* TODO: call t.conf.on_conflict *)
      end


end
