(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

module Id = Dolmen.Std.Id
module Ast = Dolmen.Std.Term
module Stmt = Dolmen.Std.Statement

module M = Map.Make(Dolmen.Std.Id)
module S = Set.Make(Dolmen.Std.Id)

(* MCIL transition systems *)
(* ************************************************************************ *)

module MCIL (Type : Tff_intf.S) = struct
  
  type _ Type.err +=
    Cannot_find_system : Dolmen.Std.Id.t -> Dolmen.Std.Loc.t Type.err

  let key = Dolmen.Std.Tag.create ()

  let get_defs env =
    match Type.get_global_custom env key with
    | None -> M.empty
    | Some m -> m

  let define_sys id ((env, _), input, output, local) =
    let map = get_defs env in
    let m = M.add id (`Trans_Sys (input, output, local)) map in
    Type.set_global_custom env key m

  let create_primed_id id =
    match (Id.name id) with
    | Simple name ->
      Id.create (Id.ns id) (Dolmen_std.Name.simple (name ^ "'") )
    | _ -> assert false

  let parse_def_params (env, env') params =
    let rec aux env env' acc = function
      | [] -> (env, env'), List.rev acc
      | p :: r ->
        let id, v, ast = Type.parse_typed_var_in_binding_pos env p in
        let id' = create_primed_id id in
        let env = Type.add_term_var env id v ast in
        let env' = Type.add_term_var env' id v ast in
        let env' = Type.add_term_var env' id' v ast in
        aux env env' ((id, v, ast) :: acc) r
    in
    aux env env' [] params

  let parse_sig env input output local =
    let envs = env, env in
    let envs, parsed_input  = parse_def_params envs input in
    let envs, parsed_output = parse_def_params envs output in
    let envs, parsed_local  = parse_def_params envs local in
    envs, parsed_input, parsed_output, parsed_local

  let parse_condition env cond =
    Type.parse_prop env cond |> ignore

  let _cannot_find_system env loc id =
    Type._error env (Located loc) (Cannot_find_system id)

  let ensure env ast t ty =
    Type._wrap2 env ast Type.T.ensure t ty

  let parse_ensure env ast ty =
    let t = Type.parse_term env ast in
    ensure env ast t ty

  let vars l = List.map (fun (_, v, _) -> v) l

  let _bad_arity env s n m t =
    Type._error env (Ast t) (Type.Bad_op_arity (s, [n], m))

  let parse_subsystems env (parent : Stmt.sys_def) =
    let defs = get_defs env in
    List.fold_left
      (fun other_subs (local_name, sub_name, args) ->
        (* Make sure local name isn't used twice *)
        if S.mem local_name other_subs then
          (* TODO: add proper error *)
          let msg =
            Format.asprintf 
              "Subsystem with local name `%a` is already declared"
                Id.print local_name
          in
          failwith msg
        else (
          let sub_inputs, sub_outputs =
            match M.find_opt sub_name defs with
            | None ->
                _cannot_find_system env parent.loc sub_name
            | Some (`Trans_Sys (input, output, _)) ->
                (vars input, vars output)
          in
          let num_args = List.length args in
          let params = sub_inputs @ sub_outputs in
          let num_params = List.length params in
          if (num_args != num_params) then (
            (* TODO: add proper error *)
            let msg =
              Format.asprintf 
                "Bad arity for instance `%a` of system `%a`: expected %d arguments but got %d"
                Id.print local_name Id.print sub_name num_params num_args
            in
            failwith msg
          ) ;
          List.iter2
            (fun arg param ->
              let expected_type = Type.T.Var.ty param in
              parse_ensure env arg expected_type |> ignore
            )
            args
            params ;
          S.add local_name other_subs
        )
      )
      S.empty
      parent.subs
    |> ignore

  let parse_def_body ((env, env'), _input, _output, _local) (d: Stmt.sys_def) =
    parse_condition env d.init ;
    parse_condition env' d.trans ; 
    parse_condition env d.inv ;
    parse_subsystems env d

  let finalize_sys (d : Stmt.sys_def) ((env, _), input, output, local) =
    Type.check_no_free_wildcards env d.init;
    Type.check_no_free_wildcards env d.trans;
    Type.check_no_free_wildcards env d.inv;
    let input, output, local = vars input, vars output, vars local in
    (* TODO: review cases of unused variable *)
    List.iter (Type.check_used_term_var ~kind:`Trans_sys_param env) input ;
    List.iter (Type.check_used_term_var ~kind:`Trans_sys_param env) output ;
    List.iter (Type.check_used_term_var ~kind:`Trans_sys_param env) local ;
    `Sys_def (d.id, input, output, local)

  let parse_def env (d : Stmt.sys_def) =
    let ssig = parse_sig env d.input d.output d.local in
    parse_def_body ssig d ;
    define_sys d.id ssig ;
    finalize_sys d ssig

  let get_sys_sig env loc id =
    let defs = get_defs env in
    match M.find_opt id defs with
    | None -> _cannot_find_system env loc id
    | Some (`Trans_Sys ssig) -> ssig

  let check_sig (env, env') id sys chk =
    match chk with
    | [] -> (
      List.fold_left
        (fun (env, env') (id, v, ast) ->
          let id' = create_primed_id id in
          let env = Type.add_term_var env id v ast in
          let env' = Type.add_term_var env' id v ast in
          let env' = Type.add_term_var env' id' v ast in
          env, env'
        )
        (env, env')
        sys
    )
    | (_, _, a) :: _ -> (
      let n1 = List.length sys in
      let n2 = List.length chk in
      if (n1 != n2) then _bad_arity env (Id id) n1 n2 a ;
      List.iter2
        (fun (_, v1, _) (_, v2, a2) ->
          ensure env a2 (Type.T.of_var v2) (Type.T.Var.ty v1)
          |> ignore
        )
        sys
        chk ;
      env, env'
    )

  let parse_check_sig env (c : Stmt.sys_check) =
    let sys_input, sys_output, sys_local = get_sys_sig env c.loc c.id in
    let (envs, input, output, local) =
      parse_sig env c.input c.output c.local
    in
    let envs = check_sig envs c.id sys_input input in
    let envs = check_sig envs c.id sys_output output in
    let envs = check_sig envs c.id sys_local local in
    envs

  let parse_conditions env ids conds =
    List.fold_left
      (fun acc (id, f) -> 
        if S.mem id acc then
          (* TODO: add proper error *)
          let msg =
            Format.asprintf 
              "Duplicate declaration of `%a`"
                Id.print id
          in
          failwith msg
        else
          parse_condition env f ; S.add id acc
      )
      ids
      conds

  let parse_assumptions_and_conditions (_, env') (c : Stmt.sys_check) =
    let cids = parse_conditions env' S.empty c.assumption in
    let cids = parse_conditions env' cids c.reachable in
    cids

  let parse_queries cond_ids (c : Stmt.sys_check) =
    let parse_query query_ids (id, conds) =
      if S.mem id query_ids then
        (* TODO: add proper error *)
        let msg =
          Format.asprintf 
            "Duplicate declaration of `%a`"
              Id.print id
        in
        failwith msg
      else
        conds |> List.iter (function
        | { Ast.term = Ast.Symbol s; _ } -> (
          if not (S.mem s cond_ids) then
            (* TODO: add proper error *)
            let msg =
              Format.asprintf
                "Condition `%a` referenced in query `%a` is not defined"
                Id.print s Id.print id
            in
            failwith msg;
        )
        | _ -> assert false
        ) ;
        S.add id query_ids
    in
    List.fold_left parse_query S.empty c.queries |> ignore

  let parse_check env (c : Stmt.sys_check) =
    let envs = parse_check_sig env c in
    let cids = parse_assumptions_and_conditions envs c in
    parse_queries cids c ;
    `Sys_check

end