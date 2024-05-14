
(* This file is free software, part of Dolmen. See file "LICENSE" for more details. *)

(* Statement transformation / normalisation

   This module defines a pipe to apply transformations to terms and statements
*)

(* Type definitions *)
(* ************************************************************************ *)

module Make
    (State : State.S)
    (Typer_Types : Typer.Types
     with type ty = Dolmen_std.Expr.ty
      and type ty_var = Dolmen_std.Expr.ty_var
      and type ty_cst = Dolmen_std.Expr.ty_cst
      and type ty_def = Dolmen_std.Expr.ty_def
      and type term = Dolmen_std.Expr.term
      and type term_var = Dolmen_std.Expr.term_var
      and type term_cst = Dolmen_std.Expr.term_cst
      and type formula = Dolmen_std.Expr.term)
= struct

  let pipe = "Transform"

  module View = Dolmen_std.Expr.View.TFF
  module S = Dolmen_type.Logic.Smtlib2.Scan(View)

  type old_logic =
    | Not_seen_yet
    | Logic of string option

  type logic_acc = {
    seen_exit : bool;
    scan_acc : S.acc;
    old_logic : old_logic;
    pre_logic_stmts : Typer_Types.typechecked Typer_Types.stmt list;
    post_logic_stmts : Typer_Types.typechecked Typer_Types.stmt list;
  }

  type state =
    | No_transform
    | Transform of {
        logic_acc : logic_acc option;
      }

  let state : state State.key =
    State.create_key ~pipe "transform_state"

  let init ?(compute_logic=false) st =
    let state_value =
      if not compute_logic then No_transform
      else begin
        let logic_acc = Some {
            seen_exit = false;
            scan_acc = S.nothing;
            old_logic = Not_seen_yet;
            pre_logic_stmts = [];
            post_logic_stmts = [];
          } in
        Transform { logic_acc; }
      end
    in
    st
    |> State.set state state_value

  let need_logic = function
    | Not_seen_yet -> Logic None
    | Logic _ as l -> l

  let compute_logic acc =
    let logic = S.to_logic acc.scan_acc in
    let logic_name = Dolmen_type.Logic.Smtlib2.to_string logic in
    (* TODO: expose a function to create stmts in Typer_Types *)
    let set_logic : Typer_Types.typechecked Typer_Types.stmt = {
      id = Dolmen.Std.Id.create Attr (Dolmen.Std.Name.simple "set_logic");
      loc = Dolmen.Std.Loc.no_loc;
      contents = `Set_logic (logic_name, Smtlib2 logic);
      attrs = [];
      implicit = false;
    } in
    let univ =
      if not (S.need_univ acc.scan_acc) then []
      else begin
        let declare_univ : Typer_Types.typechecked Typer_Types.stmt = {
          id = Dolmen.Std.Id.create Attr (Dolmen.Std.Name.simple "declare_univ");
          loc = Dolmen.Std.Loc.no_loc;
          contents = `Decls (false, [`Type_decl (Dolmen.Std.Expr.Ty.Const.base, None)]);
          attrs = [];
          implicit = false;
        } in
        [ declare_univ ]
      end
    in
    set_logic :: univ

  let flush acc res tail =
    let logic_stmts = compute_logic acc in
    let res =
      res @
      List.rev_append acc.pre_logic_stmts (
        logic_stmts @ List.rev_append acc.post_logic_stmts tail)
    in
    let acc = { acc with pre_logic_stmts = []; post_logic_stmts = []; } in
    acc, res

  let accumulate_logic (acc, res) (stmt : Typer_Types.typechecked Typer_Types.stmt) =
    match stmt.contents with
    (* Just record the old logic, if any *)
    | `Set_logic (s, _) ->
      let acc = { acc with old_logic = Logic (Some s); } in
      acc, res
    (* Not much to do for these statements *)
    | #Typer_Types.get_info
    | #Typer_Types.set_info
    | `Push _ | `Pop _ | `Reset_assertions (* `Reset must be handled separately *)
      ->
      begin match acc.old_logic with
        | Not_seen_yet -> { acc with pre_logic_stmts = stmt :: acc.pre_logic_stmts; }, res
        | Logic _ -> { acc with post_logic_stmts = stmt :: acc.post_logic_stmts; }, res
      end

    (* Decls & Defs *)
    | `Defs (_, l) ->
      let old_logic = need_logic acc.old_logic in
      let scan_acc =
        List.fold_left (fun scan_acc def ->
          match def with
            | `Type_alias _ -> scan_acc
            | `Term_def (_, _, _, _, body) -> S.scan_term scan_acc body
            | `Instanceof(_, _, _, _, _, body) -> S.scan_term scan_acc body
            (* TODO: in the Instanceof case, we might want to look at the term_cst
                     being instantiated to update the scan accumulator. *)
          ) acc.scan_acc l
      in
      let post_logic_stmts = stmt :: acc.post_logic_stmts in
      let acc = { acc with old_logic; scan_acc; post_logic_stmts; } in
      acc, res
    | `Decls (_, l) ->
      let old_logic = need_logic acc.old_logic in
      let scan_acc =
        List.fold_left (fun scan_acc decl ->
            match decl with
            | `Term_decl c -> S.scan_term_decl ~in_adt:false scan_acc c
            | `Type_decl (_, ty_def) ->
              begin match (ty_def : Dolmen.Std.Expr.ty_def option) with
                | None | Some Abstract -> S.add_free_sort scan_acc
                | Some Adt { ty = _; record = _; cases; } ->
                  let scan_acc = S.add_datatypes scan_acc in
                  Array.fold_left (fun scan_acc (case : Dolmen.Std.Expr.ty_def_adt_case)->
                      S.scan_term_decl ~in_adt:true scan_acc case.cstr
                    ) scan_acc cases
              end
          ) acc.scan_acc l
      in
      let post_logic_stmts = stmt :: acc.post_logic_stmts in
      let acc = { acc with old_logic; scan_acc; post_logic_stmts; } in
      acc, res

    (* Hyps & Goals *)
    | `Hyp f | `Goal f ->
      let old_logic = need_logic acc.old_logic in
      let scan_acc = S.scan_term acc.scan_acc f in
      let post_logic_stmts = stmt :: acc.post_logic_stmts in
      let acc = { acc with old_logic; scan_acc; post_logic_stmts; } in
      acc, res
    | `Clause l ->
      let old_logic = need_logic acc.old_logic in
      let scan_acc = List.fold_left S.scan_term acc.scan_acc l in
      let post_logic_stmts = stmt :: acc.post_logic_stmts in
      let acc = { acc with old_logic; scan_acc; post_logic_stmts; } in
      acc, res
    | `Solve (hyps, goals) ->
      let old_logic = need_logic acc.old_logic in
      let scan_acc = List.fold_left S.scan_term acc.scan_acc hyps in
      let scan_acc = List.fold_left S.scan_term scan_acc goals in
      let post_logic_stmts = stmt :: acc.post_logic_stmts in
      let acc = { acc with old_logic; scan_acc; post_logic_stmts; } in
      acc, res

    (* Exit, Reset and End trigger a flush of the statements and logic computed. *)
    | `Exit ->
      let acc, res = flush acc res [stmt] in
      let acc = { acc with seen_exit = true; } in
      acc, res
    | `Reset ->
      flush acc res [stmt]
    | `End ->
      if acc.seen_exit then begin
        assert (acc.pre_logic_stmts = [] &&
                acc.post_logic_stmts = []);
        acc, [stmt]
      end else begin
        let exit : _ Typer_Types.stmt = {
          id = Dolmen.Std.Id.create Attr (Dolmen.Std.Name.simple "exit");
          loc = Dolmen.Std.Loc.no_loc;
          contents = `Exit;
          attrs = [];
          implicit = false;
        } in
        flush acc res ([exit ; stmt])
      end


  let transform st (l : Typer_Types.typechecked Typer_Types.stmt list) =
    match State.get state st with
    | No_transform -> st, l
    | Transform { logic_acc; } ->
      let logic_acc, res =
        match logic_acc with
        | None -> None, l
        | Some acc ->
          let acc, res = List.fold_left accumulate_logic (acc, []) l in
          Some acc, res
      in
      let st = State.set state (Transform { logic_acc; }) st in
      st, res

end
