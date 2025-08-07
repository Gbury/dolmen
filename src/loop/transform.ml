
(* This file is free software, part of Dolmen. See file "LICENSE" for more details. *)

(* Statement transformation / normalisation

   This module defines a pipe to apply transformations to terms and statements
*)

(* Transforming errors *)
(* ************************************************************************ *)

type language = Logic.language

let code = Code.create
    ~category:"transform"
    ~descr:"on trasnformation errors"

let unsupported_language =
  Report.Error.mk ~code ~mnemonic:"transform-unsupported-lang"
    ~message:(fun fmt lang ->
        Format.fprintf fmt
          "The following format is not (yet) supported for transform: %s"
          (Logic.string_of_language lang)
      )
    ~name:"Unsupported Transform Language" ()

let non_minimal_logic =
  Report.Warning.mk ~code ~mnemonic:"non-minimal-logic"
    ~message:(fun fmt (old_logic, new_logic) ->
        Format.fprintf fmt
          "The logic used to typecheck the problem (%s) is not minimal. \
           The minimal logic actually is: %s" old_logic new_logic
      )
    ~name:"Non Minimal Logic" ()

let non_handled_builtin =
  Report.Error.mk ~code ~mnemonic:"transform-unhandled-builtin"
    ~message:(fun fmt (lang, f) ->
        let aux pp arg =
          Format.fprintf fmt "%a %s:@ %a"
            Format.pp_print_text
            "The following symbol cannot be exported in"
            lang pp arg
        in
        match f with
        | `Ty c -> aux Dolmen_std.Expr.Ty.Const.print c
        | `Term c -> aux Dolmen_std.Expr.Term.Const.print c)
    ~name:"Unhandled Builtin" ()


(* Common interface for transformation *)
(* ************************************************************************ *)

module Dummy = struct

  type acc = unit

  let transform st () l = st, (), l

end

(* Smtlib2 transformation *)
(* ************************************************************************ *)

module Smtlib2
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
    | Logic of (string * Typer_Types.typechecked Typer_Types.stmt) option

  type acc = {
    seen_exit : bool;
    scan_acc : S.acc option;
    old_logic : old_logic;
    pre_logic_stmts : Typer_Types.typechecked Typer_Types.stmt list;
    post_logic_stmts : Typer_Types.typechecked Typer_Types.stmt list;
  }

  let init ~compute_logic =
    {
      seen_exit = false;
      scan_acc = if compute_logic then Some S.nothing else None;
      old_logic = Not_seen_yet;
      pre_logic_stmts = [];
      post_logic_stmts = [];
    }

  let need_logic = function
    | Not_seen_yet -> Logic None
    | Logic _ as l -> l

  let compute_logic st acc =
    match acc.scan_acc with
    | None ->
      begin match acc.old_logic with
        | Not_seen_yet | Logic None -> st, []
        | Logic Some (_, stmt) -> st, [stmt]
      end
    | Some scan_acc ->
      let logic = S.to_logic scan_acc in
      let logic_name = Dolmen_type.Logic.Smtlib2.to_string logic in
      let st =
        match acc.old_logic with
        | Logic Some (old_logic, _stmt) when old_logic <> logic_name ->
          State.warn st non_minimal_logic (old_logic, logic_name)
        | _ -> st
      in
      (* TODO: expose a function to create stmts in Typer_Types *)
      let set_logic : Typer_Types.typechecked Typer_Types.stmt = {
        id = Dolmen.Std.Id.create Attr (Dolmen.Std.Name.simple "set_logic");
        loc = Dolmen.Std.Loc.no_loc;
        attrs = [];
        implicit = false;
        contents = `Set_logic (logic_name, Smtlib2 logic);
      } in
      (* Accumulate top-level declaration *)
      let acc = [] in
      (* Unit type *)
      let acc =
        if not (S.need_unit scan_acc) then acc
        else begin
          let cst = Dolmen.Std.Expr.Ty.Const.unit in
          let declare_unit : Typer_Types.typechecked Typer_Types.stmt = {
            id = Dolmen.Std.Id.create Attr (Dolmen.Std.Name.simple "declare_unit");
            loc = Dolmen.Std.Loc.no_loc;
            attrs = [];
            implicit = false;
            contents = `Decls (false, [`Type_decl (cst,
                Dolmen.Std.Expr.Ty.definition cst
              )]);
          } in
          declare_unit :: acc
        end
      in
      (* Univ/tptp's $i type *)
      let acc =
        if not (S.need_univ scan_acc) then acc
        else begin
          let declare_univ : Typer_Types.typechecked Typer_Types.stmt = {
            id = Dolmen.Std.Id.create Attr (Dolmen.Std.Name.simple "declare_univ");
            loc = Dolmen.Std.Loc.no_loc;
            contents = `Decls (false, [`Type_decl (Dolmen.Std.Expr.Ty.Const.base, None)]);
            attrs = [];
            implicit = false;
          } in
          declare_univ :: acc
        end
      in
      st, set_logic :: acc

  let flush st acc res =
    let st, logic_stmts = compute_logic st acc in
    let res =
      res @
      List.rev_append acc.pre_logic_stmts (
        logic_stmts @ List.rev acc.post_logic_stmts)
    in
    let acc = { acc with pre_logic_stmts = []; post_logic_stmts = []; } in
    st, acc, res

  (** Try and simplify some series of statements. Note that the list here is
      in reverse order (so the first element is the last statement seen). *)
  let reduce_post_logic_stmts = function
    (* fallback *)
    | l -> l

  let add_post_logic_stmt acc stmt =
    let post_logic_stmts = stmt :: acc.post_logic_stmts in
    let post_logic_stmts = reduce_post_logic_stmts post_logic_stmts in
    { acc with post_logic_stmts; }

  let add_pre_logic_stmt acc stmt =
    { acc with pre_logic_stmts = stmt :: acc.pre_logic_stmts; }

  let add_stmt acc stmt =
    match acc.old_logic with
    | Not_seen_yet -> add_pre_logic_stmt acc stmt
    | Logic _ -> add_post_logic_stmt acc stmt

  let accumulate_logic_aux (st, acc, res) (stmt : Typer_Types.typechecked Typer_Types.stmt) =
    match stmt.contents with
    (* Just record the old logic, if any *)
    | `Set_logic (s, _) ->
      begin match acc.old_logic with
        | Not_seen_yet ->
          let acc = { acc with old_logic = Logic (Some (s, stmt)); } in
          st, acc, res
        | Logic _ ->
          (* TODO: proper error *)
          assert false
      end
    (* Not much to do for these statements *)
    | #Typer_Types.get_info
    | #Typer_Types.set_info
    | `Push _ | `Pop _ | `Reset_assertions (* `Reset must be handled separately *)
      ->
      let acc = add_stmt acc stmt in
      st, acc, res

    (* Decls & Defs *)
    | `Defs (_, l) ->
      let old_logic = need_logic acc.old_logic in
      let scan_acc =
        Option.map (fun scan_acc ->
            List.fold_left (fun scan_acc def ->
                match def with
                | `Type_alias _ -> scan_acc
                | `Term_def (_, _, _, _, body) -> S.scan_term scan_acc body
                | `Instanceof(_, _, _, _, _, body) -> S.scan_term scan_acc body
                (* TODO: in the Instanceof case, we might want to look at the term_cst
                     being instantiated to update the scan accumulator. *)
              ) scan_acc l
          ) acc.scan_acc
      in
      let acc = { acc with old_logic; scan_acc; } in
      st, add_stmt acc stmt, res
    | `Decls (_, l) ->
      let old_logic = need_logic acc.old_logic in
      let scan_acc =
        Option.map (fun scan_acc ->
            List.fold_left (fun scan_acc decl ->
                match decl with
                | `Implicit_type_var -> scan_acc
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
              ) scan_acc l
          ) acc.scan_acc
      in
      let acc = { acc with old_logic; scan_acc; } in
      st, add_stmt acc stmt, res

    (* Hyps & Goals *)
    | `Hyp f | `Goal f ->
      let old_logic = need_logic acc.old_logic in
      let scan_acc = Option.map (fun scan_acc -> S.scan_term scan_acc f) acc.scan_acc in
      let acc = { acc with old_logic; scan_acc; } in
      st, add_stmt acc stmt, res
    | `Clause l ->
      let old_logic = need_logic acc.old_logic in
      let scan_acc = Option.map (fun scan_acc -> List.fold_left S.scan_term scan_acc l) acc.scan_acc in
      let acc = { acc with old_logic; scan_acc; } in
      st, add_stmt acc stmt, res
    | `Solve (hyps, goals) ->
      let old_logic = need_logic acc.old_logic in
      let scan_acc = Option.map (fun scan_acc -> List.fold_left S.scan_term scan_acc hyps) acc.scan_acc in
      let scan_acc = Option.map (fun scan_acc -> List.fold_left S.scan_term scan_acc goals) scan_acc in
      let acc = { acc with old_logic; scan_acc; } in
      st, add_stmt acc stmt, res

    (* Exit, Reset and End trigger a flush of the statements and logic computed. *)
    | `Exit ->
      let acc = add_stmt acc stmt in
      let st, acc, res = flush st acc res in
      let acc = { acc with seen_exit = true; } in
      st, acc, res
    | `Reset ->
      let acc = add_stmt acc stmt in
      flush st acc res
    | `End ->
      if acc.seen_exit then begin
        assert (acc.pre_logic_stmts = [] &&
                acc.post_logic_stmts = []);
        st, acc, [stmt]
      end else begin
        let acc =
          let exit : _ Typer_Types.stmt = {
            id = Dolmen.Std.Id.create Attr (Dolmen.Std.Name.simple "exit");
            loc = Dolmen.Std.Loc.no_loc;
            contents = `Exit;
            attrs = [];
            implicit = false;
          } in
          add_post_logic_stmt acc exit
        in
        let acc = add_post_logic_stmt acc stmt in
        flush st acc res
      end

  let accumulate_logic ((st, acc, res) as arg) stmt =
    try accumulate_logic_aux arg stmt
    with
    | S.Unknown_ty_builtin f ->
      let st = State.error st non_handled_builtin ("SMT-LIB", `Ty f) in
      (st, acc, res)
    | S.Unknown_term_builtin f ->
      let st = State.error st non_handled_builtin ("SMT-LIB", `Term f) in
      (st, acc, res)

  let transform st acc (l : Typer_Types.typechecked Typer_Types.stmt list) =
    List.fold_left accumulate_logic (st, acc, []) l

end

(* Pipe functor *)
(* ************************************************************************ *)

module Make
    (State : State.S)
    (Typer_Types : Typer.Types
     with type ty = Dolmen.Std.Expr.ty
      and type ty_var = Dolmen.Std.Expr.ty_var
      and type ty_cst = Dolmen.Std.Expr.ty_cst
      and type ty_def = Dolmen.Std.Expr.ty_def
      and type term = Dolmen.Std.Expr.term
      and type term_var = Dolmen.Std.Expr.term_var
      and type term_cst = Dolmen.Std.Expr.term_cst
      and type formula = Dolmen.Std.Expr.term)
= struct


  (* Type definitions *)
  type stmt = Typer_Types.typechecked Typer_Types.stmt

  module type S = sig
    type acc
    val transform : State.t -> acc -> stmt list -> State.t * acc * stmt list
  end

  type 'acc transformer = (module S with type acc = 'acc)

  type state =
    | No_transform : state
    | Transform : {
        acc : 'acc;
        transformer : 'acc transformer;
      } -> state

  (* available transformers *)

  module Smt2 = Smtlib2(State)(Typer_Types)

  (* setup *)

  let pipe = "Transform"

  let state : state State.key =
    State.create_key ~pipe "transform_state"

  let init ?(compute_logic=false) ?lang st =
    let mk (type acc) (acc : acc) (transformer : acc transformer) =
      Transform { acc; transformer; }
    in
    let mk st lang =
      match (lang : language) with
      | Smtlib2 _ ->
        let acc = Smt2.init ~compute_logic in
        st, mk acc (module Smt2)
      | lang ->
        let st = State.error st unsupported_language lang in
        st, mk () (module Dummy)
    in
    let st, state_value =
      match lang with
      | Some lang ->
        mk st lang
      | None ->
        begin match State.get State.export_file st with
          | { lang = None; _ } -> st, No_transform
          | { lang = Some lang; _ } -> mk st lang
          | exception State.Key_not_found _ -> st, No_transform
        end
    in
    st
    |> State.set state state_value

  (* interface *)

  let transform st (l : Typer_Types.typechecked Typer_Types.stmt list) =
    match State.get state st with
    | No_transform -> st, l
    | Transform { acc; transformer; } ->
      let (module T) = transformer in
      let st, acc, l = T.transform st acc l in
      let st = State.set state (Transform { acc; transformer }) st in
      st, l

end
