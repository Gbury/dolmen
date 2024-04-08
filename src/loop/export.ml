
(* This file is free software, part of Dolmen. See file "LICENSE" for more details. *)

(* File Export

   This module defines a pipe to print/export/output typed terms.
*)

(* Type definitions *)
(* ************************************************************************ *)

type language = Logic.language

type output = [
  | `Stdout
  | `File of string
]

type file = {
  lang : language option;
  output : output;
}

let enum = Logic.enum

(* Renaming *)
(* ************************************************************************ *)

let rename_num_postfix n name =
  match (name : Dolmen_std.Name.t) with
  | Simple s ->
    let s' = Format.asprintf "%s_%d" s n in
    let name' = Dolmen_std.Name.simple s' in
    n + 1, name'
  | Indexed _
  | Qualified _ ->
    (* TODO: proper error *)
    assert false


(* Scope and printing environment *)
(* ************************************************************************ *)

module type NS = sig
  val ty_var : Dolmen_std.Namespace.t
  val ty_cst : Dolmen_std.Namespace.t
  val term_var : Dolmen_std.Namespace.t
  val term_cst : Dolmen_std.Namespace.t
end

module Env(E : Expr_intf.Export)(N : NS) = struct

  type ty = E.Ty.t
  type ty_var = E.Ty.Var.t
  type ty_cst = E.Ty.Const.t
  type term = E.Term.t
  type term_var = E.Term.Var.t
  type term_cst = E.Term.Const.t
  type formula = E.formula

  module Id = Dolmen_std.Scope.Wrap
      (struct
        include E.Ty.Var
        let namespace _ = N.ty_var
      end)
      (struct
        include E.Ty.Const
        let namespace _ = N.ty_cst
      end)
      (struct
        include E.Term.Var
        let namespace _ = N.term_var
      end)
      (struct
        include E.Term.Const
        let namespace _ = N.term_cst
      end)

  module Scope = Dolmen_std.Scope.Make(Id)

  type 'a info = {
    unit: unit;
  }

  module H = Hmap.Make(struct type 'a t = 'a info end)

  type 'a key = 'a H.key

  type t = {
    scope : Scope.t;
    hmap : H.t;
  }

  let empty scope = {
    scope;
    hmap = H.empty;
  }

  let key () =
    H.Key.create { unit = (); }

  let get { hmap; _ } k =
    H.find k hmap

  let set ({ hmap; _ } as t) k v =
    { t with hmap = H.add k v hmap; }

  module Ty_var = struct
    let bind env v =
      { env with scope = Scope.bind env.scope (Ty_var v); }
    let name env v =
      Scope.name env.scope (Ty_var v)
  end
  module Ty_cst = struct
    let bind env c =
      { env with scope = Scope.bind env.scope (Ty_cst c); }
    let name env c =
      Scope.name env.scope (Ty_cst c)
  end
  module Term_var = struct
    let bind env v =
      { env with scope = Scope.bind env.scope (Term_var v); }
    let name env v =
      Scope.name env.scope (Term_var v)
  end
  module Term_cst = struct
    let bind env c =
      { env with scope = Scope.bind env.scope (Term_cst c); }
    let name env c =
      Scope.name env.scope (Term_cst c)
  end

end

(* Printing errors *)
(* ************************************************************************ *)


(* Printer interface *)
(* ************************************************************************ *)

module type S = sig

  include Typer.Types

  type acc

  val print : acc -> typechecked stmt -> acc

  val finalise : acc -> unit

end

(* Smtlib2 Printer *)
(* ************************************************************************ *)

module Smtlib2_6
    (Expr : Expr_intf.Export)
    (Sexpr : Dolmen_intf.View.Sexpr.S
      with type t = Dolmen_std.Term.t
       and type id := Dolmen_std.Id.t)
    (View : Dolmen_intf.View.TFF.S
     with type ty = Expr.Ty.t
      and type ty_var = Expr.Ty.Var.t
      and type ty_cst = Expr.Ty.Const.t
      and type ty_def = Expr.ty_def
      and type term = Expr.Term.t
      and type term_var = Expr.Term.Var.t
      and type term_cst = Expr.Term.Const.t
      and type formula = Expr.formula)
    (Typer_Types : Typer.Types
     with type ty = Expr.ty
      and type ty_var = Expr.ty_var
      and type ty_cst = Expr.ty_cst
      and type ty_def = Expr.ty_def
      and type term = Expr.term
      and type term_var = Expr.term_var
      and type term_cst = Expr.term_cst
      and type formula = Expr.formula)
= struct

  include Typer_Types

  module Env = Env(Expr)(struct
      let ty_var = Dolmen_std.Namespace.sort
      let ty_cst = Dolmen_std.Namespace.sort
      let term_var = Dolmen_std.Namespace.term
      let term_cst = Dolmen_std.Namespace.term
    end)

  module P = Dolmen.Smtlib2.Script.V2_6.Print.Make(Env)(Sexpr)(View)

  type acc = {
    env : Env.t;
    fmt : Format.formatter;
  }

  let f (x : Env.ty) : Typer_Types.ty = x

  let init fmt =
    let rename = Env.Scope.mk_rename 0 rename_num_postfix in
    let sanitize = Dolmen.Smtlib2.Script.V2_6.Print.sanitize in
    let on_conflict ~prev_id:_ ~new_id:_ ~name:_ = Env.Scope.Rename in
    let scope = Env.Scope.empty ~rename ~sanitize ~on_conflict in
    let env = Env.empty scope in
    { env; fmt; }

  (* declarations *)

  let map_decl = function
    | `Type_decl (c, None) ->
      Either.Left (`Type_decl c)
    | `Term_decl c ->
      Either.Left (`Term_decl c)
    | `Type_decl (c, Some def) ->
      begin match View.Ty.Def.view def with
        | Abstract -> Either.Left (`Type_decl c)
        | Algebraic { vars; cases; } ->
          let cases =
            List.map (function
                  Dolmen_intf.View.TFF.TypeDef.Case { constructor; params; } ->
                  constructor, params
              ) cases
          in
          Either.Right (c, vars, cases)
      end

  let register_simple_decl env = function
    | `Type_decl c -> Env.Ty_cst.bind env c
    | `Term_decl c -> Env.Term_cst.bind env c

  let register_adt_decl env (c, _vars, cases) =
    let env = Env.Ty_cst.bind env c in
    List.fold_left (fun env (cstr, params) ->
        let env = Env.Term_cst.bind env cstr in
        List.fold_left (fun env (_ty, dstr) ->
            Env.Term_cst.bind env dstr
          ) env params
      ) env cases

  let print_simple_decl env fmt = function
    | `Type_decl c ->
      Format.fprintf fmt "%a@." (P.declare_sort env) c
    | `Term_decl c ->
      Format.fprintf fmt "%a@." (P.declare_fun env) c

  (* defintions *)

  let map_def = function
    | `Type_alias (_, c, vars, body) -> Either.Left (c, vars, body)
    | `Term_def (_, c, [], params, body) -> Either.Right (c, params, body)
    | `Term_def (_, _, _ :: _, _, _) -> assert false (* TODO: proper error, poly fun in smt2 not allowed *)
    | `Instanceof _ -> assert false (* TODO: proper error, cannot print *)

  (* statement printing *)

  let pp_stmt ({ env; fmt; } as acc) pp x =
    Format.fprintf fmt "%a@." (pp env) x;
    acc

  let print acc (stmt : Typer_Types.typechecked Typer_Types.stmt) =
    match stmt.contents with
      (* info setters *)
      | `Set_logic (s, _) ->
        begin match Dolmen_type.Logic.Smtlib2.parse s with
          | Some _ -> pp_stmt acc P.set_logic s
          | None -> assert false (* TODO: proper error *)
        end
      | `Set_info s -> pp_stmt acc P.set_info s
      | `Set_option s -> pp_stmt acc P.set_option s
      (* Info getters *)
      | `Get_info s -> pp_stmt acc P.get_info s
      | `Get_option s -> pp_stmt acc P.get_option s
      | `Get_proof -> pp_stmt acc P.get_proof ()
      | `Get_unsat_core -> pp_stmt acc P.get_unsat_core ()
      | `Get_unsat_assumptions -> pp_stmt acc P.get_unsat_assumptions ()
      | `Get_model -> pp_stmt acc P.get_model ()
      | `Get_value l -> pp_stmt acc P.get_value l
      | `Get_assignment -> pp_stmt acc P.get_assignment ()
      | `Get_assertions -> pp_stmt acc P.get_assertions ()
      | `Echo s -> pp_stmt acc P.echo s
      (* Stack management *)
      | `Pop n -> pp_stmt acc P.pop n
      | `Push n -> pp_stmt acc P.push n
      | `Reset -> pp_stmt acc P.reset ()
      | `Reset_assertions -> pp_stmt acc P.reset_assertions ()
      (* Decls *)
      | `Decls (recursive, l) ->
        let { env; fmt; } = acc in
        let simples, adts = List.partition_map map_decl l in
        let env =
          match simples, adts, recursive with
          | [], l, _ ->
            (* slight over-approximation: we always treat all adts as recursive *)
            let env = List.fold_left register_adt_decl env l in
            Format.fprintf fmt "%a@." (P.declare_datatypes env) l;
            env
          | l, [], _ ->
            (* declarations for smtlib cannot be recursive:
               - type declarations's bodies are just integers
               - term declarations's bodies are types (and thus the term
                 constant begin declared cannot appear in them).
               Therefore, it should be fine to ignore the recursive flag.
               For the future, it might be better to adjust the flag to
               represent whether things are actually recursive. *)
            let env = List.fold_left register_simple_decl env l in
            List.iter (print_simple_decl env fmt) l;
            env
          | _ ->
            assert false (* TODO: better error / can this happen ? *)
        in
        { acc with env }
      (* Defs *)
      | `Defs (recursive, l) ->
        let { env; fmt; } = acc in
        let typedefs, fundefs = List.partition_map map_def l in
        let env =
          match typedefs, fundefs, recursive with
          | [], [], _ -> assert false (* internal invariant: should not happen *)
          | _ :: _, _ :: _, _ -> assert false (* can this happen ? *)
          | _ :: _, [], true -> assert false (* TODO: proper error / cannot print *)
          (* Note: we might want to have the body of a definition printed with
             an env that does not contain said definition, if only for shadowing
             purposes, but that would not change much for the smt2 since shadowing
             of constants is not allowed. *)
          | l, [], false ->
            List.fold_left (fun env ((c, _, _) as def) ->
                let env = Env.Ty_cst.bind env c in
                Format.fprintf fmt "%a@." (P.define_sort env) def;
                env
              ) env l
          | [], l, false ->
            List.fold_left (fun env ((c, _, _) as def) ->
                let env = Env.Term_cst.bind env c in
                Format.fprintf fmt "%a@." (P.define_fun env) def;
                env
              ) env l
          | [], [(c, _, _) as def], true ->
            let env = Env.Term_cst.bind env c in
            Format.fprintf fmt "%a@." (P.define_fun_rec env) def;
            env
          | [], l, true ->
            let env = List.fold_left (fun env (c, _, _) ->
                Env.Term_cst.bind env c
              ) env l
            in
            Format.fprintf fmt "%a@." (P.define_funs_rec env) l;
            env
        in
        { acc with env }
      (* Assume *)
      | `Hyp t -> pp_stmt acc P.assert_ t
      | `Goal _
      | `Clause _ ->
        (* TODO: need access to the `not` and `or` functions to create
           terms/formulas that can be viewed and printed *)
        assert false
      (* Solve *)
      | `Solve (hyps, goals) ->
        (* TODO: relax these restrictions *)
        assert (goals = []);
        assert (hyps = []);
        pp_stmt acc P.check_sat ()
      (* Exit *)
      | `Exit -> pp_stmt acc P.exit ()
      (* Other *)
      | `Other _ ->
        (* TODO: proper error / or allow extensions ? *)
        assert false

  let finalise _acc = ()

end


(* Dummy Printer *)
(* ************************************************************************ *)

module Dummy
    (Expr : Expr_intf.S)
    (Typer_Types : Typer.Types
     with type ty = Expr.ty
      and type ty_var = Expr.ty_var
      and type ty_cst = Expr.ty_cst
      and type ty_def = Expr.ty_def
      and type term = Expr.term
      and type term_var = Expr.term_var
      and type term_cst = Expr.term_cst
      and type formula = Expr.formula)
= struct

  include Typer_Types

  type acc = Format.formatter

  let init fmt = fmt

  let print fmt _ =
    Format.fprintf fmt "statement@.";
    fmt

  let finalise _fmt = ()

end

(* Pipe functor *)
(* ************************************************************************ *)

module Make
    (Expr : Expr_intf.Export)
    (Sexpr : Dolmen_intf.View.Sexpr.S
     with type t = Dolmen_std.Term.t
      and type id := Dolmen_std.Id.t)
    (View : Dolmen_intf.View.TFF.S
     with type ty = Expr.Ty.t
      and type ty_var = Expr.Ty.Var.t
      and type ty_cst = Expr.Ty.Const.t
      and type ty_def = Expr.ty_def
      and type term = Expr.Term.t
      and type term_var = Expr.Term.Var.t
      and type term_cst = Expr.Term.Const.t
      and type formula = Expr.formula)
    (State : State.S)
    (Typer_Types : Typer.Types
     with type ty = Expr.ty
      and type ty_var = Expr.ty_var
      and type ty_cst = Expr.ty_cst
      and type ty_def = Expr.ty_def
      and type term = Expr.term
      and type term_var = Expr.term_var
      and type term_cst = Expr.term_cst
      and type formula = Expr.formula)
= struct

  (* Type definitions *)
  module type S' = S
    with type ty := Expr.ty
     and type ty_var := Expr.ty_var
     and type ty_cst := Expr.ty_cst
     and type ty_def := Expr.ty_def
     and type term := Expr.term
     and type term_var := Expr.term_var
     and type term_cst := Expr.term_cst
     and type formula := Expr.formula

  type 'acc printer = (module S' with type acc = 'acc)

  type state =
    | No_export : state
    | Export : {
        acc : 'acc;
        printer : 'acc printer;
        close : unit -> unit;
      } -> state

  (* available printers *)

  module Dummy = Dummy(Expr)(Typer_Types)
  module Smtlib2_6 = Smtlib2_6(Expr)(Sexpr)(View)(Typer_Types)

  (* setup *)

  let pipe = "Export"

  let state : state State.key =
    State.create_key ~pipe "export_state"

  let init ?output_file st =
    let mk (type acc) close (acc : acc) (printer : acc printer) =
      Export { acc; printer; close; }
    in
    let mk lang output =
      let fmt, close =
        match output with
        | `Stdout ->
          Format.std_formatter, (fun () -> ())
        | `File filename ->
          let ch = open_out filename in
          let close () = close_out ch in
          let fmt = Format.formatter_of_out_channel ch in
          fmt, close
      in
      match (lang : language) with
      | Smtlib2 (`V2_6 | `Latest) ->
        let acc = Smtlib2_6.init fmt in
        mk close acc (module Smtlib2_6)
      | _ ->
        let acc = Dummy.init fmt in
        mk close acc (module Dummy)
    in
    let state_value =
      match output_file with
      | None -> No_export
      | Some { lang = Some lang; output; } ->
        mk lang output
      | Some { lang = None; output = `Stdout; } ->
        (* TODO: proper error *)
        assert false
      | Some { lang = None; output = (`File filename) as output; } ->
        begin match Logic.of_filename filename with
          | lang, _, _ -> mk lang output
          | exception Logic.Extension_not_found _ext ->
            assert false (* TODO: proper error *)
        end
    in
    st
    |> State.set state state_value

  (* interface *)

  let export st (c : Typer_Types.typechecked Typer_Types.stmt list) =
    match State.get state st with
    | No_export -> st, c
    | Export { acc; printer; close; } ->
      let (module P) = printer in
      let acc = List.fold_left P.print acc c in
      let st = State.set state (Export { acc; printer; close; }) st in
      st, c

  let finalise st =
    match State.get state st with
    | No_export -> st
    | Export { acc; printer; close; } ->
      let (module P) = printer in
      let () = P.finalise acc in
      let () = close () in
      State.set state No_export st

end
