
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

(* TODO: functorize over the vars/csts so that it is compatible with
   abstract types later in the `Smtlib2_6` module *)
module Env = struct

  module Id = Dolmen_std.Scope.Wrap
      (Dolmen_std.Expr.Ty.Var)(Dolmen_std.Expr.Ty.Const)
      (Dolmen_std.Expr.Term.Var)(Dolmen_std.Expr.Term.Const)

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

  module P =
    Dolmen.Smtlib2.Script.V2_6.Print.Make
      (Dolmen.Std.Expr.View.FO)(Env)

  type acc = {
    env : Env.t;
    fmt : Format.formatter;
  }

  let init fmt =
    let rename = Env.Scope.mk_rename 0 rename_num_postfix in
    let sanitize = Dolmen.Smtlib2.Script.V2_6.Print.sanitize in
    let on_conflict ~prev_id:_ ~new_id:_ ~name:_ = Env.Scope.Rename in
    let scope = Env.Scope.empty ~rename ~sanitize ~on_conflict in
    let env = Env.empty scope in
    { env; fmt; }

  let register_decl ({ env; fmt = _; } as acc) = function
    | `Type_decl (c, None) ->
      let env = Env.Ty_cst.bind env c in
      { acc with env }
    | `Type_decl (_c, Some _) ->
      (* TODO: handle datatypes, and constructors *)
      assert false
    | `Term_decl c ->
      let env = Env.Term_cst.bind env c in
      { acc with env }

  let print_decl { env; fmt; } = function
    | `Type_decl (c, None) ->
      P.declare_sort env fmt c
    | `Type_decl (_c, Some _) ->
      (* TODO: handle datatypes *)
      assert false
    | `Term_decl c ->
      P.declare_fun env fmt c

  let print ({ env; fmt; } as acc) (stmt : Typer_Types.typechecked Typer_Types.stmt) =
    match stmt.contents with
      (* info setters *)
      | `Set_logic (s, _) ->
        begin match Dolmen_type.Logic.Smtlib2.parse s with
          | Some _ ->
            Format.fprintf fmt "%a@." (P.set_logic env) s;
            acc
          | None ->
            assert false (* TODO: proper error *)
        end
      | `Set_info _ -> assert false
      | `Set_option _ -> assert false
      (* Info getters *)
      | `Get_info _ -> assert false
      | `Get_option _ -> assert false
      | `Get_proof -> P.get_proof env fmt (); acc
      | `Get_unsat_core -> P.get_unsat_core env fmt (); acc
      | `Get_unsat_assumptions -> P.get_unsat_assumptions env fmt (); acc
      | `Get_model -> P.get_model env fmt (); acc
      | `Get_value _ -> assert false
      | `Get_assignment -> P.get_assignment env fmt (); acc
      | `Get_assertions -> P.get_assertions env fmt (); acc
      | `Echo _ -> assert false
      (* Stack management *)
      | `Pop n -> P.pop env fmt n; acc
      | `Push n -> P.push env fmt n; acc
      | `Reset -> P.reset env fmt (); acc
      | `Reset_assertions -> P.reset_assertions env fmt (); acc
      (* Decls *)
      | `Decls (_recursive, l) ->
        (* TODO: use the `recursive` part *)
        let acc = List.fold_left register_decl acc l in
        List.iter (print_decl acc) l;
        acc
      (* Defs *)
      | `Defs _ -> assert false
      (* Assume *)
      | `Hyp _
      | `Goal _
      | `Clause _ -> assert false
      (* Solve *)
      | `Solve _ -> assert false
      (* Exit *)
      | `Exit -> P.exit env fmt (); acc
      (* Other *)
      | `Other _ -> assert false

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
    (Expr : Expr_intf.S)
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
    with type ty = Expr.ty
     and type ty_var = Expr.ty_var
     and type ty_cst = Expr.ty_cst
     and type ty_def = Expr.ty_def
     and type term = Expr.term
     and type term_var = Expr.term_var
     and type term_cst = Expr.term_cst
     and type formula = Expr.formula

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
  module Smtlib2_6 = Smtlib2_6(Expr)(Typer_Types)

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
