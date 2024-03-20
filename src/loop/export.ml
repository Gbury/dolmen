
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

  module P = Dolmen.Smtlib2.Script.V2_6.Print.Make(Dolmen.Std.Expr.View.FO)

  type acc = {
    fmt : Format.formatter;
  }

  let init fmt = { fmt; }

  let print ({ fmt; } as acc) (stmt : Typer_Types.typechecked Typer_Types.stmt) =
    begin match stmt.contents with
      (* info setters *)
      | `Set_logic (s, _) ->
        begin match Dolmen_type.Logic.Smtlib2.parse s with
          | Some _ -> Format.fprintf fmt "%a@." P.set_logic s
          | None -> assert false (* TODO: proper error *)
        end
      | `Set_info _ -> assert false
      | `Set_option _ -> assert false
      (* Info getters *)
      | `Get_info _ -> assert false
      | `Get_option _ -> assert false
      | `Get_proof -> P.get_proof fmt ()
      | `Get_unsat_core -> P.get_unsat_core fmt ()
      | `Get_unsat_assumptions -> P.get_unsat_assumptions fmt ()
      | `Get_model -> P.get_model fmt ()
      | `Get_value _ -> assert false
      | `Get_assignment -> P.get_assignment fmt ()
      | `Get_assertions -> P.get_assertions fmt ()
      | `Echo _ -> assert false
      (* Stack management *)
      | `Pop n -> P.pop fmt n
      | `Push n -> P.push fmt n
      | `Reset -> P.reset fmt ()
      | `Reset_assertions -> P.reset_assertions fmt ()
      (* Decls *)
      | `Decls _ -> assert false
      (* Defs *)
      | `Defs _ -> assert false
      (* Assume *)
      | `Hyp _
      | `Goal _
      | `Clause _ -> assert false
      (* Solve *)
      | `Solve _ -> assert false
      (* Exit *)
      | `Exit -> P.exit fmt ()
      (* Other *)
      | `Other _ -> assert false
    end;
    acc

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
