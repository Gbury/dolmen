
(* This file is free software, part of Dolmen. See file "LICENSE" for more details. *)

(* File Export

   This module defines a pipe to print/export/output typed terms.
*)

(* Type definitions *)
(* ************************************************************************ *)

type language = Logic.language

type file = language State.output_file

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

let code = Code.create
    ~category:"export"
    ~descr:"on export/printing errors"

let internal_error =
  Report.Error.mk ~code:Code.bug ~mnemonic:"export-unexpected"
    ~message:(fun fmt msg ->
        Format.fprintf fmt "@Internal Error: @[<hov>%a@]" Format.pp_print_text msg)
    ~name:"Internal Error" ()

let missing_language =
  Report.Error.mk ~code ~mnemonic:"export-missing-lang"
    ~message:(fun fmt () ->
        Format.fprintf fmt "Missing language for export")
    ~name:"Missing Export Language" ()

let extension_not_found =
  Report.Error.mk ~code ~mnemonic:"export-unknown-extension"
    ~message:(fun fmt ext ->
        Format.fprintf fmt
          "The following extension is not recognized: '%s'.@ %a"
          ext Format.pp_print_text
          "Either provide a recognized extension, or explicitly specify \
           an output language for export")
    ~name:"Unknown Export Extension" ()

let unsupported_language =
  Report.Error.mk ~code ~mnemonic:"export-unsupported-lang"
    ~message:(fun fmt lang ->
        Format.fprintf fmt
          "The following format is not (yet) supported for export: %s"
          (Logic.string_of_language lang)
      )
    ~name:"Unsupported Export Language" ()

let polymorphic_function_in_smt2 =
  Report.Error.mk ~code ~mnemonic:"export-smt2-poly"
    ~message:(fun fmt () -> (* TODO: find the location ? *)
        Format.fprintf fmt "%a" Format.pp_print_text
          "Cannot print: SMT-LIB2 version 2.6 and earlier do not allow polymorphic \
           function declarations or definitions.")
    ~name:"Polymorphic function in SMT-LIB2" ()



(* Printer interface *)
(* ************************************************************************ *)

module type S = sig

  include Typer.Types

  type st

  type acc

  val print : st -> acc -> typechecked stmt -> st * acc

end

(* Smtlib2 Printer *)
(* ************************************************************************ *)

module type Make_smt2_printer = functor
  (Env : Dolmen_intf.Env.Print
   with type name := Dolmen_std.Name.t)
  (S : Dolmen_intf.View.Sexpr.S
   with type id := Dolmen_std.Id.t)
  (V : Dolmen_intf.View.TFF.S
   with type ty = Env.ty
    and type ty_var = Env.ty_var
    and type ty_cst = Env.ty_cst
    and type term = Env.term
    and type term_var = Env.term_var
    and type term_cst = Env.term_cst)
  -> Dolmen_intf.Print.Smtlib2
  with type env := Env.t
   and type sexpr := S.t
   and type ty := Env.ty
   and type ty_var := Env.ty_var
   and type ty_cst := Env.ty_cst
   and type term := Env.term
   and type term_var := Env.term_var
   and type term_cst := Env.term_cst

module Smtlib2
    (State : State.S)
    (Printer : Make_smt2_printer)
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
      and type term_cst = Expr.Term.Const.t)
    (Typer_Types : Typer.Types
     with type ty = Expr.ty
      and type ty_var = Expr.ty_var
      and type ty_cst = Expr.ty_cst
      and type ty_def = Expr.ty_def
      and type term = Expr.term
      and type term_var = Expr.term_var
      and type term_cst = Expr.term_cst
      and type formula = Expr.term)
= struct

  (* printing misc    *)
  (* **************** *)

  let split_dec s =
    match Q.of_string s with
    | exception Invalid_argument _ -> None
    | q ->
      let num = Q.num q in
      let den = Q.den q in
      let sign = if Z.lt num Z.zero then `Neg else `Pos in
      Some (sign, Z.to_string (Z.abs num), Z.to_string den)


  (* modules and init *)
  (* **************** *)

  exception Error of State.t

  include Typer_Types

  module Env = Env(Expr)(struct
      let ty_var = Dolmen_std.Namespace.sort
      let ty_cst = Dolmen_std.Namespace.sort
      let term_var = Dolmen_std.Namespace.term
      let term_cst = Dolmen_std.Namespace.term
    end)

  module P = Printer(Env)(Sexpr)(View)

  type acc = {
    env : Env.t;
    fmt : Format.formatter;
    close : unit -> unit;
  }

  let init ~close fmt =
    let rename = Env.Scope.mk_rename 0 rename_num_postfix in
    let sanitize = Dolmen.Smtlib2.Script.V2_6.Print.sanitize in
    let on_conflict ~prev_id:_ ~new_id:_ ~name:_ = Env.Scope.Rename in
    let scope = Env.Scope.empty ~rename ~sanitize ~on_conflict in
    let env = Env.empty scope in
    let env = P.set_split_dec env split_dec in
    { env; fmt; close; }

  let pp_stmt st ({ env; fmt; close = _; } as acc) pp x =
    try
      Format.fprintf fmt "%a@." (pp env) x;
      st, acc
    with
    | Dolmen.Smtlib2.Script.V2_6.Print.Cannot_print msg
    | Dolmen.Smtlib2.Script.Poly.Print.Cannot_print msg ->
      let message = Format.asprintf "Could not print: %s" msg in
      let st = State.error st internal_error message in
      st, acc
    | Dolmen.Smtlib2.Script.V2_6.Print.Polymorphic_function_definition
    | Dolmen.Smtlib2.Script.V2_6.Print.Polymorphic_function_declaration ->
      let st = State.error st polymorphic_function_in_smt2 () in
      st, acc


  (* declarations *)
  (* ************ *)

  let map_decl = function
    | `Type_decl (c, None) ->
      Either.Left (`Type_decl c)
    | `Term_decl c ->
      Either.Left (`Term_decl c)
    | `Type_decl (c, Some def) ->
      begin match View.Ty.Def.view ~expand:false def with
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

  let print_simple_decl (st, acc) = function
    | `Type_decl c -> pp_stmt st acc P.declare_sort c
    | `Term_decl c -> pp_stmt st acc P.declare_fun c

  let print_decls st acc decls recursive =
    let simples, adts = List.partition_map map_decl decls in
    match simples, adts, recursive with
    | [], l, _ ->
      (* slight over-approximation: we always treat all adts as recursive *)
      let acc =
        { acc with env = List.fold_left register_adt_decl acc.env l }
      in
      pp_stmt st acc P.declare_datatypes l
    | l, [], _ ->
      (* declarations for smtlib cannot be recursive:
         - type declarations's bodies are just integers
         - term declarations's bodies are types (and thus the term
           constant begin declared cannot appear in them).
         Therefore, it should be fine to ignore the recursive flag.
         For the future, it might be better to adjust the flag to
         represent whether things are actually recursive. *)
      let acc =
        { acc with env = List.fold_left register_simple_decl acc.env l; }
      in
      List.fold_left print_simple_decl (st, acc) l
    | _ ->
      (* Can this happen ? *)
      let st = State.error st internal_error "Unsupported mix of declarations" in
      st, acc

  (* definitions *)
  (* *********** *)

  let map_def st = function
    | `Type_alias (_, c, vars, body) -> Either.Left (c, vars, body)
    | `Term_def (_, c, vars, params, body) -> Either.Right (c, vars, params, body)
    | `Instanceof _ ->
      let st =
        State.error st internal_error
          "Cannot print model redefinition of corner cases"
      in
      raise (Error st)

  let assert_not_named c =
    match Expr.Term.Const.get_tag c Expr.Tags.named with
    | Some _ -> assert false (* TODO: better error *)
    | None -> ()

  let print_defs st acc defs recursive =
    match List.partition_map (map_def st) defs, recursive with
    | exception Error st -> st, acc
    | ([], []), _ -> assert false (* internal invariant: should not happen *)
    | (_ :: _, _ :: _), _ -> assert false (* can this happen ? *)
    | (_ :: _, []), true -> assert false (* TODO: proper error / cannot print *)

    (* Note: we might want to have the body of a definition printed with
       an env that does not contain said definition, if only for shadowing
       purposes, but that would not change much for the smt2 since shadowing
       of constants is not allowed. *)

    (* Type Defs *)
    | (l, []), false ->
      List.fold_left (fun (st, acc) ((c, _, _) as def) ->
          let acc = { acc with env = Env.Ty_cst.bind acc.env c; } in
          pp_stmt st acc P.define_sort def
        ) (st, acc) l

    (* Term Defs (regular) *)
    | ([], l), false ->
      List.fold_left (fun (st, acc) ((c, vars, params, body) as def) ->
          let env = Env.Term_cst.bind acc.env c in
          match Expr.Term.Const.get_tag c Expr.Tags.named with
          | None ->
            let acc = { acc with env = Env.Term_cst.bind acc.env c; } in
            pp_stmt st acc P.define_fun def
          | Some _ ->
            assert (vars = [] && params = []);
            let env = P.add_named env c body in
            st, { acc with env }
        ) (st, acc) l

    (* Term Defs (recursive) *)
    | ([], [(c, _, _, _) as def]), true ->
      let acc = { acc with env = Env.Term_cst.bind acc.env c; } in
      assert_not_named c;
      pp_stmt st acc P.define_fun_rec def
    | ([], l), true ->
      let acc = {
        acc with
        env =
          List.fold_left (fun env (c, _, _, _) ->
              assert_not_named c;
              Env.Term_cst.bind env c
            ) acc.env l}
      in
      pp_stmt st acc P.define_funs_rec l


  (* solve/check-sat *)
  (* *************** *)

  let is_not_trivially_false t =
    match View.Term.view t with
    | App (c, [], []) when (match View.Term.Cst.builtin c with
          | Dolmen_std.Builtin.False -> true
          | _ -> false) -> false
      | _ -> true

    let print_solve_aux st acc ~hyps =
      let st, acc, local_hyps_rev =
        List.fold_left (fun (st, acc, local_hyps) hyp ->
            match P.match_prop_literal hyp with
            | `Cst _ | `Neg _ -> st, acc, hyp :: local_hyps
            | `Not_a_prop_literal ->
              let prop = View.Term.ty hyp in
              let path = Dolmen_std.Path.global "local_hyp" in
              let c = Expr.Term.Const.mk path prop in
              let st, acc = pp_stmt st acc P.define_fun (c, [], [], hyp) in
              st, acc, (Expr.Term.of_cst c :: local_hyps)
          ) (st, acc, []) hyps
      in
      pp_stmt st acc P.check_sat_assuming (List.rev local_hyps_rev)

    let print_solve st acc ~hyps ~goals =
      let goals = List.filter is_not_trivially_false goals in
      match goals with
      | [] -> print_solve_aux st acc ~hyps
      | _ :: _ ->
        let st, acc = pp_stmt st acc P.push 1 in
        let st, acc = List.fold_left (fun (st, acc) goal ->
            pp_stmt st acc P.assert_ (Expr.Term.neg goal)
          ) (st, acc) goals
        in
        let st, acc = print_solve_aux st acc ~hyps in
        let st, acc = pp_stmt st acc P.pop 1 in
        st, acc


  (* statement printing *)
  (* ****************** *)

  let print st acc (stmt : Typer_Types.typechecked Typer_Types.stmt) =
    match stmt.contents with
      (* info setters *)
      | `Set_logic (s, _) ->
        begin match Dolmen_type.Logic.Smtlib2.parse s with
          | Some _ ->
            pp_stmt st acc P.set_logic s
          | None ->
            let st = State.error st internal_error "Unparseable logic" in
            st, acc
        end
      | `Set_info s -> pp_stmt st acc P.set_info s
      | `Set_option s -> pp_stmt st acc P.set_option s
      (* Info getters *)
      | `Get_info s -> pp_stmt st acc P.get_info s
      | `Get_option s -> pp_stmt st acc P.get_option s
      | `Get_proof -> pp_stmt st acc P.get_proof ()
      | `Get_unsat_core -> pp_stmt st acc P.get_unsat_core ()
      | `Get_unsat_assumptions -> pp_stmt st acc P.get_unsat_assumptions ()
      | `Get_model -> pp_stmt st acc P.get_model ()
      | `Get_value l -> pp_stmt st acc P.get_value l
      | `Get_assignment -> pp_stmt st acc P.get_assignment ()
      | `Get_assertions -> pp_stmt st acc P.get_assertions ()
      | `Echo s -> pp_stmt st acc P.echo s
      (* Stack management *)
      | `Pop n -> pp_stmt st acc P.pop n
      | `Push n -> pp_stmt st acc P.push n
      | `Reset -> pp_stmt st acc P.reset ()
      | `Reset_assertions -> pp_stmt st acc P.reset_assertions ()
      (* Decls & defs *)
      | `Decls (recursive, decls) -> print_decls st acc decls recursive
      | `Defs (recursive, defs) -> print_defs st acc defs recursive
      (* Assume *)
      | `Hyp t -> pp_stmt st acc P.assert_ t
      | `Goal g -> pp_stmt st acc P.assert_ (Expr.Term.neg g)
      | `Clause l ->  pp_stmt st acc P.assert_ (Expr.Term._or l)
      (* Solve *)
      | `Solve (hyps, goals) -> print_solve st acc ~hyps ~goals
      (* Exit *)
      | `Exit -> pp_stmt st acc P.exit ()
      | `End -> acc.close (); st, acc
      (* Other *)
      | `Other _ ->
        (* TODO: allow extensions/plugin *)
        let st =
          State.error st internal_error
            "Unsupported statement (coming from an extension)"
        in
        st, acc

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
      and type formula = Expr.term)
= struct

  include Typer_Types

  type acc = Format.formatter

  let init fmt = fmt

  let print st fmt _ =
    Format.fprintf fmt "statement@.";
    st, fmt

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
      and type term_cst = Expr.Term.Const.t)
    (State : State.S)
    (Typer_Types : Typer.Types
     with type ty = Expr.ty
      and type ty_var = Expr.ty_var
      and type ty_cst = Expr.ty_cst
      and type ty_def = Expr.ty_def
      and type term = Expr.term
      and type term_var = Expr.term_var
      and type term_cst = Expr.term_cst
      and type formula = Expr.term)
= struct

  (* Type definitions *)
  module type S' = S
    with type st := State.t
     and type ty := Expr.ty
     and type ty_var := Expr.ty_var
     and type ty_cst := Expr.ty_cst
     and type ty_def := Expr.ty_def
     and type term := Expr.term
     and type term_var := Expr.term_var
     and type term_cst := Expr.term_cst
     and type formula := Expr.term

  type 'acc printer = (module S' with type acc = 'acc)

  type state =
    | No_export : state
    | Export : {
        acc : 'acc;
        printer : 'acc printer;
      } -> state

  (* available printers *)

  module Dummy = Dummy(Expr)(Typer_Types)
  module Smtlib2_6 = Smtlib2(State)(Dolmen.Smtlib2.Script.V2_6.Print.Make)(Expr)(Sexpr)(View)(Typer_Types)
  module Smtlib2_Poly = Smtlib2(State)(Dolmen.Smtlib2.Script.Poly.Print.Make)(Expr)(Sexpr)(View)(Typer_Types)

  (* setup *)

  let pipe = "Export"

  let state : state State.key =
    State.create_key ~pipe "export_state"

  let init ?output_file st =
    let mk (type acc) (acc : acc) (printer : acc printer) =
      Export { acc; printer; }
    in
    let mk st lang output =
      let fmt, close =
        match output with
        | `Stdout ->
          Format.std_formatter, (fun () -> ())
        | `File filename ->
          (* Use a tmpfile because it helps a lot. That way, we are guaranteed
             the tmpfile is distinct form any pre-existing file, and we can rename
             it at the end. This brings a notion of atomicity of the effective
             printing, and more importantly allow the destination of the printing
             to be the same file as the input file. *)
          let dst_dir = Filename.dirname filename in
          let closed = ref false in
          let temp_file_name, ch =
            Filename.open_temp_file "dolmen" ".out"
              ~temp_dir:dst_dir ~perms:0o644
          in
          (* slight hack to ensure that the tmpfile is deleted if
             there is an error before the printing is finished. *)
          at_exit (fun () ->
              if not !closed then begin
                close_out ch;
                Unix.unlink temp_file_name
              end);
          let close () =
            closed := true;
            close_out ch;
            Unix.rename temp_file_name filename
          in
          let fmt = Format.formatter_of_out_channel ch in
          fmt, close
      in
      match (lang : language) with
      | Smtlib2 (`V2_6 | `Latest) ->
        let acc = Smtlib2_6.init ~close fmt in
        st, mk acc (module Smtlib2_6)
      | Smtlib2 `Poly ->
        let acc = Smtlib2_Poly.init ~close fmt in
        st, mk acc (module Smtlib2_Poly)
      | lang ->
        let () = close () in
        let st = State.error st unsupported_language lang in
        let acc = Dummy.init fmt in
        st, mk acc (module Dummy)
    in
    let st, state_value =
      match (output_file : file option) with
      | None -> st, No_export
      | Some ({ lang = Some lang; sink; } as f) ->
        let st = State.set State.export_file f st in
        mk st lang sink
      | Some { lang = None; sink = `Stdout; } ->
        let st = State.error st missing_language () in
        st, No_export
      | Some { lang = None; sink = (`File filename) as output; } ->
        begin match Logic.of_filename filename with
          | lang, _, _ ->
            let f : file = { lang = Some lang; sink = output; } in
            let st = State.set State.export_file f st in
            mk st lang output
          | exception Logic.Extension_not_found ext ->
            let st = State.error st extension_not_found ext in
            st, No_export
        end
    in
    st
    |> State.set state state_value

  (* interface *)

  let export st (l : Typer_Types.typechecked Typer_Types.stmt list) =
    match State.get state st with
    | No_export -> st, l
    | Export { acc; printer; } ->
      let (module P) = printer in
      let st, _ =
        List.fold_left (fun (st, acc) stmt ->
            let st, acc = P.print st acc stmt in
            let st = State.set state (Export { acc; printer; }) st in
            st, acc
          ) (st, acc) l
      in
      st, l

end
