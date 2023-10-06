
(* Flow check

   This module defined a pipe to check the coherence of the sequence of
   statements in a file. This is pertinent for non-declarative languages,
   such as smtlib (whereas languages such as TPTP are coherent by design).
*)

module S = State

(* Automaton *)
(* ************************************************************************ *)

module type Automaton = sig
  type st
  type node
  type statement

  val init : node
  val step : st -> node -> statement -> st * node
  val finalise : st -> node -> st
end

(* Dummy automaton *)
(* ************************************************************************ *)

module Dummy = struct
  type node = unit
  let init = ()
  let step st () _ = st, ()
  let finalise st () = st
end

(* Smtlib automaton *)
(* ************************************************************************ *)

let code = Code.create
    ~category:"Flow"
    ~descr:"on flow verification errors"

let ignored_statement =
  Report.Warning.mk ~code ~mnemonic:"ignored-statement"
    ~message:(fun fmt () ->
        Format.fprintf fmt "%a" Format.pp_print_text
          "This statement is effectively ignored: there is no check-sat/prove \
           statement between it and a following pop/exit/reset statement.")
    ~name:"Ignored Statement" ()

let unclosed_push =
  Report.Warning.mk ~code ~mnemonic:"unclosed-push"
    ~message:(fun fmt () ->
        Format.fprintf fmt "%a" Format.pp_print_text
          "This push statement was not closed")
    ~hints:[fun () ->
        Some (Format.dprintf "%a" Format.pp_print_text
                "You can close a push statement with one of the following statements: \
                 pop, reset, reset-assertions.")
      ]
    ~name:"Unclosed Push" ()

let pop_without_push =
  Report.Error.mk ~code ~mnemonic:"pop-without-push"
    ~message:(fun fmt () ->
        Format.fprintf fmt "%a" Format.pp_print_text
          "This pop statement is missing a corresponding 'push' statement.")
    ~name:"Pop without a Push" ()

let missing_exit =
  Report.Error.mk ~code ~mnemonic:"missing-exit"
    ~message:(fun fmt () ->
        Format.fprintf fmt "Missing exit statement")
    ~name:"Missing Exit statement" ()

let stmt_after_exit =
  Report.Error.mk ~code ~mnemonic:"stmt-after-push"
    ~message:(fun fmt () ->
        Format.fprintf fmt "Statement after an exit")
    ~name:"Statement after an Exit" ()

let forbidden_stmt =
  Report.Error.mk ~code ~mnemonic:"forbidden-stmt"
    ~message:(fun fmt msg ->
        Format.fprintf fmt "Forbidden statement: %a"
          Format.pp_print_text msg)
    ~name:"Forbidden Statement" ()


module Smtlib2(State : State.S)
  : Automaton with type st := State.t
               and type statement := Dolmen.Std.Statement.t
= struct

  type loc = {
    file : Logic.language S.file;
    loc : Dolmen.Std.Loc.t;
  }

  (* Smtlib transitions, the reference is page 53 of:
     http://smtlib.cs.uiowa.edu/papers/smt-lib-reference-v2.6-r2017-07-18.pdf

     However, here we are insterested in doing a bit, and actually ensuring
     that any assertions is never "lost". For instance, consider the following:
     ```smt2
     (set-logic ALL)
     (assert true)
     (check-sat)
     (assert false)
     (exit)
     ```
     The last assertion is effectively lost, and cannot be used, which is most
     likely an error. While such a situation is not likely to happen in regular
     problems, once you add push/pop operations, it is much easier to end up
     in such a situation. *)
  type assert_level = {
    start : loc;
    (* None for top-level, Some push.loc for levels pushed *)
    stmts : loc list;
    (* asserts and declares/defines for the current level *)
  }

  type assert_stack = {
    top : loc list;
    stack: assert_level list;
  }

  type node =
    | Start_mode
    | Assert_mode of assert_stack
    | Sat_or_unsat_mode of assert_stack
    | Exited

  (* Reporting helpers *)

  let loc st (stmt : Dolmen.Std.Statement.t) =
    let file = State.get State.logic_file st in
    { file; loc = stmt.loc; }

  let lose_stmts st l =
    List.fold_left (fun st { file; loc } ->
        let loc : Dolmen.Std.Loc.full = { file = file.loc; loc; } in
        State.warn ~file ~loc st ignored_statement ()
      ) st l

  let after_exit st stmt =
    let { file; loc; } = loc st stmt in
    let loc : Dolmen.Std.Loc.full = { file = file.loc; loc; } in
    State.error ~file ~loc st stmt_after_exit (), Exited

  let forbidden st mode stmt msg =
    let { file; loc; } = loc st stmt in
    let loc : Dolmen.Std.Loc.full = { file = file.loc; loc; } in
    State.error ~file ~loc st forbidden_stmt msg, mode

  let no_set_logic st mode stmt =
    forbidden st mode stmt "missing set-logic statement prior to this one"


  (* Automaton helpers *)

  let add st { top; stack; } stmt =
    match stack with
    | [] ->
      { top = loc st stmt :: top; stack; }
    | { start; stmts; } :: r ->
      { top; stack = { start; stmts = loc st stmt :: stmts; } :: r; }

  let push stmt (st, { top; stack; }) =
    st, { top; stack = { start = loc st stmt; stmts = []; } :: stack; }

  let pop stmt (st, { top; stack; }) =
    match stack with
    | [] ->
      let loc = loc st stmt in
      let file = loc.file in
      let loc : Dolmen.Std.Loc.full = { file = file.loc; loc = loc.loc; } in
      let st = State.error ~file ~loc st pop_without_push () in
      st, { top; stack = []; }
    | { start = _; stmts; } :: r ->
      let st = lose_stmts st stmts in
      st, { top; stack = r; }

  let check_sat { top = _; stack; } =
    let stack = List.map (fun { start; stmts = _ } -> { start; stmts = []; }) stack in
    { top = []; stack; }

  let reset_assertions ~explicit st { top; stack; } =
    let st =
      match stack with
      | [] -> st
      | (_ :: _) as l ->
        List.fold_left (fun st { start = { file; loc; }; stmts; } ->
            let st =
              if explicit then st
              else begin
                let loc : Dolmen.Std.Loc.full = { file = file.loc; loc; } in
                State.warn ~file ~loc st unclosed_push ()
              end
            in
            lose_stmts st stmts
          ) st l
    in
    let st = lose_stmts st top in
    st, { top = []; stack = []; }

  (* Automaton functions *)

  let init = Start_mode

  let finalise st mode =
    match mode with
    | Exited -> st
    | Start_mode ->
      let file = State.get State.logic_file st in
      State.error st ~file missing_exit ()
    | Assert_mode stack
    | Sat_or_unsat_mode stack ->
      (* check for "lost" statements or unclosed "pushs" *)
      let st, _stack = reset_assertions ~explicit:false st stack in
      let file = State.get State.logic_file st in
      State.error st ~file missing_exit ()

  let step st mode (stmt : Dolmen.Std.Statement.t) =
    match stmt.descr with

    (* these should not happen, just ignore them (TODO: emit warning ?) *)
    | Pack _ | Include _ ->
      begin match mode with
        | Start_mode
        | Assert_mode _
        | Sat_or_unsat_mode _ ->
          st, mode
        | Exited ->
          after_exit st stmt
      end

    (* non smtlib ref case *)
    | Exit ->
      begin match mode with
        | Start_mode -> st, Exited
        | Assert_mode stack
        | Sat_or_unsat_mode stack ->
          let st, _stack = reset_assertions ~explicit:false st stack in
          st, Exited
        | Exited ->
          after_exit st stmt
      end

    (* smtlib ref case: 'gsio' *)
    | Other _
    | Get_info _ | Set_info _
    | Get_option _ | Set_option _ ->
      begin match mode with
        | Start_mode
        | Assert_mode _
        | Sat_or_unsat_mode _ ->
          st, mode
        | Exited ->
          after_exit st stmt
      end

    (* smtlib ref case: 'e' *)
    | Echo _ ->
      begin match mode with
        | Start_mode
        | Assert_mode _
        | Sat_or_unsat_mode _ ->
          st, mode
        | Exited ->
          after_exit st stmt
      end

    (* smtlib ref case: 'sl' *)
    | Set_logic _ ->
      begin match mode with
        | Start_mode ->
          st, Assert_mode { top = []; stack = []; }
        | Assert_mode _
        | Sat_or_unsat_mode _ ->
          forbidden st mode stmt "there is already a previous set-logic statement"
        | Exited ->
          after_exit st stmt
      end

    (* smtlib ref case: 'g' *)
    | Get_assertions ->
      begin match mode with
        | Start_mode -> no_set_logic st mode stmt
        | Assert_mode _
        | Sat_or_unsat_mode _ ->
          st, mode
        | Exited ->
          after_exit st stmt
      end

    (* smtlib ref case: 'r' *)
    | Reset ->
      begin match mode with
        | Start_mode -> st, Start_mode
        | Assert_mode stack
        | Sat_or_unsat_mode stack ->
          let st, _stack = reset_assertions ~explicit:true st stack in
          st, Start_mode
        | Exited ->
          after_exit st stmt
      end

    (* smtlib ref case: 'ra' *)
    | Reset_assertions ->
      begin match mode with
        | Start_mode -> st, Start_mode
        | Assert_mode stack
        | Sat_or_unsat_mode stack ->
          let st, stack = reset_assertions ~explicit:true st stack in
          st, Assert_mode stack
        | Exited ->
          after_exit st stmt
      end

    (* stmlib ref case: 'p' *)
    | Pop n ->
      begin match mode with
        | Start_mode -> no_set_logic st mode stmt
        | Assert_mode stack
        | Sat_or_unsat_mode stack ->
          let st, stack = Dolmen.Std.Misc.foldn n (pop stmt) (st, stack) in
          st, Assert_mode stack
        | Exited ->
          after_exit st stmt
      end
    | Push n ->
      begin match mode with
        | Start_mode -> no_set_logic st mode stmt
        | Assert_mode stack
        | Sat_or_unsat_mode stack ->
          let st, stack = Dolmen.Std.Misc.foldn n (push stmt) (st, stack) in
          st, Assert_mode stack
        | Exited ->
          after_exit st stmt
      end

    (* smtlib ref case: 'ad' *)
    | Defs _ | Decls _
    | Clause _ | Antecedent _ | Consequent _ ->
      begin match mode with
        | Start_mode -> no_set_logic st mode stmt
        | Assert_mode stack
        | Sat_or_unsat_mode stack ->
          let stack = add st stack stmt in
          st, Assert_mode stack
        | Exited ->
          after_exit st stmt
      end

    (* smtlib ref case: 'c' *)
    | Prove _ ->
      begin match mode with
        | Start_mode -> no_set_logic st mode stmt
        | Assert_mode stack ->
          let stack = check_sat stack in
          st, Sat_or_unsat_mode stack
        | Sat_or_unsat_mode _ ->
          st, mode
        | Exited ->
          after_exit st stmt
      end

    (* smtlib ref case: 'gpu' *)
    | Get_proof | Get_unsat_core | Get_unsat_assumptions ->
      begin match mode with
        | Start_mode -> no_set_logic st mode stmt
        | Assert_mode _ ->
          forbidden st mode stmt "missing check-sat prior to this statement"
        | Sat_or_unsat_mode _ ->
          st, mode
        | Exited ->
          after_exit st stmt
      end

    (* smtlib ref case: 'gamv' *)
    | Get_model | Get_value _ | Get_assignment ->
      begin match mode with
        | Start_mode -> no_set_logic st mode stmt
        | Assert_mode _ ->
          forbidden st mode stmt "missing check-sat prior to this statement"
        | Sat_or_unsat_mode _ ->
          st, mode
        | Exited ->
          after_exit st stmt
      end

end

(* Pipe *)
(* ************************************************************************ *)

module type S = Flow_intf.S

module Make(State : State.S) = struct

  module type S = Automaton
    with type st := State.t
     and type statement := Dolmen.Std.Statement.t

  module Smtlib2 = Smtlib2(State)

  type 'node automaton = 'node * (module S with type node = 'node)

  type state =
    | No_check
    | Check_init
    | Check : 'node automaton -> state

  type aux = {
    f : 'node. State.t -> 'node automaton -> State.t;
  }

  let state_key : state State.key =
    State.create_key ~pipe:"flow" "flow_state"

  let init ~flow_check st =
    let state = if flow_check then Check_init else No_check in
    State.set state_key state st

  let with_automaton st ~check =
    match State.get state_key st with
    | No_check -> st
    | Check_init ->
      let (module Automaton : S) =
        let logic_file = State.get State.logic_file st in
        match logic_file.lang with
        | Some Smtlib2 _ -> (module Smtlib2)
        | _ -> (module Dummy)
      in
      check.f st (Automaton.init, (module Automaton))
    | Check automaton ->
      check.f st automaton

  let inspect_aux (type node) st c ((node,(module Automaton)) : node automaton) =
    let st, node = Automaton.step st node c in
    State.set state_key (Check (node, (module Automaton))) st

  let finalise_aux (type node) st ((node,(module Automaton)) : node automaton) =
    Automaton.finalise st node

  let inspect st c =
    let st =
      with_automaton st ~check:{
        f = fun st automaton -> inspect_aux st c automaton
      }
    in
    st, c

  let finalise st =
    with_automaton st ~check:{ f = finalise_aux }

end

