
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)


(* Debug printing *)
(* ************** *)

let debug_parsed_pipe st c =
  if Loop.State.get Loop.State.debug st then
    Format.eprintf "[logic][parsed][%a] @[<hov>%a@]@."
      Dolmen.Std.Loc.print_compact c.Dolmen.Std.Statement.loc
      Dolmen.Std.Statement.print c;
  st, c

let debug_typed_pipe st stmts =
  if Loop.State.get Loop.State.debug st then begin
    List.iter (fun stmt ->
        Format.eprintf "[logic][typed][%a] @[<hov>%a@]@\n"
          Dolmen.Std.Loc.print_compact stmt.Loop.Typer_Pipe.loc
          Loop.Typer_Pipe.print stmt) stmts;
    Format.eprintf "@.";
  end;
  st, stmts


(* Run dolmen (regular use) *)
(* ************************ *)

let handle_exn st bt exn =
  let _st = Errors.exn st bt exn in
  exit 125

let finally st e =
  match e with
  | None -> st
  | Some (bt,exn) ->
    handle_exn st bt exn

let run st preludes logic_file =
  if Loop.State.get Loop.State.debug st then begin
    Dolmen.Std.Expr.Print.print_tags := true;
    Dolmen.Std.Expr.Print.print_index := true;
    ()
  end;
  let g =
    try
      Loop.Parser.parse_logic ~preludes logic_file
    with exn ->
      let bt = Printexc.get_raw_backtrace () in
      handle_exn st bt exn
  in
  let st =
    let open Loop.Pipeline in
    run ~finally g st (
      (fix (op ~name:"expand" Loop.Parser.expand) (
          (op ~name:"debug-parsed" debug_parsed_pipe)
          @>>> (op ~name:"flow" Loop.Flow.inspect)
          @>>> (op ~name:"headers" Loop.Header.inspect)
          @>>> (op ~name:"typecheck" Loop.Typer_Pipe.typecheck)
          @>|> (op ~name:"debug-typed" debug_typed_pipe)
          @>>> (op ~name:"check" Loop.Check.check)
          @>>> (op (fun st _ -> st, ())) @>>> _end
        )
      )
    )
  in
  let st = Loop.Flow.finalise st in
  let st = Loop.Header.check st in
  let _st = Dolmen_loop.State.flush st () in
  ()

(* Warning/Error list *)
(* ****************** *)

let list conf =
  let open Dolmen_loop in
  let l =
    List.sort (fun r r' ->
        String.compare (Report.T.mnemonic r) (Report.T.mnemonic r')
      ) (Report.T. list ())
  in
  let pp_kind fmt = function
    | `All ->
      Format.fprintf fmt "%-15s" "group"
    | `Error _ ->
      Format.fprintf fmt "%-15s" "error"
    | `Warning Report.Any_warn w ->
      Format.fprintf fmt "w:%-13s"
        (Report.Warning.Status.to_string (Report.Conf.status conf w))
  in
  Format.printf "%-30s%-15s%-15s%s@\n%s@\n"
    "mnemonic" "kind" "category" "description"
    (String.make 100 '-');
  List.iter (fun t ->
      Format.printf "%-30s%a%-15s%s@\n"
        (Report.T.mnemonic t) pp_kind t
        (Report.T.category t) (Report.T.name t)
    ) l


(* Warning/Error documentation *)
(* *************************** *)

let doc conf t =
  let open Dolmen_loop in
  let pp_status fmt = function
    | `All | `Error _ -> ()
    | `Warning Report.Any_warn w ->
      Format.fprintf fmt "@ By default: %a"
        Report.Warning.Status.print (Report.Conf.status conf w)
  in
  Format.printf
    "@[<v>@   %s@ @ kind: %s@ Category: %s@ Mnemonic: %s%a@ @ @[<hov>  %t@]@]@."
    (Report.T.name t)
    (Report.T.kind t)
    (Report.T.category t)
    (Report.T.mnemonic t)
    pp_status t
    (Report.T.doc t)

(* Extensions list *)
(* *************** *)

let list_extensions () =
  Format.printf "%a@."
    Fmt.(vbox @@ list (box Extensions.pp)) (Extensions.list ());
  match Extensions.invalid () with
  | [] -> ()
  | invalid ->
    Format.printf "@[<v 2>%a@ %a@]@."
      (Fmt.styled `Bold @@ Fmt.styled (`Fg (`Hi `Magenta)) @@ Fmt.string)
      "The following plugins were found but are invalid:"
      Fmt.(list string) invalid


(* Main code *)
(* ********* *)

let () =
  let version = "0.10" in
  let exits =
    List.map (fun code ->
        let retcode, doc = Dolmen_loop.Code.descr code in
        Cmdliner.Cmd.Exit.info ~doc retcode
      ) (Dolmen_loop.Code.errors ())
    @ Cmdliner.Cmd.Exit.defaults
  in
  let cli_term = Cmdliner.Cmd.v
      (Cmdliner.Cmd.info "dolmen" ~exits ~man:Man.cli ~version)
      Options.cli
  in
  match Cmdliner.Cmd.eval_value cli_term with
  | Ok (`Version | `Help) ->
    exit 0
  | Error (`Parse | `Term | `Exn) ->
    exit Cmdliner.Cmd.Exit.cli_error
  | Ok (`Ok Run { state ; preludes; logic_file }) ->
    run state preludes logic_file
  | Ok (`Ok Doc { report; conf; }) ->
    doc conf report
  | Ok (`Ok List_reports { conf; }) ->
    list conf
  | Ok (`Ok List_extensions) ->
    list_extensions ()