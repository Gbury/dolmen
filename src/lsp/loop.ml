
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

module Pipeline = Dolmen_loop.Pipeline.Make(State)

module Parser = Dolmen_loop.Parser.Make(State)
module Header = Dolmen_loop.Headers.Make(State)
module Typer = Dolmen_loop.Typer.Typer(State)
module Typer_Pipe = Dolmen_loop.Typer.Make(Dolmen.Std.Expr)(Dolmen.Std.Expr.Print)(State)(Typer)

exception Finished of (State.t, string) result

let handle_exn st = function

  (* Internal exception used for jumping *)
  | State.Error st -> Ok st

  (* Simple error cases *)
  | Dolmen_loop.Pipeline.Sigint -> Error "user interrupt"
  | Dolmen_loop.Pipeline.Out_of_time -> Error "timeout"
  | Dolmen_loop.Pipeline.Out_of_space -> Error "memoryout"
  (* Fallback *)
  | exn ->
    let bt = Printexc.get_raw_backtrace () in
    Ok (State.error st
          Dolmen_loop.Report.Error.uncaught_exn (exn, bt))

let finally st e =
  match e with
  | None -> st
  | Some (bt,exn) ->
    (* Print the backtrace if requested *)
    if Printexc.backtrace_status () then
      Printexc.print_raw_backtrace stdout bt;
    let res = handle_exn st exn in
    raise (Finished res)

let process path opt_contents =
  let dir = Filename.dirname path in
  let file = Filename.basename path in
  let l_file : _ State.file = {
    lang = None; mode = None; dir;
    loc = Dolmen.Std.Loc.mk_file "";
    source =match opt_contents with
      | None -> `File file
      | Some contents -> `Raw (file, contents);
  } in
  let r_file : _ State.file = {
    lang = None; mode = None; dir;
    loc = Dolmen.Std.Loc.mk_file "";
    source = `Raw ("", "");
  } in
  let reports = Dolmen_loop.Report.Conf.mk ~default:Enabled in
  let set = State.set in
  let st =
    State.empty
    |> set State.debug false
    |> set State.report_style Regular
    |> set State.reports reports
    |> set State.max_warn max_int
    |> set State.cur_warn 0
    |> set State.time_limit 0. (* disables the timer *)
    |> set State.size_limit max_float
    |> set State.logic_file l_file
    |> set State.response_file r_file
    |> set Header.header_check false
    |> set Header.header_state Dolmen_loop.Headers.empty
    |> set Header.header_licenses []
    |> set Header.header_lang_version None
    |> set Typer_Pipe.type_check true
    |> set Typer.ty_state (Dolmen_loop.Typer.new_state ())
  in
  try
    let st, g = Parser.parse_logic [] st l_file in
    let open Pipeline in
    let st = run ~finally g st (
        (fix (op ~name:"expand" Parser.expand) (
            (op ~name:"headers" Header.inspect)
            @>>> (op ~name:"typecheck" Typer_Pipe.typecheck)
            @>|> (op (fun st _ -> st, ())) @>>> _end
          )
        )
      ) in
    Ok st
  with
  | Finished res -> res
  | exn -> handle_exn st exn

