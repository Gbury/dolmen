
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
  | Dolmen_loop.Alarm.Out_of_time -> Error "timeout"
  | Dolmen_loop.Alarm.Out_of_space -> Error "memoryout"
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

let process preludes path opt_contents =
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
  let st =
    State.empty
    |> State.init
      ~debug:false ~report_style:Regular ~reports
      ~max_warn:max_int ~time_limit:0. ~size_limit:max_float
      ~response_file:r_file
    |> Parser.init ~syntax_error_ref:false
    |> Typer.init
    |> Typer_Pipe.init ~type_check:true
    |> Header.init
      ~header_check:false
      ~header_licenses:[]
      ~header_lang_version:None
  in
  try
    let g = Parser.parse_logic ~preludes l_file in
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
