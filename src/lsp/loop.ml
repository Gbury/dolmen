
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

module Pipeline = Dolmen_loop.Pipeline.Make(State)

module Parser = Dolmen_loop.Parser.Pipe(Dolmen.Std.Expr)(State)
module Header = Dolmen_loop.Headers.Pipe(State)
module Typer = struct
  module T = Dolmen_loop.Typer.Make(State)
  include T
  include Dolmen_loop.Typer.Pipe(Dolmen.Std.Expr)(Dolmen.Std.Expr.Print)(State)(T)
end

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
  let reports = Dolmen_loop.Report.Conf.mk ~default:Enabled in
  let st = State.{
      debug = false;
      reports; loc_style = `Short;
      max_warn = max_int;
      cur_warn = 0;
      time_limit = 0.; (* disable the timer *)
      size_limit = max_float;
      input_dir = dir;
      input_lang = None;
      input_mode = None;
      input_source = begin match opt_contents with
        | None -> `File file
        | Some contents -> `Raw (file, contents)
      end;
      input_file_loc = Dolmen.Std.Loc.mk_file "";
      header_check = false;
      header_licenses = [];
      header_lang_version = None;
      header_state = Dolmen_loop.Headers.empty;
      type_state = Dolmen_loop.Typer.new_state ();
      type_check = true;
      solve_state = [];
      export_lang = [];
    } in
  try
    let st, g = Parser.parse [] st in
    let open Pipeline in
    let st = run ~finally g st (
        (fix (op ~name:"expand" Parser.expand) (
            (op ~name:"headers" Header.inspect)
            @>>> (op ~name:"typecheck" Typer.typecheck)
            @>|> (op (fun st _ -> st, ())) @>>> _end
          )
        )
      ) in
    Ok st
  with
  | Finished res -> res
  | exn -> handle_exn st exn

