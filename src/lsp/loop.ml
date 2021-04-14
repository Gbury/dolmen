
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

  (* Simple error cases *)
  | Pipeline.Sigint -> Error "user interrupt"
  | Pipeline.Out_of_time -> Error "timeout"
  | Pipeline.Out_of_space -> Error "memoryout"
  (* Exn during parsing *)
  | Dolmen.Std.Loc.Uncaught (loc, exn, _) ->
    let file = State.input_file_loc st in
    let loc = Dolmen.Std.Loc.loc file loc in
    Error (Format.asprintf "%a: %s"
             Dolmen.Std.Loc.fmt loc (Printexc.to_string exn))

  (* lexing error *)
  | Dolmen.Std.Loc.Lexing_error (loc, msg) ->
    let file = State.input_file_loc st in
    let loc = { Dolmen.Std.Loc.file; loc; } in
    Ok (State.error ~loc st "Lexing error: %s" msg)
  (* Parsing error *)
  | Dolmen.Std.Loc.Syntax_error (loc, `Regular msg) ->
    let file = State.input_file_loc st in
    let loc = { Dolmen.Std.Loc.file; loc; } in
    Ok (State.error ~loc st "%t" msg)
  | Dolmen.Std.Loc.Syntax_error (loc, `Advanced (_, _, expected)) ->
    let file = State.input_file_loc st in
    let loc = { Dolmen.Std.Loc.file; loc; } in
    Ok (State.error ~loc st "Syntax error: expected %t" expected)
  (* Typing error *)
  | Dolmen_loop.Typer.T.Typing_error (
      Dolmen_loop.Typer.T.Error (env, fragment, _err) as error) ->
    let loc = Dolmen_loop.Typer.T.fragment_loc env fragment in
    Ok (State.error ~loc st "Typing error: %a" Typer.report_error error)

  (* File not found *)
  | State.File_not_found (loc, dir, f) ->
    Ok (State.error ~loc st "File not found: '%s' in directory '%s'" f dir)
  (* Input lang changed *)
  | State.Input_lang_changed _ ->
    Ok (State.error st "Language changed because of an include")

  (* Fallback *)
  | exn ->
    Ok (State.error st
          "Internal error, please report upstream: %s"
          (Printexc.to_string exn))

let finally st e =
  match e with
  | None -> st
  | Some exn ->
    let res = handle_exn st exn in
    raise (Finished res)

let process path opt_contents =
  let dir = Filename.dirname path in
  let file = Filename.basename path in
  let st = State.{
      debug = false;
      max_warn = max_int;
      cur_warn = 0;
      context = false;
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
      header_check = true;
      header_licenses = [];
      header_lang_version = None;
      header_state = Dolmen_loop.Headers.empty;
      type_state = Dolmen_loop.Typer.new_state ();
      type_check = true;
      type_strict = true;
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

