
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

open Result

module Pipeline = Dolmen_loop.Pipeline.Make(State)
module Pipe = Dolmen_loop.Pipes.Make(Dolmen.Expr)(State)(State.Typer)

exception Finished of (State.t, string) result

let no_loc = Dolmen.ParseLocation.mk "" 0 0 0 0
let get_loc = function
  | Some l -> l
  | None -> no_loc

let handle_exn st = function

  (* Simple error cases *)
  | Pipeline.Sigint -> Error "user interrupt"
  | Pipeline.Out_of_time -> Error "timeout"
  | Pipeline.Out_of_space -> Error "memoryout"
  (* Exn during parsing *)
  | Dolmen.ParseLocation.Uncaught (loc, exn) ->
    Error (Format.asprintf "%a: %s"
             Dolmen.ParseLocation.fmt loc (Printexc.to_string exn))

  (* lexing error *)
  | Dolmen.ParseLocation.Lexing_error (loc, msg) ->
    Ok (State.error st loc "Lexing error: %s" msg)
  (* Parsing error *)
  | Dolmen.ParseLocation.Syntax_error (loc, msg) ->
    Ok (State.error st loc "Syntax_error: %s" msg)
  (* Typing error *)
  | State.Typer.T.Typing_error (err, _, t) ->
    let loc = get_loc t.Dolmen.Term.loc in
    Ok (State.error st loc "%a" State.Typer.report_error err)

  (* File not found *)
  | Dolmen_loop.State.File_not_found (l, dir, f) ->
    Ok (State.error st (get_loc l) "File not found: '%s' in directory '%s'" f dir)

  (* Fallback *)
  | exn ->
    Error (Format.asprintf "unknown exn: %s" (Printexc.to_string exn))

let finally st e =
  match e with
  | None -> st
  | Some exn ->
    let res = handle_exn st exn in
    raise (Finished res)

let process path =
  let dir = Filename.dirname path in
  let file = Filename.basename path in
  let st = Dolmen.State.{
      time_limit = Float.max_float;
      size_limit = Float.max_float;
      input_dir = dir;
      input_lang = None;
      input_mode = None;
      input_source = `File file;
      type_state = State.Typer.T.new_state ();
      type_check = true;
      type_infer = None;
      type_shadow = None;
      type_smtlib_logic = None;
      solve_state = [];
      export_lang = None;
    } in
  try
    let st, g = Pipe.parse [] st in
    let open Pipeline in
    let st = run ~finally g st (
        (fix (apply ~name:"expand" Pipe.expand) (
            (apply ~name:"typecheck" Pipe.typecheck)
            @>|> ((apply fst) @>>> _end)
          )
        )
      ) in
    Ok st
  with
  | Finished res -> res
  | exn -> handle_exn st exn

