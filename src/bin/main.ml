
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

let handle_exn st exn =
  let _st = Errors.exn st exn in
  exit 125

let finally st e =
  match e with
  | None -> st
  | Some (bt,exn) ->
    (* Print the backtrace if requested *)
    if Printexc.backtrace_status () then
      Printexc.print_raw_backtrace stdout bt;
    handle_exn st exn

let debug_pre_pipe st c =
  if st.Loop.State.debug then
    Format.eprintf "[pre] @[<hov>%a@]@."
      Dolmen.Std.Statement.print c;
  st, c

let debug_post_pipe st stmt =
  if st.Loop.State.debug then
    Format.eprintf "[post] @[<hov>%a@]@\n@."
      Loop.Typer.print stmt;
  st, stmt

let () =
  let exits =
    List.map (fun code ->
        let retcode, doc = Dolmen_loop.Code.descr code in
        Cmdliner.Term.exit_info ~doc retcode
      ) (Dolmen_loop.Code.errors ())
    @ Cmdliner.Term.default_exits
  in
  let man = [
    `S Cmdliner.Manpage.s_description;
    `P "Dolmen is a tool to parse and type input files that contain problem \
        used in automated deduction.";
    `S Options.common_section;
    `P "Common options for the dolmen binary";
    `S Options.error_section;
    `P "Options to customize the behaviour of dolmen on errors/warnings";
    `P "A warning can have one of three status: Disabled, Enabled, and Fatal. \
        When disabled, a warning will be ignored, when enabled, it will be
        printed, and when fatal, it will be transformed into an error.";
    `S Options.header_section;
    `P "Options to control the checking of headers in the input file";
    `S Options.profiling_section;
    `P (Format.asprintf
          "Options to profile Dolmen.%s"
          (if Memory_profiler.available then "" else
             " WARNING: Memory profiling is not available on this version
            of Dolmen. You should install memtrace and recompile Dolmen
            if you desire to use memory profiling.")
       );
    `S Options.gc_section;
    `P "Options to fine-tune the gc, only experts should use these.";
    `S Cmdliner.Manpage.s_exit_status;
    `P "dolmen exits with the following status:";
    `S Cmdliner.Manpage.s_bugs;
    `P "You can report bugs at https://github.com/Gbury/dolmen/issues";
    `S Cmdliner.Manpage.s_authors;
    `P "Guillaume Bury <guillaume.bury@gmail.com>"
  ] in
  let info = Cmdliner.Term.info ~exits ~man ~version:"0.1" "dolmen" in
  let st = match Cmdliner.Term.eval (Options.state, info) with
    | `Version | `Help ->
      exit 0
    | `Error `Parse | `Error `Term | `Error `Exn ->
      exit Cmdliner.Term.exit_status_cli_error
    | `Ok opt -> opt
  in
  if st.Loop.State.debug then
    Dolmen.Std.Expr.Print.print_index := true;
  let st, g =
    try Loop.Parser.parse [] st
    with exn -> handle_exn st exn
  in
  let st =
    let open Loop.Pipeline in
    run ~finally g st (
      (fix (op ~name:"expand" Loop.Parser.expand) (
          (op ~name:"debug_pre" debug_pre_pipe)
          @>>> (op ~name:"headers" Loop.Header.inspect)
          @>>> (op ~name:"typecheck" Loop.Typer.typecheck)
          @>|> (op ~name:"debug_post" debug_post_pipe)
          @>>> (op (fun st _ -> st, ())) @>>> _end
        )
      )
    )
  in
  let st = Loop.Header.check st in
  let _st = Dolmen_loop.State.flush st () in
  ()

