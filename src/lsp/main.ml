
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

let server =
  let open Cmdliner in
  let aux preludes =
    new Server.dolmen_lsp_server (Loop.mk_prelude preludes)
  in
  let preludes =
    let doc = "Choose prelude files to use." in
    Arg.(value & opt_all string [] & info ["p"; "prelude"] ~doc)
  in
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "dolmenls") Term.(const aux $ preludes)

let run () =
  match Cmdliner.Cmd.eval_value server with
  | Ok (`Version | `Help) ->
    exit 0
  | Error (`Parse | `Term | `Exn) ->
    exit Cmdliner.Cmd.Exit.cli_error
  | Ok (`Ok s) ->
    let server = Linol_lwt.Jsonrpc2.create_stdio s in
    let task = Linol_lwt.Jsonrpc2.run server in
    match Linol_lwt.run task with
    | () -> ()
    | exception e ->
      let e = Printexc.to_string e in
      Printf.eprintf "error: %s\n%!" e;
      exit 1

(* Finally, we actually run the server *)
let () = run ()

