
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

module Task = Linol_lwt.Task

let run () =
  let open Task.Infix in
  let s = new Server.dolmen_lsp_server in
  (* TODO: the task is the LSP server *)
  let task =
    Task.start ~descr:"top task"
      (fun _top_task ->
         let server = Linol_lwt.Jsonrpc2.create_stdio s in
         let* () =
           Task.run_sub ~descr:"lsp server" ~parent:_top_task
             (fun _ -> Linol_lwt.Jsonrpc2.run server _top_task)
           >>= Task.unwrap
         in
         Task.return ()
      )
  in
  match Task.run task with
  | Ok () -> 0
  | Error e ->
    let e = Printexc.to_string e in
    Printf.eprintf "error: %s\n%!" e;
    1

let () =
  let retcode = run () in
  exit retcode

