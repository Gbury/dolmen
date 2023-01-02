
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

let run () =
  let s = new Server.dolmen_lsp_server in
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

