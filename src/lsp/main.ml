
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

type listen =
  | Stdio
  | Server of string * int

let run ic oc =
  let section = Printf.sprintf "%s[%d]" Handler.section @@ Thread.id (Thread.self ()) in
  Lsp.Logger.log ~section ~title:Debug "start lsp";
  Lsp.Rpc.start Handler.empty Handler.handler ic oc;
  Lsp.Logger.log ~section ~title:Debug "stop lsp";
  ()

let main_tcp addr port =
  let sock = Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt_optint sock Unix.SO_LINGER None;
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  let inet_addr = Unix.inet_addr_of_string addr in
  Unix.bind sock (Unix.ADDR_INET (inet_addr, port));
  Unix.listen sock 10;
  while true do
    let client_sock, _ = Unix.accept sock in
    let ic = Unix.in_channel_of_descr client_sock in
    let oc = Unix.out_channel_of_descr client_sock in
    let _th = Thread.create (fun () -> run ic oc) () in
    ()
  done;
  ()

let main ~listen () : unit =
  begin match listen with
    | Stdio -> run stdin stdout
    | Server (addr,port) ->
      main_tcp addr port
  end

let () =
  let stdio = ref true in
  let host = ref "0.0.0.0" in
  let port = ref 8854 in
  let opts = [
    "--host", Arg.Set_string host, " address to listen in";
    "--port", Arg.Set_int port, " port to listen on";
    "--stdio", Arg.Set stdio, " connection on stdio";
    "--tcp", Arg.Clear stdio, " connections on TCP";
  ] |> Arg.align in
  Arg.parse opts (fun _ -> raise (Arg.Bad "no such arg")) "dolmenls [option*]";
  let listen = if !stdio then Stdio else Server (!host, !port) in
  Lsp.Logger.with_log_file (Some "/tmp/lsp.log") (main ~listen)

