(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Lsp server class *)
(* ************************************************************************ *)

exception ServerError of string

let parse_settings settings =
  match settings with
  | `Assoc [ "preludes", `List l ] ->
    List.fold_left (
      fun acc s ->
        match s with
        | `String f -> f :: acc
        | _ ->
          raise (ServerError (
              Format.asprintf
                "Expected a path to a prelude file as a string, \
                 instead got %a@."
                Yojson.Safe.pp s))
    ) [] l
  | _ ->
    raise (ServerError (
        Format.asprintf
          "ChangeConfiguration: received unexpected settings: \n%a@."
          Yojson.Safe.pp settings))

let preprocess_uri uri =
  let prefix = "file://" in
  let prefix_len = String.length prefix in
  let uri_len = String.length uri in
  if prefix_len < uri_len && (String.sub uri 0 prefix_len) = prefix
  then String.sub uri prefix_len (uri_len - prefix_len)
  else uri

let mk_prelude files =
  List.map (
    fun f ->
      let dir, file = Dolmen_loop.State.split_input (`File f) in
      Dolmen_loop.State.mk_file dir file
  ) files

class dolmen_lsp_server =
  object(self)
    inherit Linol_lwt.Jsonrpc2.server

    (* one env per document *)
    val buffers: (Lsp.Types.DocumentUri.t, State.t) Hashtbl.t = Hashtbl.create 32

    (* A list of include statements of the prelude files *)
    val mutable prelude = []

    method! config_sync_opts =
      (* configure how sync happens *)
      let change = Lsp.Types.TextDocumentSyncKind.Incremental in
      (* Lsp.Types.TextDocumentSyncKind.Full *)
      Lsp.Types.TextDocumentSyncOptions.create ~openClose:true ~change
        ~save:(Lsp.Types.SaveOptions.create ~includeText:false ())
        ()

    method private _on_doc
        ~(notify_back:Linol_lwt.Jsonrpc2.notify_back)
        (uri:Lsp.Types.DocumentUri.t) (contents:string) =
      (* TODO: unescape uri/translate it to a correct path ? *)
      match Loop.process prelude (preprocess_uri uri) (Some contents) with
      | Ok state ->
        let diags = State.get State.diagnostics state in
        Hashtbl.replace buffers uri state;
        notify_back#send_diagnostic diags
      | Error msg ->
        Linol_lwt.Jsonrpc2.IO.failwith (
          Format.asprintf "Internal dolmen error: %s" msg
        )

    method on_notif_doc_did_open ~notify_back d ~content =
      self#_on_doc ~notify_back d.uri content

    method on_notif_doc_did_change ~notify_back d _c ~old_content:_old ~new_content =
      self#_on_doc ~notify_back d.uri new_content

    method! on_notification_unhandled
        ~notify_back:_ (n:Lsp.Client_notification.t) =
      match n with
      | Lsp.Client_notification.ChangeConfiguration { settings; } ->
        begin try
            prelude <- mk_prelude (parse_settings settings);
            Linol_lwt.Jsonrpc2.IO.return ()
          with ServerError s ->
            Linol_lwt.Jsonrpc2.IO.failwith s
        end
      | _ ->
        Lwt.return ()

    method on_notif_doc_did_close ~notify_back d =
      Hashtbl.remove buffers d.uri;
      notify_back#send_diagnostic []

  end
