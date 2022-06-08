(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Lsp server class *)
(* ************************************************************************ *)

class dolmen_lsp_server =
  object(self)
    inherit Linol_lwt.Jsonrpc2.server

    (* one env per document *)
    val buffers: (Lsp.Types.DocumentUri.t, State.t) Hashtbl.t = Hashtbl.create 32

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
      match Loop.process uri (Some contents) with
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

    method on_notif_doc_did_close ~notify_back:_ d =
      Hashtbl.remove buffers d.uri;
      Linol_lwt.Jsonrpc2.IO.return ()

  end




