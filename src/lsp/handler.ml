
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Some module aliases and small stuff *)
(* ************************************************************************ *)

let section = "dolmen_lsp"

module Res_ = struct

  let (>|=) x f = match x with
    | Error e -> Error e
    | Ok x -> Ok (f x)

  let (>>=) x f = match x with
    | Error e -> Error e
    | Ok x -> f x

  let wrap f =
    Fiber.with_error_handler (fun () ->
        match f () with
        | Ok res ->
          Fiber.return res
        | Error msg ->
          Lsp.Logger.log ~section ~title:Error
            "Error: %s" msg;
          Fiber.never
      ) ~on_error:(function { Stdune.Exn_with_backtrace.exn; backtrace; } ->
        Lsp.Logger.log ~section ~title:Error
          "Uncaught exn: %s@\n%s"
          (Printexc.to_string exn)
          (Printexc.raw_backtrace_to_string backtrace)
      )
end

module Doc = Lsp.Text_document

module Umap = Map.Make(struct
    type t = Lsp.Types.DocumentUri.t
    let compare = String.compare
  end)

(* Main handler state *)
(* ************************************************************************ *)

type file_mode =
  | Read_from_disk
  | Compute_incremental

type diag_mode =
  | On_change
  | On_did_save
  | On_will_save

type t = {

  (* Mode of execution *)
  file_mode : file_mode;
  diag_mode : diag_mode;

  (* Document tracking *)
  docs: Doc.t Umap.t;
}

let empty = {
  file_mode = Compute_incremental;
  diag_mode = On_will_save;
  docs = Umap.empty;
}

(* Manipulating documents *)
(* ************************************************************************ *)

let add_doc t uri doc =
  let t' = { t with docs = Umap.add uri doc t.docs; } in
  Ok t'

let open_doc t (d : Lsp.Types.DidOpenTextDocumentParams.t) =
  let doc = Doc.make d in
  add_doc t d.textDocument.uri doc

let close_doc t uri =
  { t with docs = Umap.remove uri t.docs; }

let fetch_doc t uri =
  match Umap.find uri t.docs with
  | doc -> Ok doc
  | exception Not_found ->
    Error "uri not found"

let fetch_version t uri =
  let open Res_ in
  fetch_doc t uri >|= Doc.version

let change_doc t version uri changes =
  let open Res_ in
  fetch_doc t uri >>= fun doc ->
  let doc' = List.fold_right (Doc.apply_content_change ?version) changes doc in
  add_doc t uri doc'

(* Document processing *)
(* ************************************************************************ *)

let process state uri =
  Lsp.Logger.log ~section ~title:Debug
    "Starting processing of %s" uri;
  (* TODO: translate uri to path ? *)
  let path = uri in
  let open Res_ in
  begin match state.file_mode with
    | Read_from_disk ->
      Lsp.Logger.log ~section ~title:Debug
        "reading from disk...";
      Ok (None, None)
    | Compute_incremental ->
      Lsp.Logger.log ~section ~title:Debug
        "Fetching computed document...";
      fetch_doc state uri >|= fun doc ->
      Some (Doc.version doc), Some (Doc.text doc)
  end >>= fun (version, contents) ->
  Loop.process path contents >>= fun st ->
  let diags = st.solve_state in
  Ok (version, diags)

(* Initialization *)
(* ************************************************************************ *)

let on_initialize _server state (params : Lsp.Types.InitializeParams.t) =
  Lsp.Logger.log ~section ~title:Debug "Initialization started";
  (* Determine in which mode we are *)
  let diag_mode =
    match params.capabilities.textDocument with
    | Some { synchronization = Some { willSave = Some true; _ }; _ } ->
      Lsp.Logger.log ~section ~title:Info "Setting mode: on_will_save";
      On_will_save
    | Some { synchronization = Some { didSave = Some true; _ }; _ } ->
      Lsp.Logger.log ~section ~title:Info "Setting mode: on_did_save";
      On_did_save
    | _ ->
      Lsp.Logger.log ~section ~title:Info "Setting mode: on_change";
      On_change
  in
  (* New state *)
  let state = { state with diag_mode; } in
  (* Create the capabilities answer *)
  let serverInfo = Lsp.Types.InitializeResult.create_serverInfo ~name:"dolmenls" () in
  let capabilities =
    Lsp.Types.ServerCapabilities.create
      ~textDocumentSync:(`TextDocumentSyncOptions (
          Lsp.Types.TextDocumentSyncOptions.create
          (* Request willSave notifications, better then didSave notification,
             because it is sent earlier (before the editor has to write the full
             file), but still on save sfrom the user. *)
            ~willSave:(match state.diag_mode with
                | On_change -> false
                | On_did_save -> false
                | On_will_save -> true
              )
            (* Request didSave notifiations. *)
            ?save:(match state.diag_mode with
                | On_change -> None
                | On_will_save -> None
                | On_did_save ->
                  Some (Lsp.Types.SaveOptions.create ~includeText:false ())
              )
            (* NoSync is bugged under vim-lsp where it send null instead
               of an empty list, so this is set to incremental even for
               read_from_disk mode *)
            ~change:Incremental
            ())
        ) ()
  in
  (* Return the new state and answer *)
  let result = Lsp.Types.InitializeResult.create ~serverInfo ~capabilities () in
  Ok (result, state)

(* Request handler *)
(* ************************************************************************ *)

let on_request : t Lsp.Server.Handler.on_request =
  let on_request : type a.
    'state Lsp.Server.t ->
    a Lsp.Server.in_request ->
    (a * 'state, Lsp.Jsonrpc.Response.Error.t) result Fiber.t
    = fun server request ->
      let state = Lsp.Server.state server in
      match (request : a Lsp.Server.in_request) with
      | Initialize params ->
        Fiber.return (on_initialize server state params)
      | _ ->
        Fiber.return @@ Error (
          Lsp.Jsonrpc.Response.Error.make
            ~code:InternalError ~message:"not implemented" ()
        )
  in
  { on_request }


(* Notification handler *)
(* ************************************************************************ *)

let send_diagnostics server uri (version, l) =
  Lsp.Logger.log ~section ~title:Debug
    "sending diagnostics list (length %d)" (List.length l);
  Lsp.Server.notification server @@
  Lsp.Server_notification.PublishDiagnostics (
    Lsp.Types.PublishDiagnosticsParams.create ~uri ?version ~diagnostics:l ()
  )

let process_and_send server state uri =
  let open Fiber.O in
  Res_.wrap (fun () -> process state uri)
  >>= send_diagnostics server uri
  >>= fun () -> Fiber.return state

let on_will_save server state (d : Lsp.Types.WillSaveTextDocumentParams.t) =
  Lsp.Logger.log ~section ~title:Debug "will save / uri %s" d.textDocument.uri;
  match state.diag_mode, state.file_mode with
  | On_will_save, Compute_incremental ->
    process_and_send server state d.textDocument.uri
  | On_will_save, Read_from_disk ->
    Res_.wrap (fun () -> Error "incoherent internal state")
  | _ ->
    Fiber.return state

let on_did_save server state (d : Lsp.Types.DidSaveTextDocumentParams.t) =
  Lsp.Logger.log ~section ~title:Debug "did save / uri %s" d.textDocument.uri;
  match state.diag_mode with
  | On_did_save -> process_and_send server state d.textDocument.uri
  | _ -> Fiber.return state

let on_did_close _server state (d : Lsp.Types.DidCloseTextDocumentParams.t) =
  Lsp.Logger.log ~section ~title:Debug "did close / uri %s" d.textDocument.uri;
  Fiber.return (close_doc state d.textDocument.uri)

let on_did_open server state (d : Lsp.Types.DidOpenTextDocumentParams.t) =
  Lsp.Logger.log ~section ~title:Debug "did open / uri %s, size %d"
    d.textDocument.uri (String.length d.textDocument.text);
  (* Register the doc in the state if in incremental mode *)
  begin match state.file_mode with
    | Read_from_disk -> Ok state
    | Compute_incremental -> open_doc state d
  end |> function
  | Ok state -> process_and_send server state d.textDocument.uri
  | Error _ as res -> Res_.wrap (fun () -> res)

let on_did_change server state (d : Lsp.Types.DidChangeTextDocumentParams.t) =
  let open Fiber.O in
  Lsp.Logger.log ~section ~title:Debug "did change / uri %s" d.textDocument.uri;
  (* Update the doc in the state if in incremental mode *)
  Res_.wrap (fun () -> match state.file_mode with
    | Read_from_disk -> Ok state
    | Compute_incremental ->
      change_doc state d.textDocument.version d.textDocument.uri d.contentChanges
    ) >>= fun state ->
  (* Process and send if in on_change mode *)
  begin match state.diag_mode with
    | On_change -> process_and_send server state d.textDocument.uri
    | On_did_save
    | On_will_save -> Fiber.return state
  end

let on_notification server in_notification =
  let state = Lsp.Server.state server in
  match (in_notification : Lsp.Client_notification.t) with

  (* Initialization, not much to do *)
  | Initialized ->
    Lsp.Logger.log ~section ~title:Debug "initializing";
    Fiber.return state

  (* New document *)
  | TextDocumentDidOpen params ->
    on_did_open server state params
  (* Close document *)
  | TextDocumentDidClose params ->
    on_did_close server state params

  (* Document changes *)
  | TextDocumentDidChange params ->
    on_did_change server state params
  (* will save *)
  | WillSaveTextDocument params ->
    on_will_save server state params
  (* did save *)
  | DidSaveTextDocument params ->
    on_did_save server state params

  (* Exit *)
  | Exit -> Fiber.return state

  (* TODO stuff *)
  | ChangeWorkspaceFolders _ ->
    Lsp.Logger.log ~section ~title:Debug "workspace change";
    Fiber.return state
  | ChangeConfiguration _ ->
    Lsp.Logger.log ~section ~title:Debug "change config";
    (* Error "not implemented" *)
    Fiber.return state
  | Unknown_notification _req ->
    Lsp.Logger.log ~section ~title:Debug "unknown notification";
    (* Error "not implemented" *)
    Fiber.return state


(* Lsp Handler *)
(* ************************************************************************ *)

let handler : t Lsp.Server.Handler.t =
  Lsp.Server.Handler.make ~on_request ~on_notification ()

