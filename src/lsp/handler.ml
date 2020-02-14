
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
end

module Loc = Dolmen.ParseLocation

module N = Lsp.Client_notification
module Doc = Lsp.Text_document

module Umap = Map.Make(struct
    type t = Lsp.Uri.t
    let compare a b =
      String.compare (Lsp.Uri.to_path a) (Lsp.Uri.to_path b)
  end)

(* Main handler state *)
(* ************************************************************************ *)

type file_mode =
  | Read_from_disk
  | Compute_incremental

type diag_mode =
  | On_save
  | On_change

type res = {
  diags : Lsp.Protocol.PublishDiagnostics.params list;
}

type t = {

  (* Mode of execution *)
  file_mode : file_mode;
  diag_mode : diag_mode;

  (* Document tracking *)
  docs: Doc.t Umap.t;
  processed: res Umap.t;
}

let empty = {
  file_mode = Compute_incremental;
  diag_mode = On_save;
  docs = Umap.empty;
  processed = Umap.empty;
}

(* Manipulating documents *)
(* ************************************************************************ *)

let add_doc t uri doc =
  let t' = { t with docs = Umap.add uri doc t.docs; } in
  Ok t'

let open_doc t version uri text =
  let doc = Doc.make ~version uri text in
  add_doc t uri doc

let fetch_doc t uri =
  match Umap.find uri t.docs with
  | doc -> Ok doc
  | exception Not_found ->
    Error "uri not found"

let apply_changes version doc changes =
  List.fold_right (Doc.apply_content_change ~version) changes doc

let change_doc t version uri changes =
  let open Res_ in
  fetch_doc t uri >>= fun doc ->
  let doc' = apply_changes version doc changes in
  add_doc t uri doc'


(* Document processing *)
(* ************************************************************************ *)

let process state uri =
  Lsp.Logger.log ~section ~title:"processing"
    "Starting processing of %s" (Lsp.Uri.to_path uri);
  let path = Lsp.Uri.to_path uri in
  let open Res_ in
  begin match state.file_mode with
    | Read_from_disk ->
      Lsp.Logger.log ~section ~title:"processing"
        "reading from disk...";
      Ok None
    | Compute_incremental ->
      Lsp.Logger.log ~section ~title:"processing"
        "Fetching computed document...";
      fetch_doc state uri >|= fun doc ->
      Some (Doc.text doc)
  end >>= fun contents ->
  Loop.process path contents >>= fun st ->
  let diags = st.solve_state in
  Ok (diags)

(* Initialization *)
(* ************************************************************************ *)

let on_initialize _rpc state (params : Lsp.Initialize.Params.t) =
  Lsp.Logger.log ~section ~title:"initialize" "Initialization started";
  (* Determine in which mode we are *)
  let diag_mode =
    if params.capabilities.textDocument.synchronization.didSave then begin
      Lsp.Logger.log ~section ~title:"initialize"
        "Setting mode: on_save";
      On_save
    end else begin
      Lsp.Logger.log ~section ~title:"initialize"
        "Setting mode: on_change";
      On_change
    end
  in
  (* New state *)
  let state = { state with diag_mode; } in
  (* Create the capabilities answer *)
  let info = Lsp.Initialize.Info.{ name = "dolmenls"; version = None; } in
  let default = Lsp.Initialize.ServerCapabilities.default in
  let capabilities =
    { default with
      textDocumentSync = {
        default.textDocumentSync with
        (* Request willSave notifications, better then didSave notification,
           because it is sent earlier (before the editor has to write the full
           file), but still on save sfrom the user. *)
        willSave = begin match state.diag_mode with
          | On_change -> false
          | On_save -> true
        end;
        save = None;
        (* begin match state.diag_mode with
          | On_change -> None
          | On_save -> Some {
              Lsp.Initialize.TextDocumentSyncOptions.includeText = false; }
        end; *)
        (* NoSync is bugged under vim-lsp where it send null instead
           of an empty list, so this is set to incremental even for
           read_from_disk mode *)
        change = IncrementalSync;
      };
    } in
  (* Return the new state and answer *)
  let result = Lsp.Initialize.Result.{ serverInfo = Some info; capabilities; } in
  Ok (state, result)

(* Request handler *)
(* ************************************************************************ *)

let on_request _rpc _state _capabilities _req =
  Error "not implemented"

(* Notification handler *)
(* ************************************************************************ *)

let send_diagnostics rpc uri version l =
  Lsp.Logger.log ~section ~title:"sendDiagnostics"
    "sending diagnostics list (length %d)" (List.length l);
  Lsp.Rpc.send_notification rpc @@
  Lsp.Server_notification.PublishDiagnostics {
    uri; version;
    diagnostics = l;
  }

let process_and_send rpc state uri =
  let open Res_ in
  process state uri >>= fun l ->
  send_diagnostics rpc uri None l;
  Ok state

let on_will_save rpc state (d : Lsp.Protocol.TextDocumentIdentifier.t) =
  let open Res_ in
  Lsp.Logger.log ~section ~title:"docWillSave" "uri %s" (Lsp.Uri.to_path d.uri);
  match state.diag_mode, state.file_mode with
  | On_save, Compute_incremental -> process_and_send rpc state d.uri
  | On_save, Read_from_disk -> Ok state (* wait for the save write to finish *)
  | On_change, _ -> Ok state

let on_did_save rpc state (d : Lsp.Protocol.TextDocumentIdentifier.t) =
  let open Res_ in
  Lsp.Logger.log ~section ~title:"docDidSave" "uri %s" (Lsp.Uri.to_path d.uri);
  match state.diag_mode, state.file_mode with
  | On_save, Compute_incremental -> Ok state (* processing done on willSave *)
  | On_save, Read_from_disk -> process_and_send rpc state d.uri
  | On_change, _ -> Ok state

let on_did_open rpc state (d : Lsp.Protocol.TextDocumentItem.t) =
  let open Res_ in
  Lsp.Logger.log ~section ~title:"docDidOpen" "uri %s, size %d"
    (Lsp.Uri.to_path d.uri) (String.length d.text);
  (* Register the doc in the state if in incremental mode *)
  begin match state.file_mode with
    | Read_from_disk -> Ok state
    | Compute_incremental -> open_doc state d.version d.uri d.text
  end >>= fun state ->
  (* Process and send if in on_change mode *)
  begin match state.diag_mode with
    | On_change -> process_and_send rpc state d.uri
    | On_save -> process_and_send rpc state d.uri
  end

let on_did_change rpc state
    (d : Lsp.Protocol.VersionedTextDocumentIdentifier.t) changes =
  let open Res_ in
  Lsp.Logger.log ~section ~title:"docDidChange"
    "uri %s" (Lsp.Uri.to_path d.uri);
  (* Update the doc in the state if in incremental mode *)
  begin match state.file_mode with
    | Read_from_disk -> Ok state
    | Compute_incremental -> change_doc state d.version d.uri changes
  end >>= fun state ->
  (* Process and send if in on_change mode *)
  begin match state.diag_mode with
    | On_change -> process_and_send rpc state d.uri
    | On_save -> Ok state
  end

let on_notification rpc state = function

  (* Initialization, not much to do *)
  | N.Initialized ->
    Lsp.Logger.log ~section ~title:"initialized" "ok";
    Ok state

  (* New document *)
  | N.TextDocumentDidOpen { textDocument=d } ->
    on_did_open rpc state d
  (* Document changes *)
  | N.TextDocumentDidChange { textDocument; contentChanges; } ->
    on_did_change rpc state textDocument contentChanges
  (* will save *)
  | N.WillSaveTextDocument { textDocument=d; _ } ->
    on_will_save rpc state d
  (* did save *)
  | N.DidSaveTextDocument { textDocument=d; _ } ->
    on_did_save rpc state d

  (* Exit *)
  | N.Exit -> Ok state

  (* TODO stuff *)
  | N.ChangeWorkspaceFolders _ ->
    Lsp.Logger.log ~section ~title:"on-change_workspace" "nothing to do";
    Ok state
  | N.ChangeConfiguration _ ->
    Lsp.Logger.log ~section ~title:"on-change_config" "unhandled notification";
    Error "not implemented"
  | N.Unknown_notification _req ->
    Lsp.Logger.log ~section ~title:"on-unknown_notif" "don't know what to do";
    Error "not implemented"


(* Lsp Handler *)
(* ************************************************************************ *)

let handler : t Lsp.Rpc.handler = {
  on_initialize;
  on_request;
  on_notification;
}

