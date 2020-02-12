
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

open Result

(* Some module aliases and small stuff *)
(* ************************************************************************ *)

let section = "dolmen_lsp"

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

type res = {
  diags : Lsp.Protocol.PublishDiagnostics.params list;
}

type t = {
  processed: res Umap.t;
}

let empty = {
  processed = Umap.empty;
}

(* Document processing *)
(* ************************************************************************ *)

let process state uri =
  let path = Lsp.Uri.to_path uri in
  let+ st = Loop.process path in
  let diags = st.solve_state in
  Ok (diags, state)

(* Initialization *)
(* ************************************************************************ *)

let on_initialize _rpc state _params =
  Lsp.Logger.log ~section ~title:"initialize" "Initialization succesful";
  let info = Lsp.Initialize.Info.{ name = "dolmenls"; version = None; } in
  let default = Lsp.Initialize.ServerCapabilities.default in
  let capabilities =
    { default with
      textDocumentSync = {
        default.textDocumentSync with
        didSave = Some {
            Lsp.Initialize.TextDocumentSyncOptions.includeText = false; };
        change = NoSync;
      };
    } in
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

let on_save rpc state (d : Lsp.Protocol.TextDocumentIdentifier.t) =
  let+ res, state = process state d.uri in
  let () = send_diagnostics rpc d.uri None res in
  Ok state

let on_notification rpc state = function

  (* Initialization, not much to do *)
  | N.Initialized ->
    Lsp.Logger.log ~section ~title:"initialized" "ok";
    Ok state

  (* New document *)
  | N.TextDocumentDidOpen _ ->
    Lsp.Logger.log ~section ~title:"doc open" "Document opened succesfully";
    Ok state
  | N.TextDocumentDidChange _ ->
    Lsp.Logger.log ~section ~title:"doc change" "Document changed succesfully";
    Ok state

  (* Document saving *)
  | N.DidSaveTextDocument { textDocument=d; _ } ->
    Lsp.Logger.log ~section ~title:"doc saved" "Document saved";
    on_save rpc state d

  (* Exit *)
  | N.Exit -> Ok state

  (* TODO stuff *)
  | N.WillSaveTextDocument _
  | N.ChangeWorkspaceFolders _
  | N.ChangeConfiguration _ ->
    Lsp.Logger.log ~section ~title:"on-notif" "unhandled notif";
    Error "not implemented"
  | N.Unknown_notification _req ->
    Lsp.Logger.log ~section ~title:"on-notif" "unknown notif";
    Error "not implemented"


(* Lsp Handler *)
(* ************************************************************************ *)

let handler : t Lsp.Rpc.handler = {
  on_initialize;
  on_request;
  on_notification;
}

