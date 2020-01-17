
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Initialization *)
(* ************************************************************************ *)

let on_initialize _rpc _state _params =
  Error "not implemented"

(* Request handler *)
(* ************************************************************************ *)

let on_request _rpc _state _capabilities _req =
  Error "not implemented"

(* Notification handler *)
(* ************************************************************************ *)

let on_notification _rpc _state _notification =
  Error "not implemented"

(* Lsp Handler *)
(* ************************************************************************ *)

let handler : State.t Lsp.Rpc.handler = {
    on_initialize;
    on_request;
    on_notification;
  }

