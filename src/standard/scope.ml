
(* This file is free software, part of dolmen. See file "LICENSE" for more details. *)

type 'a binding =
  | Bound of 'a
  | Reserved

type 'a t = {
  bindings: 'a binding Name.Map.t;
}

let empty = { bindings = Name.Map.empty; }

let reserve name t =
  { (* t with *) bindings = Name.Map.add_set ~prefix:name Reserved t.bindings; }

let bind name value t =
  { (* t with *) bindings = Name.Map.add name (Bound value) t.bindings; }

let find_exn name t =
  Name.Map.find_exn name t.bindings

let find_opt name t =
  Name.Map.find_opt name t.bindings


