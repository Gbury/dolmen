
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

type namespace =
  | Var
  | Sort
  | Term
  | Attr
  | Decl
  | Module of string

type t = {
  ns : namespace;
  name : string;
}

let hash = Hashtbl.hash
let compare = Pervasives.compare
let equal = Pervasives.(=)

let pp b { ns; name } =
  Printf.bprintf b "%s" name

let print fmt { ns; name } =
  Format.fprintf fmt "%s" name

(* Namespaces *)
(* let var = Var *)
let sort = Sort
let term = Term
let attr = Attr
let decl = Decl
let mod_name s = Module s

(* Identifiers *)
let mk ns name = { ns; name; }

let full_name =function
  | { name; ns = Module m; } ->
    Printf.sprintf "%s.%s" m name
  | { name; _ } ->
    name

