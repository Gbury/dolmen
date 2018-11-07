
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

let pp b { name ; _ } =
  Printf.bprintf b "%s" name

let print fmt { name ; _ } =
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

(* Standard attributes *)
let rwrt_rule = mk Decl "rewrite_rule"
let tptp_role = mk Decl "tptp_role"

