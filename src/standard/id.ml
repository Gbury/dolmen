
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

type value =
  | Integer
  | Rational
  | Real
  | Binary
  | Hexadecimal
  | Bitvector

type namespace =
  | Var
  | Sort
  | Term
  | Attr
  | Decl
  | Track
  | Module of string
  | Value of value

type t = {
  ns : namespace;
  name : string;
}

let hash = Hashtbl.hash
let compare = Stdlib.compare
let equal = Stdlib.(=)

let pp b { name ; _ } =
  Printf.bprintf b "%s" name

let print fmt { name ; _ } =
  let name = String.map (function
      | '\000' -> ':'
      | c -> c
    ) name
  in
  Format.fprintf fmt "%s" name

(* Tracked hashtbl *)
let trackers = Hashtbl.create 13
let trackeds = Hashtbl.create 13

(* Namespaces *)
let var = Var
let sort = Sort
let term = Term
let attr = Attr
let decl = Decl
let track = Track
let mod_name s = Module s

(* Identifiers *)
let mk ns name = { ns; name; }

let tracked ~track ns name =
  let id = mk ns name in
  Hashtbl.add trackers track id;
  Hashtbl.add trackeds id track;
  id

let full_name =function
  | { name; ns = Module m; } ->
    Printf.sprintf "%s.%s" m name
  | { name; _ } ->
    name

(* Standard attributes *)
let ac_symbol = mk Attr "ac"
let case_split = mk Decl "case_split"
let theory_decl = mk Decl "theory"
let rwrt_rule = mk Decl "rewrite_rule"
let tptp_role = mk Decl "tptp_role"

