
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

type value =
  | Integer
  | Rational
  | Real
  | Binary
  | Hexadecimal
  | Bitvector
  | String

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

(* Name&Printing *)

let split { name; _ } =
  Misc.split_on_char '\000' name

let to_string ({ name; _} as id) =
  match split id with
  | [s] -> s
  | l ->
    let b = Buffer.create (String.length name + List.length l + 3) in
    Buffer.add_string b "(_";
    List.iter (fun s -> Buffer.add_char b ' '; Buffer.add_string b s) l;
    Buffer.add_char b ')';
    Buffer.contents b

let pp b id =
  Printf.bprintf b "%s" (to_string id)

let print fmt id =
  Format.fprintf fmt "%s" (to_string id)

let full_name = function
  | { ns = Module m; _ } as id ->
    Printf.sprintf "%s.%s" m (to_string id)
  | id ->
    to_string id


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

(* Standard attributes *)
let ac_symbol = mk Attr "ac"
let case_split = mk Decl "case_split"
let theory_decl = mk Decl "theory"
let rwrt_rule = mk Decl "rewrite_rule"
let tptp_role = mk Decl "tptp_role"
let tptp_kind = mk Decl "tptp_kind"

