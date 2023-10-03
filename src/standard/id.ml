
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(* Types *)
(* ************************************************************************* *)

type namespace = Namespace.t

type t = {
  name : Name.t;
  ns : namespace;
}


(* Std functions *)
(* ************************************************************************* *)

let hash { ns; name; } =
  Misc.hash2 (Namespace.hash ns) (Name.hash name)

let compare { ns; name; } { ns = ns'; name = name'; } =
  let (<?>) = Misc.(<?>) in
  Namespace.compare ns ns'
  <?> (Name.compare, name, name')

let equal id id' =
  id == id' || compare id id' = 0

let print fmt { name; ns = _; } =
  Name.print fmt name


(* Namespaces *)
(* ************************************************************************* *)

let var = Namespace.var
let sort = Namespace.sort
let term = Namespace.term
let attr = Namespace.attr
let decl = Namespace.decl
let track = Namespace.track


(* Inspection & Creation *)
(* ************************************************************************* *)

let ns { ns; _ } = ns
let name { name; _ } = name

let create ns name = { ns; name; }

let mk ns s =
  let name = Name.simple s in
  create ns name

let indexed ns basename indexes =
  let name = Name.indexed basename indexes in
  create ns name

let qualified ns path name =
  let name = Name.qualified path name in
  create ns name


(* Tracked hashtbl *)
(* ************************************************************************* *)

let trackers = Hashtbl.create 13
let trackeds = Hashtbl.create 13


let tracked ~track ns path =
  let id = mk ns path in
  Hashtbl.add trackers track id;
  Hashtbl.add trackeds id track;
  id


(* Standard attributes *)
(* ************************************************************************* *)

let stmt = mk Attr "stmt"
let ac_symbol = mk Attr "ac"
let predicate_def = mk Attr "predicate"

let case_split = mk Decl "case_split"
let theory_decl = mk Decl "theory"
let rwrt_rule = mk Decl "rewrite_rule"
let tptp_role = mk Decl "tptp_role"
let tptp_kind = mk Decl "tptp_kind"


(* Maps *)
(* ************************************************************************* *)

module Map = struct

  type 'a t = 'a Name.Map.t Namespace.Map.t

  let empty = Namespace.Map.empty

  let find_exn k t =
    Name.Map.find_exn k.name (Namespace.Map.find_exn k.ns t)

  let find_opt k t =
    match Namespace.Map.find_opt k.ns t with
    | None -> None
    | Some map -> Name.Map.find_opt k.name map

  let add k v t =
    Namespace.Map.find_add k.ns (function
        | None -> Name.Map.add k.name v Name.Map.empty
        | Some map -> Name.Map.add k.name v map
      ) t

  let find_add k f t =
    Namespace.Map.find_add k.ns (function
        | None -> Name.Map.find_add k.name f Name.Map.empty
        | Some map -> Name.Map.find_add k.name f map
      ) t

  let iter f t =
    Namespace.Map.iter (fun ns map ->
        Name.Map.iter (fun name v ->
            f { ns; name; } v
          ) map
      ) t

  let fold f t acc =
    Namespace.Map.fold (fun ns map acc ->
        Name.Map.fold (fun name v acc ->
            f { ns; name; } v acc
          ) map acc
      ) t acc

end

