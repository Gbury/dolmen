(* This file is free software, part of Archsat. See file "LICENSE" for more details. *)

(* Wrapper around polymorphic identifiers *)
(* ************************************************************************ *)

module type Arg = Dolmen_intf.Id.Escape

(* Escaping helper *)
(* ************************************************************************ *)

let smap f s =
  let b = Buffer.create (String.length s) in
  let rec aux i =
    if i >= String.length s then
      Buffer.contents b
    else begin
      Buffer.add_string b (f (i + 1) s.[i]);
      aux (i + 1)
    end
  in
  aux 0

(* Renaming function *)
(* ************************************************************************ *)

let get_num ~sep s =
  let rec aux acc mult i =
    if i < 0 then s, 0
    else match s.[i] with
      | ('0' .. '9') as c ->
        let j = int_of_char c - 48 in
        aux (acc + j * mult) (mult * 10) (i - 1)
      | c when c = sep ->
        if i = 0 then s, 0 (* no base name found *)
        else String.sub s 0 i, acc
      | _ -> s, 0
  in
  aux 0 1 (String.length s - 1)

let rename ~sep s =
  let base, n = get_num ~sep s in
  Format.sprintf "%s%c%d" base sep (n + 1)


(* Printing wrappers for escapped sequences *)
(* ************************************************************************ *)

type status =
  | Same    (* No changes to the identifiers name *)
  | Escaped (* Identifiers has been escaped *)
  | Renamed (* Identifier has been renamed due to a conflict
               with another escaped or renamed identifier *)

module Make(Id : Arg) = struct

  module H = Hashtbl.Make(Id)

  type t = {
    lang   : string;
    name   : Id.t -> string;     (* function for identifier name *)
    escape : string -> string;  (* escape function, ideally idempotent *)
    rename : string -> string;  (* renaming function, should have no fixpoint *)
    mutable table : (status * string) H.t;
    mutable names : (string, Id.t) Hashtbl.t;
  }

  let mk ~lang ~name ~escape ~rename = {
    lang; name;
    escape; rename;
    table = H.create 1013;
    names = Hashtbl.create 1013;
  }

  let flush t =
    Hashtbl.reset t.names;
    H.reset t.table;
    ()

  (* Adding escapped sequences *)
  (* ************************************************************************ *)

  let rec add t id status name =
    match Hashtbl.find t.names name with
    | exception Not_found ->
      add_success t id status name
    | r ->
      assert (not (Id.equal id r));
      add_failure t id name

  and add_success t any status name =
    let () = H.add t.table any (status, name) in
    let () = Hashtbl.add t.names name any in
    name

  and add_failure t id name =
    let new_name = t.rename name in
    assert (new_name <> name);
    add t id Renamed new_name

  let escape t id =
    match H.find t.table id with
    | (_, s) -> s
    | exception Not_found ->
      let name = t.name id in
      let escaped = t.escape (t.name id) in
      let status, new_name =
        if (escaped = name)
        then Same, name (* allow the gc to reclaim 'escaped' *)
        else Escaped, escaped
      in
      add t id status new_name

  (* Convenience functions *)
  (* ************************************************************************ *)

  let print t fmt id =
    Format.fprintf fmt "%s" (escape t id)

end

