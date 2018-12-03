
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(** Escaping identifiers

    This module provides facilities for escaping identifiers, i.e. make them conform
    to some output syntax, which typically have restrictions on what are correct
    identifiers.
*)

open Dolmen_std

module H = Hashtbl.Make(Id)

(* Printing wrappers for escapped sequences *)

type status =
  | Same    (* No changes to the identifiers name *)
  | Escaped (* Identifiers has been escaped *)
  | Renamed (* Identifier has been renamed due to a conflict
               with another escaped or renamed identifier *)

type name =
  | Exact of string   (** The given name is to be printed exactly as is *)
  | Normal of string  (** The given name should be escaped/renamed if necessary *)

type t = {
  lang   : string;
  name   : Id.t -> name;     (* function for identifier name *)
  escape : string -> string;  (* escape function, ideally idempotent *)
  rename : string -> string;  (* renaming function, should have no fixpoint *)
  (* Each escaping mechanism may need to distinguish different formatters when escaping symbols. *)
  mutable tables  : (Format.formatter * (status * string) H.t) list;
  mutable names  : (Format.formatter * (string, Id.t) Hashtbl.t) list;
}

let mk ~lang ~name ~escape ~rename = {
  lang; name;
  escape; rename;
  tables = [];
  names = [];
}


let pp_assign fmt (id, status, name) =
  Format.fprintf fmt "@[<hov>%a@ %s@ %s@]"
    Id.print id
    (match status with
     | Same -> "->"
     | Escaped -> "~>"
     | Renamed -> "~~>")
    name

let get_table t fmt =
  try List.assq fmt t.tables
  with Not_found ->
    let h = H.create 4013 in
    t.tables <- (fmt, h) :: t.tables;
    h

let get_names t fmt =
  try List.assq fmt t.names
  with Not_found ->
    let h = Hashtbl.create 4013 in
    t.names <- (fmt, h) :: t.names;
    h

(* Adding escapped sequences *)

let rec add ?(fragile=false) fmt t any status name =
  match Hashtbl.find (get_names t fmt) name with
  | exception Not_found ->
    add_success fmt t any status name
  | r ->
    assert (not (Id.equal any r));
    if status = Same then begin
      match H.find (get_table t fmt) r with
      (** Two ids have the same name, we trust the developpers
          that this is intended *)
      | (Same, s) ->
        assert (s = name);
        add_success fmt t any status name
      (** The escaped id collided with another escaped/renamed id,
          this is a potentially dangerous situation. *)
      | _ ->
        add_failure ~fragile fmt t any status name r
    end else
      add_failure ~fragile fmt t any status name r

and add_success fmt t any status name =
  (* Util.debug ~section "Adding %a" pp_assign (any, status, name); *)
  let () = H.add (get_table t fmt) any (status, name) in
  let () = Hashtbl.add (get_names t fmt) name any in
  name

and add_failure ~fragile fmt t id status name r =
  let conflict_st, conflict_name = H.find (get_table t fmt) r in
  if fragile then
    failwith (Format.asprintf "Escaping %a,@ conficted with@ %a"
                pp_assign (id, status, name) pp_assign (r, conflict_st, conflict_name));
  let new_name = t.rename name in
  assert (new_name <> name);
  add fmt t id Renamed new_name

let escape fmt t any =
  match H.find (get_table t fmt) any with
  | (_, s) -> s
  | exception Not_found ->
    let fragile, status, new_name =
      match t.name any with
      | Exact name ->
        true, Same, name
      | Normal name ->
        let s = t.escape name in
        let status = if (s = name) then Same else Escaped in
        false, status, s
    in
    add ~fragile fmt t any status new_name

(* Convenience functions *)

let id t fmt x =
  Format.fprintf fmt "%s" (escape fmt t x)

(* Unicode wrappers *)

let encode e c =
  match Uutf.encode e c with
  | `Ok -> ()
  | `Partial ->
    (* should only happen with manual sources according to the doc,
       so it is safe to assume it doesn't happen *)
    assert false

let encode_char e c = encode e (`Uchar c)

let umap f s =
  let encoding = `UTF_8 in
  let b = Buffer.create (String.length s) in
  let d = Uutf.decoder ~encoding (`String s) in
  let e = Uutf.encoder encoding (`Buffer b) in
  let rec aux () =
    match Uutf.decode d with
    | `End ->
      let () = encode e `End in
      Buffer.contents b
    | `Await ->
      let () = encode e `Await in
      aux ()
    | `Uchar c ->
      let pos = Uutf.decoder_count d in
      let () = List.iter (encode_char e) (f pos (Some c)) in
      aux ()
    | `Malformed _ ->
      let pos = Uutf.decoder_count d in
      let () = List.iter (encode_char e) (f pos None) in
      aux ()
  in
  aux ()

(* Print an Uchar *)

let pp_uchar fmt c =
  let b = Buffer.create 5 in
  let () = Uutf.Buffer.add_utf_8 b c in
  let s = Buffer.contents b in
  Format.fprintf fmt "%s" s

(* Renaming function *)

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


