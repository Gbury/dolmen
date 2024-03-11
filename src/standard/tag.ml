
(* This file is free software, part of Archsat. See file "LICENSE" for more details. *)

(* Arbitrary tags for expressions. *)


(* Functor instantiation *)
(* ************************************************************************ *)

(* Key info *)
type 'a info = {
  print : 'a Pretty.print;
}

module M = Hmap.Make(struct type 'a t = 'a info end)

(* Types and key creation *)
(* ************************************************************************ *)

type map = M.t
type 'a t = 'a M.key

let info k =
  M.Key.info k

let create ?(print=Pretty.Ignore) () =
  let info = { print; } in
  M.Key.create info


(* Iteration *)
(* ************************************************************************ *)

type binding = M.binding = B : 'a t * 'a -> binding

let iter m f = M.iter f m

let fold m acc f = M.fold f m acc


(* small wrappers *)
(* ************************************************************************ *)

let empty = M.empty

let is_empty = M.is_empty

let get m k =
  M.find k m

let unset m k =
  M.rem k m

let set m k l =
  M.add k l m


(* convenient wrappers for advanced tags *)
(* ************************************************************************ *)

let get_list m k =
  match get m k with
  | None -> []
  | Some l -> l

let get_last m k =
  match get m k with
  | None -> None
  | Some [] -> None
  | Some (x :: _) -> Some x

let set_opt m k = function
  | None -> m
  | Some v -> set m k v

let add m k v =
  set m k (v :: get_list m k)

let add_opt m k = function
  | None -> m
  | Some v -> add m k v

let add_list m k = function
  | [] -> m
  | l -> set m k (List.rev_append l (get_list m k))

