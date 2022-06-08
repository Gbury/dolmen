
(* This file is free software, part of Archsat. See file "LICENSE" for more details. *)

(* Arbitrary tags for expressions. *)


(* Functor instantiation *)
(* ************************************************************************ *)

module M = Hmap.Make(struct
    type t = int
    let compare (a: int) (b: int) = compare a b
  end)

type map = M.t

type 'a t = {
  id : int;
  inj : 'a Hmap.injection;
}

let equal k k' = k.id = k'.id

let mk_key id = { id; inj = Hmap.create_inj (); }

let max_id = ref 0

let create () =
  incr max_id;
  mk_key !max_id

let empty = M.empty

let get m k =
  M.get ~inj:k.inj k.id m

let get_list m k =
  match get m k with
  | None -> []
  | Some l -> l

let get_last m k =
  match get m k with
  | None -> None
  | Some [] -> None
  | Some (x :: _) -> Some x

let unset m k =
  M.remove k.id m

let set m k l =
  M.add ~inj:k.inj k.id l m

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

