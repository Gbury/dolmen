
(* This file is free software, part of Archsat. See file "LICENSE" for more details. *)

(* Arbitrary tags for expressions.
   Uses a mixed map (see containers.data.CCMixmap) *)


(* Mixmap Implementation (from containers) *)
(* ************************************************************************ *)

(* Implementation taken from containers. *)

type 'b injection = {
  get : (unit -> unit) -> 'b option;
  set : 'b -> (unit -> unit);
}

let create_inj () =
  let r = ref None in
  let get f =
    r := None;
    f ();
    !r
  and set v =
    (fun () -> r := Some v)
  in
  {get;set}

module type S = sig
  type key

  type t
  (** A map containing values of different types, indexed by {!key}. *)

  val empty : t
  (** Empty map *)

  val get : inj:'a injection -> key -> t -> 'a option
  (** Get the value corresponding to this key, if it exists and
      belongs to the same key *)

  val add : inj:'a injection -> key -> 'a -> t -> t
  (** Bind the key to the value, using [inj] *)
end

module type ORD = sig
  type t
  val compare : t -> t -> int
end

module Make(X : ORD) : S with type key = X.t = struct
  module M = Map.Make(X)

  type key = X.t
  type t = (unit -> unit) M.t

  let empty = M.empty

  let get ~inj x map =
    try inj.get (M.find x map)
    with Not_found -> None

  let add ~inj x y map =
    M.add x (inj.set y) map
end


(* Functor instantiation *)
(* ************************************************************************ *)

module M = Make(struct
    type t = int
    let compare (a: int) (b: int) = compare a b
  end)

type map = M.t

type 'a t = {
  id : int;
  inj : 'a list injection;
}

let equal k k' = k.id = k'.id

let mk_key id = { id; inj = create_inj (); }

let max_id = ref 0

let create () =
  incr max_id;
  mk_key !max_id

let empty = M.empty

let get m k =
  match M.get ~inj:k.inj k.id m with
  | None -> []
  | Some l -> l

let last m k =
  match get m k with
  | x :: _ -> Some x
  | [] -> None

let replace m k l =
  M.add ~inj:k.inj k.id l m

let add m k v =
  replace m k (v :: get m k)

