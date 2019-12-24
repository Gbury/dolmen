(* This file is free software, part of Archsat. See file "LICENSE" for more details. *)

(** Proofs

    This modules defines the smallest signatures for proofs that allow
    to isntantiates the {Pipes.Make} functor. *)

module type S = sig

  type id

  type term

  type t

end
