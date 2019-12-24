(* This file is free software, part of Archsat. See file "LICENSE" for more details. *)

(** Expressions

    This modules defines the smallest signatures for a solver that allow
    to isntantiates the {Pipes.Make} functor. *)

module type S = sig

  type id

end
