(* This file is free software, part of Archsat. See file "LICENSE" for more details. *)

(** Callback

    This modules defines the smallest signatures for callbacks that allow
    to isntantiates the {Pipes.Make} functor. *)

type phase =[
  | `Parsing
  | `Include
]

module type S = sig

  val start : phase -> unit
  val stop : phase -> unit

end
