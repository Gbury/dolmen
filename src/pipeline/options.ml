(* This file is free software, part of Archsat. See file "LICENSE" for more details. *)

(** Options

    This modules defines the smallest signatures for options that allow
    to isntantiates the {Pipes.Make} functor. *)

module type S = sig

  type t

  val prelude : t -> string
  val is_interactive : t -> bool

  val input : t -> [
      | `Stdin
      | `File of string
    ]

  val lang : t -> Parse.language option
  val set_lang : t -> Parse.language -> t

end
