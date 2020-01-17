(* This file is free software, part of Archsat. See file "LICENSE" for more details. *)

(** Solver State

    This module defines various interfaces for states used in main loops of solvers
    (or other binaries) using dolmen. *)

module type Pipeline = sig
  (** This modules defines the smallest signatures for a solver state that allow
      to isntantiate the {Pipeline.Make} functor. *)

  type t
  (** The type of values recording options for the current run. *)

  val time_limit : t -> float
  (** The time limit for one original statement (in seconds). *)

  val size_limit : t -> float
  (** The size limit for one original statement (in octets). *)

  val error : t -> ('a, Format.formatter, unit, t) format4 -> 'a
  (** A function to log error messages. *)

end

type phase =[
  | `Parsing
  | `Include
  | `Typing
  | `Solving
]

module type S = sig
  (** This modules defines the smallest signatures for a solver state that allow
      to isntantiate the {Pipes.Make} functor. *)

  type t
  (** The type of state *)

  type term
  (** The type of solver terms. *)

  (* Hooks at the start/end of phases *)
  val start : phase -> unit
  val stop : phase -> unit

  (* Interactivity-related queries *)
  val prelude : t -> string
  val is_interactive : t -> bool

  (* Input options *)
  val input_dir : t -> string
  val set_lang : t -> Parse.language -> t
  val input_lang : t -> Parse.language option
  val input_source : t -> [
      | `Stdin
      | `File of string
    ]
  val file_not_found :
    ?loc:Dolmen.ParseLocation.t -> dir:string -> file:string -> 'a

  (* Executing statements *)
  val pop : t -> int -> t
  val push : t -> int -> t
  val reset_assertions : t -> t
  val plain : t -> Dolmen.Term.t -> t
  val get_proof : t -> t
  val get_unsat_core : t -> t
  val get_unsat_assumptions : t -> t
  val get_model : t -> t
  val get_values : t -> term list -> t
  val get_assignment : t -> t
  val get_assertions : t -> t
  val get_info : t -> string -> t
  val get_option : t -> string -> t
  val set_logic : t -> string -> t
  val set_info : t -> Dolmen.Term.t -> t
  val set_option : t -> Dolmen.Term.t -> t
  val echo : t -> string -> t
  val reset : t -> t
  val exit : t -> t

end
