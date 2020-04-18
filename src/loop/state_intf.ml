(* This file is free software, part of Archsat. See file "LICENSE" for more details. *)

(** Solver State

    This module defines various interfaces for states used in main loops of solvers
    (or other binaries) using dolmen. *)

(** {1 Useful type aliases} *)

type source = [
  | `Stdin
  | `File of string
  | `Raw of string * string
]

type phase =[
  | `Parsing
  | `Include
  | `Typing
  | `Solving
]

type mode = [
  | `Full
  | `Incremental
]

(** {1 Signatures} *)

(** This modules defines the smallest signatures for a solver state that allow
    to instantiate the {Typer.Make} functor. *)
module type Typer = sig

  type solve_st
  (** The type used to store results of solving. *)

  val missing_smtlib_logic : unit -> 'a
  (** Called when an smtlib set-logic command is missing. *)

end

(** This modules defines the smallest signatures for a solver state that allow
    to instantiate the {Pipeline.Make} functor. *)
module type Pipeline = sig

  type t
  (** The type of values recording options for the current run. *)

  val time_limit : t -> float
  (** The time limit for one original statement (in seconds). *)

  val size_limit : t -> float
  (** The size limit for one original statement (in octets). *)

end

(** This modules defines the smallest signatures for a solver state that allow
    to instantiate the {Pipes.Make} functor. *)
module type S = sig

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
  val set_mode : t -> mode -> t
  val set_lang : t -> Parser.language -> t

  val input_mode : t -> mode option
  val input_lang : t -> Parser.language option
  val input_dir : t -> string
  val input_source : t -> source
  val file_not_found :
    ?loc:Dolmen.ParseLocation.t -> dir:string -> file:string -> 'a
  val warn : t -> Dolmen.ParseLocation.t -> string -> t

  val set_logic : t -> string -> t

end
