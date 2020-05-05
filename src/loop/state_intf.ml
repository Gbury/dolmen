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

type phase = [
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

  val warn : t -> Dolmen.ParseLocation.t -> string -> t
  (** Emit a warning *)

  val start : phase -> unit
  (** Hook at the start of a phase *)

  val stop : phase -> unit
  (** Hook at the end of a phase *)

  val prelude : t -> string
  (** Some prelude to print at the begining of lines when in interactive mode. *)

  val is_interactive : t -> bool
  (** Whether we are running in interactive mode. *)

  val set_mode : t -> mode -> t
  (* Set the input mode. *)

  val set_lang : t -> Parser.language -> t
  (** Set the input language. *)

  val input_mode : t -> mode option
  (** Return the current mode (if any). *)

  val input_lang : t -> Parser.language option
  (** Return the input language (if any). *)

  val input_dir : t -> string
  (** Return the directory of the input source (e.g. the directory of the
      input file, or the current directory if in interactive mode). *)

  val input_source : t -> source
  (** Return the input source. *)

  val file_not_found :
    ?loc:Dolmen.ParseLocation.t -> dir:string -> file:string -> 'a
  (** Callback for when a file specified by the input source is not found. *)

end
