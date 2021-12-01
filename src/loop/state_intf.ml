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

type mode = [
  | `Full
  | `Incremental
]

(** {1 Signatures} *)

module type Common = sig

  type t
  (** The type of state *)

  exception Error of t
  (** Convenient exception. *)

  val warn :
    ?loc:Dolmen.Std.Loc.full ->
    t -> 'a Report.Warning.t -> 'a -> t
  (** Emit a warning *)

  val error :
    ?loc:Dolmen.Std.Loc.full ->
    t -> 'a Report.Error.t -> 'a -> t
  (** Emit an error. *)

end

(** This modules defines the smallest signatures for a solver state that allow
    to instantiate the {Pipeline.Make} functor. *)
module type Pipeline = sig

  include Common
  (** Common interface for the state. *)

  val time_limit : t -> float
  (** The time limit for one original statement (in seconds). *)

  val size_limit : t -> float
  (** The size limit for one original statement (in octets). *)

end

(** This modules defines the smallest signatures for a solver state that allow
    to instantiate the {Parser.Pipe} functor. *)
module type Parser_pipe = sig

  include Common
  (** common interface *)

  type term
  (** The type of solver terms. *)

  val input_file_loc : t -> Dolmen.Std.Loc.file
  (** CUrrent input file location meta-data. *)

  val set_input_file_loc : t -> Dolmen.Std.Loc.file -> t
  (** Set the input file location meta-data. *)

  val prelude : t -> string
  (** Some prelude to print at the begining of lines when in interactive mode. *)

  val is_interactive : t -> bool
  (** Whether we are running in interactive mode. *)

  val set_mode : t -> mode -> t
  (* Set the input mode. *)

  val set_lang : t -> Logic.language -> t
  (** Set the input language. *)

  val input_mode : t -> mode option
  (** Return the current mode (if any). *)

  val input_lang : t -> Logic.language option
  (** Return the input language (if any). *)

  val input_dir : t -> string
  (** Return the directory of the input source (e.g. the directory of the
      input file, or the current directory if in interactive mode). *)

  val input_source : t -> source
  (** Return the input source. *)

end

(** This modules defines the smallest signatures for a solver state that allow
    to instantiate the {Typer.Make} functor. *)
module type Typer = sig

  include Common
  (** common interface *)

  type ty_state
  (** The type of state used by the typer. *)

  val input_file_loc : t -> Dolmen.Std.Loc.file
  (** CUrrent input file location meta-data. *)

  val input_lang : t -> Logic.language option
  (** The current input language. *)

  val typecheck : t -> bool
  (** Whether to type-check expressions. *)

  val ty_state : t -> ty_state
  (** Returns the typing state associated. *)

  val set_ty_state : t -> ty_state -> t
  (** Set the typing state. *)

end

(** This modules defines the smallest signatures for a solver state that allow
    to instantiate the {Typer.Pipe} functor. *)
module type Typer_pipe = sig

  include Common
  (** common interface *)

  val input_lang : t -> Logic.language option
  (** Return the input language (if any). *)

end


(** This modules defines the smallest signatures for a solver state that allow
    to instantiate the {Headers.Pipe} functor. *)
module type Header_pipe = sig

  include Common
  (** common interface *)

  type header_state
  (** The type of state used for the header check*)

  val input_file_loc : t -> Dolmen.Std.Loc.file
  (** CUrrent input file location meta-data. *)

  val input_lang : t -> Logic.language option
  (** Return the input language (if any). *)

  val header_state : t -> header_state
  (** Get the header-check state. *)

  val set_header_state : t -> header_state -> t
  (** Set the header-check state. *)

  val check_headers : t -> bool
  (** Whether to check the headers. *)

  val allowed_licenses : t -> string list
  (** Licenses allowed. An empty list means all licenses are allowed. *)

  val allowed_lang_version : t -> string option
  (** Language version number allowed. [None] means allowing everything. *)

end

(** This modules defines the smallest signatures for a solver state that allow
    to instantiate the {Printer.Pipe} functor. *)
module type Printer_pipe = sig

  include Common
  (** common interface *)

end


