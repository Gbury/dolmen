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
    to instantiate the {Parser.Pipe} functor. *)
module type Parser_pipe = sig

  type t
  (** The type of state *)

  type term
  (** The type of solver terms. *)

  val warn :
    ?loc:Dolmen.Std.Loc.full ->
    t -> ('a, Format.formatter, unit, t) format4 ->
    'a
  (** Emit a warning *)

  val input_file_loc : t -> Dolmen.Std.Loc.file
  (** CUrrent input file location meta-data. *)

  val set_input_file_loc : t -> Dolmen.Std.Loc.file -> t
  (** Set the input file location meta-data. *)

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

  val file_not_found :
    loc:Dolmen.Std.Loc.full -> dir:string -> file:string -> 'a
  (** Callback for when a file specified by the input source is not found. *)

end

(** This modules defines the smallest signatures for a solver state that allow
    to instantiate the {Typer.Make} functor. *)
module type Typer = sig

  type ty_state
  (** The type of state used by the typer. *)

  type t
  (** The type for the global state. *)

  val warn :
    ?loc:Dolmen.Std.Loc.full -> t ->
    ('a, Format.formatter, unit, t) format4 -> 'a
  (** Emit a warning *)

  val input_file_loc : t -> Dolmen.Std.Loc.file
  (** CUrrent input file location meta-data. *)

  val input_lang : t -> Logic.language option
  (** The current input language. *)

  val typecheck : t -> bool
  (** Whether to type-check expressions. *)

  val strict_typing : t -> bool
  (** Whether to be strict about typing warnings/errors *)

  val ty_state : t -> ty_state
  (** Returns the typing state associated. *)

  val set_ty_state : t -> ty_state -> t
  (** Set the typing state. *)

end

(** This modules defines the smallest signatures for a solver state that allow
    to instantiate the {Typer.Pipe} functor. *)
module type Typer_pipe = sig

  type t
  (** The type of state *)

  val input_lang : t -> Logic.language option
  (** Return the input language (if any). *)

end


(** This modules defines the smallest signatures for a solver state that allow
    to instantiate the {Headers.Pipe} functor. *)
module type Header_pipe = sig

  type t
  (** The type of state *)

  type header_state
  (** The type of state used for the header check*)

  val warn :
    ?loc:Dolmen.Std.Loc.full ->
    t -> ('a, Format.formatter, unit, t) format4 ->
    'a
  (** Emit an error. *)

  val error :
    ?code:Code.t ->
    ?loc:Dolmen.Std.Loc.full ->
    t -> ('a, Format.formatter, unit, t) format4 ->
    'a
  (** Emit an error. *)

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
