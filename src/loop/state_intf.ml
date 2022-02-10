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

type 'lang file = {
  lang    : 'lang option;
  mode    : mode option;
  loc     : Dolmen.Std.Loc.file;
  dir     : string;
  source  : source;
}

(** {1 Signatures} *)

module type Common = sig

  type t
  (** The type of state *)

  exception Error of t
  (** Convenient exception. *)

  val warn :
    ?file:_ file ->
    ?loc:Dolmen.Std.Loc.full ->
    t -> 'a Report.Warning.t -> 'a -> t
  (** Emit a warning *)

  val error :
    ?file:_ file ->
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

  val prelude : t -> string
  (** Some prelude to print at the begining of lines when in interactive mode. *)

  val is_interactive : t -> bool
  (** Whether we are running in interactive mode. *)

  val logic_file : t -> Logic.language file
  (** Get the logic file info. *)

  val set_logic_file : t -> Logic.language file -> t
  (** Set the logic file info. *)

  val response_file : t -> Response.language file
  (** Get the logic file info. *)

  val set_response_file : t -> Response.language file -> t
  (** Set the logic file info. *)

end

(** This modules defines the smallest signatures for a solver state that allow
    to instantiate the {Typer.Make} functor. *)
module type Typer = sig

  include Common
  (** common interface *)

  type ty_state
  (** The type of state used by the typer. *)

  val logic_file : t -> Logic.language file
  (** Get the logic file info. *)

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

  val logic_file : t -> Logic.language file
  (** Get the logic file info. *)

end


(** This modules defines the smallest signatures for a solver state that allow
    to instantiate the {Headers.Pipe} functor. *)
module type Header_pipe = sig

  include Common
  (** common interface *)

  type header_state
  (** The type of state used for the header check*)

  val logic_file : t -> Logic.language file
  (** Get the logic file info. *)

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
    to instantiate the {Check.Pipe} functor. *)
module type Check_pipe = sig

  include Common
  (** common interface *)

  type 'st check_state
  (** The type of state used for the header check*)


  (* Response file *)

  val response_file : t -> Response.language file
  (** Get the logic file info. *)


  (* Checking options *)

  val check_state : t -> t check_state
  (** Get the checker state. *)

  val set_check_state : t -> t check_state -> t
  (** Set the checker state. *)

  val check_model : t -> bool
  (** Whether to check models. *)

  val check_model_eager : t -> bool
  (** Flag for eager model checking. *)

end
