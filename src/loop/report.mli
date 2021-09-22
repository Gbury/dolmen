
(* This file is free software, part of Dolmen. See file "LICENSE" for more details. *)

(** {2 Errors} *)

module Error : sig

  type 'a t
  (** The type of errors, parameterized by their payload/parameters. *)

  val print : Format.formatter -> ('a t * 'a) -> unit
  (** Print an error. *)

  val print_hints : Format.formatter -> ('a t * 'a) -> unit
  (** Print an error's hints. *)

  val code : _ t -> Code.t
  (** Return the return code of an error. *)

  val user_interrupt : unit t
  (** Error for a user interrupt. *)

  val timeout : unit t
  (** Error for timeouts. *)

  val spaceout : unit t
  (** Error for spaceouts. *)

  val internal_error : (Format.formatter -> unit) t
  (** Internal error, the param is a delayed printer
      (typically created using `Format.dprintf`). *)

  val uncaught_exn : (exn * Printexc.raw_backtrace) t
  (** Error for an uncaught exn (together with a backtrace). *)

  val mk :
    ?code:Code.t ->
    mnemonic:string ->
    message:(Format.formatter -> 'a -> unit) ->
    ?hints:('a -> (Format.formatter -> unit) option) list ->
    name:string -> ?doc:(Format.formatter -> unit) ->
    unit -> 'a t
  (** Create a new error. *)

end

(** {2 Warnings} *)

module Warning : sig

  type 'a t
  (** The type of warnings, parameterized by their payload/parameters. *)

  val print : Format.formatter -> ('a t * 'a) -> unit
  (** Print a warning. *)

  val print_hints : Format.formatter -> ('a t * 'a) -> unit
  (** Print an warning's hints. *)

  val code : _ t -> Code.t
  (** Return the return code of an error. *)

  val mk :
    ?code:Code.t ->
    mnemonic:string ->
    message:(Format.formatter -> 'a -> unit) ->
    ?hints:('a -> (Format.formatter -> unit) option) list ->
    name:string -> ?doc:(Format.formatter -> unit) ->
    unit -> 'a t
    (** Create a new warning. *)

end

(** {2 Report configuration} *)

module Conf : sig

  type t
  (** The type of configuration for reports. *)

  type status =
    | Disabled
    | Enabled
    | Fatal (**)
  (** The status of a report. *)

  val status : t -> _ Warning.t -> status
  (** Status for an individual warning. *)

  val mk : default:status -> t
  (** Create a configuration with a default status for warnings. *)

  val disable : t -> _ Warning.t -> t
  (** Disable the warning. *)

  val disable_mnemonic : t -> string ->
    (t, [ `Error_mnemonic | `Unknown_mnemonic ]) result
  (** Disable the warning identified by the given mnemonic. *)

  val enable : t -> _ Warning.t -> t
  (** Enable the warning. *)

  val enable_mnemonic : t -> string ->
    (t, [ `Error_mnemonic | `Unknown_mnemonic ]) result
  (** Enable the warning identified by the given mnemonic. *)

  val fatal : t -> _ Warning.t -> t
  (** Make fatal the warning. *)

  val fatal_mnemonic : t -> string ->
    (t, [ `Error_mnemonic | `Unknown_mnemonic ]) result
  (** Make fatal the warning identified by the given mnemonic. *)

  val set_enabled : t -> _ Warning.t -> t
  (** Force the warning to be exactly enabled (and not fatal). *)

  val set_enabled_mnemonic : t -> string ->
    (t, [ `Error_mnemonic | `Unknown_mnemonic ]) result
  (** Force the warning identified by the mnemonic to be
      exactly enabled (and not fatal). *)

end
