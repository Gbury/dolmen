
(* This file is free software, part of Dolmen. See file "LICENSE" for more details. *)

(** {2 Some types} *)

type ('kind, 'param) aux
type 'a error = ([`Error], 'a) aux
type 'a warning = ([`Warning], 'a) aux

type any_error = Any_err : _ error -> any_error
type any_warning = Any_warn : _ warning -> any_warning

val add_hint : _ aux -> (Format.formatter -> unit) -> unit
(** Dynamically add a hint to a warning/error. *)

(** {2 Reports} *)

module T : sig

  type all = [ `All ]
  type err = [ `Error of any_error ]
  type warn = [ `Warning of any_warning ]

  type t = [ all | err | warn ]

  val list : unit -> t list
  (** List all reports. *)

  val find_mnemonic : string -> t option
  (** Find the warning/error/group associated to a mnemonic. *)

  val name : [< t ] -> string
  (** Name of a report. *)

  val mnemonic : [< t ] -> string
  (** mnemonic of a report. *)

  val kind : [< t ] -> string
  (** kind of a report. *)

  val category : [< t ] -> string
  (** category of a report. *)

  val doc : [< t ] -> (Format.formatter -> unit)
  (** documentation for a report. *)
end


(** {2 Errors} *)

module Error : sig

  type 'a t = 'a error
  (** The type of errors, parameterized by their payload/parameters. *)

  val code : _ t -> Code.t
  (** Return the return code of an error. *)

  val name : _ t -> string
  (** Return the name/short description of an error. *)

  val mnemonic : _ t -> string
  (** Return the mnemonic of an error. *)

  val print : Format.formatter -> ('a t * 'a) -> unit
  (** Print an error. *)

  val print_hints : Format.formatter -> ('a t * 'a) -> unit
  (** Print an error's hints. *)

  val print_doc : Format.formatter -> _ t -> unit
  (** Print the (long) documentation for an error. *)

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

  type 'a t = 'a warning
  (** The type of warnings, parameterized by their payload/parameters. *)

  val code : _ t -> Code.t
  (** Return the return code of an error. *)

  val name : _ t -> string
  (** Return the name (short description) of a warning. *)

  val mnemonic : _ t -> string
  (** Return the mnemonic of a warning. *)

  val print : Format.formatter -> ('a t * 'a) -> unit
  (** Print a warning. *)

  val print_hints : Format.formatter -> ('a t * 'a) -> unit
  (** Print an warning's hints. *)

  val print_doc : Format.formatter -> _ t -> unit
  (** Print the (long) documentation of a warning. *)

  val mk :
    ?code:Code.t ->
    mnemonic:string ->
    message:(Format.formatter -> 'a -> unit) ->
    ?hints:('a -> (Format.formatter -> unit) option) list ->
    name:string -> ?doc:(Format.formatter -> unit) ->
    unit -> 'a t
    (** Create a new warning. *)

  module Status : sig

    type t =
      | Disabled
      | Enabled
      | Fatal (**)
    (** The status of a report. *)

    val print : Format.formatter -> t -> unit
    (** Print a status. *)

    val to_string : t -> string
    (** Print into a string. *)

  end

end

(** {2 Report configuration} *)

module Conf : sig

  type t
  (** The type of configuration for reports. *)

  val mk : default:Warning.Status.t -> t
  (** Create a configuration with a default status for warnings. *)

  val status : t -> _ Warning.t -> Warning.Status.t
  (** Status for an individual warning. *)

  val disable : t -> [ `All | `Warning of any_warning ] -> t
  (** Disable the warning. *)

  val enable : t -> [ `All | `Warning of any_warning ] -> t
  (** Enable the warning. *)

  val fatal : t -> [ `All | `Warning of any_warning ] -> t
  (** Make fatal the warning. *)

  val set_enabled : t -> [ `All | `Warning of any_warning ] -> t
  (** Force the warning to be exactly enabled (and not fatal). *)

end
