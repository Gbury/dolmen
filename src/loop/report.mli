
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

  val enabled : _ t -> bool
  (** Is the warning enabled ? *)

  val fatal : _ t -> bool
  (** Is the warning fatal ? *)

  val mk :
    ?code:Code.t ->
    ?enabled:bool -> ?fatal:bool ->
    mnemonic:string ->
    message:(Format.formatter -> 'a -> unit) ->
    ?hints:('a -> (Format.formatter -> unit) option) list ->
    name:string -> ?doc:(Format.formatter -> unit) ->
    unit -> 'a t
    (** Create a new warning. *)

end

