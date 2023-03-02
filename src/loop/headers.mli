
(* This file is free software, part of Dolmen. See file "LICENSE" for more details. *)

(** {2 Header fields} *)

module Field : sig

  type t =
    | Lang_version
    | Problem_version
    | Problem_source
    | Problem_license
    | Problem_category
    | Problem_status (**)
  (** Header fields. *)

  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
  (** Usual functions *)

  val name : Logic.language option -> t -> string
  (** Name of a header field, parameterized by language *)

  val print : ?lang:Logic.language -> Format.formatter -> t -> unit
  (** Print a header field (with the same string as {!name}). *)

end

(** {2 Header set} *)

type t
(** Header set, i.e. a map of fields to values for the header. *)

val empty : t
(** The empty header set *)

val set : t -> Field.t -> string -> t
(** Add/set a header to the corresponding value. *)

val get : t -> Field.t -> string option
(** Get a header value, if present in the set. *)

val mem : t -> Field.t -> bool
(** Test the presence of a header field. *)

val remove : t -> Field.t -> t
(** Remove a field from a set. *)

(** {2 Pipe functor} *)

val code : Code.t
(** Code for header errors. *)

module type S = Headers_intf.S

module Make(S : State.S) : S with type state := S.t
                              and type 'a key := 'a S.key
                              and type header_state := t


