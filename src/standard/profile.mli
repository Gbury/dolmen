
(** {1 Profiling probes} *)

type probe

val null_probe : probe

val enabled : unit -> bool

val instant : string -> unit

val begin_ : string -> probe

val exit : probe -> unit

val with_ : string -> (unit -> 'a) -> 'a
val with1 : string -> ('a -> 'b) -> 'a -> 'b
val with2 : string -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c

module type BACKEND = sig
  val get_ts : unit -> float

  val emit_duration_event :
    name : string ->
    start : float ->
    end_ : float ->
    unit ->
    unit

  val emit_instant_event :
    name : string ->
    ts : float ->
    unit ->
    unit

  val teardown : unit -> unit
end

type backend = (module BACKEND)

module Control : sig
  val setup : backend option -> unit

  val teardown : unit -> unit
end
