
(* This file is free software, part of Dolmen. See file "LICENSE" for more details. *)

(** {2 Global display} *)

val init_display : unit -> unit

val finalise_display : unit -> unit


(** {2 Printing} *)

val printf : ('a, Format.formatter, unit) format -> 'a

val eprintf : ('a, Format.formatter, unit) format -> 'a

val fprintf :
  Format.formatter -> ('a, Format.formatter, unit) format -> 'a

val kfprintf :
  (Format.formatter -> 'a) -> Format.formatter ->
  ('b, Format.formatter, unit, 'a) format4 -> 'b


(** {2 Progress Bar} *)

module Bar : sig

  type t

  type config = {
    mem_bar : bool;
  }

  val create :
    ?above:int -> config:config -> name:string ->
    max_mem:int -> total_bytes:int -> unit -> t

  val add_to_process :
    t -> loc:Dolmen.Std.Loc.t -> unit

  val add_processed :
    t -> span:Mtime.Span.t ->
    processed:[< `Last of Dolmen.Std.Loc.t | `Sum of Dolmen.Std.Loc.t ] ->
    mem:[< `None | `Add of _ | `Set of _ ] ->
    unit

end
