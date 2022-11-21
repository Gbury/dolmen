
(* This file is free software, part of Dolmen. See file "LICENSE" for more details. *)

(** {2 Global display} *)

val init_display : unit -> unit
(* Create and initialise the display *)

val finalise_display : unit -> unit
(* Finalise the display. Can be called multiple times. *)


(** {2 Printing} *)

val printf : ('a, Format.formatter, unit) format -> 'a
(* Analogue to {Format.printf} but properly wraps the display. *)

val eprintf : ('a, Format.formatter, unit) format -> 'a
(* Analogue to {Format.eprintf} but properly wraps the display. *)

val fprintf :
  Format.formatter -> ('a, Format.formatter, unit) format -> 'a
(* Analogue to {Format.fprintf} but properly wraps the display. *)

val kfprintf :
  (Format.formatter -> 'a) -> Format.formatter ->
  ('b, Format.formatter, unit, 'a) format4 -> 'b
(* Analogue to {Format.kfprintf} but properly wraps the display. *)


(** {2 Progress Bar} *)

module Bar : sig

  type t
  (** The type of a progress bar. This type includes some mutable data. *)

  type config = {
    mem_bar : bool;
  }
  (** Configuration for progress bars. *)

  val create :
    ?above:int -> config:config -> name:string ->
    max_mem:int -> total_bytes:int -> unit -> t
  (** Create a new progress bar. *)

  val add_to_process :
    t -> loc:Dolmen.Std.Loc.t -> unit
  (** Add some location as needing to be processed in the progress bar. *)

  val add_processed :
    t -> span:Mtime.Span.t ->
    processed:[< `Last of Dolmen.Std.Loc.t | `Sum of Dolmen.Std.Loc.t ] ->
    mem:[< `None | `Add of _ | `Set of _ ] ->
    unit
  (** Record the given loc as processed for the given bar. *)

end
