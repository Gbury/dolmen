
(* This file is free software, part of Dolmen. See file "LICENSE" for more details. *)

(** {2 Some types} *)

exception Out_of_time
exception Out_of_space

(** The module signature that implements everything needed
    to use alarms for time and sizes. *)
module type S = sig

  type t
  (** The type of active alarms. *)

  val setup : time:float -> size:float -> t
  (** Create an alarm ith the given size and time limits. *)

  val delete : t -> unit
  (** Delete an alarm. *)

end

type t = (module S)
(** An alarm system is a packed module. *)


(** {2 Alarm Implementations} *)

val dummy : t
(** Dummy implementation. Does nothing. *)

val linux : t
(** Linux alarms. Uses Gc alarm for size limits, and
    Unix timers for time limits. Fairly accurate and
    reliable. *)

val windows : t
(** Windows alarms. Uses Gc alarm for size limits as
    well as time limits. The time limit is not enforced
    very precisely/reliably because of the use of Gc
    alarms. *)

val default : t
(** A default alarm, chosen based on `Sys.os_type`. Can be
    either {linux} or {windows}, and falls back on {dummy}
    for unknown os (e.g. js_of_ocaml). *)

