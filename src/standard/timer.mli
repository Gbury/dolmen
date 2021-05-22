
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** {2 Simple timer} *)
(*  ************************************************************************* *)

type t
(** The type of a timer. *)

val start : unit -> t
(** Start a timer. *)

val stop : t -> float
(** Stop a timer and return the total time of the timer. *)

