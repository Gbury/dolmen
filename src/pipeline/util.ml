
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

module type S = sig

  type opt
  (** The type of values recording options for the current run. *)

  val time_limit : opt -> float
  (** The time limit for one original statement (in seconds). *)

  val size_limit : opt -> float
  (** The size limit for one original statement (in octets). *)

  val error : ('a, Format.formatter, unit, unit) format4 -> 'a
  (** A function to log error messages. *)

end
