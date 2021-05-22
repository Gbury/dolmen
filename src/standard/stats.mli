
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* {2 Global things} *)
(* ************************************************************************* *)

val enabled : bool ref


(* {2 One value statistics} *)
(* ************************************************************************* *)

module Float : sig

  type t
  (** Statistics holding exactly one value *)

  val create : string -> t
  (** Create a float statistics, with the given name. *)

  val print : Format.formatter -> t -> unit
  (** Print the statistic's current value. *)

  val set : t -> float -> unit
  (** Set the stat value. *)

end


(* {2 Multiple values statistics} *)
(* ************************************************************************* *)

module Floats : sig

  type t
  (** The type for a time statistics. *)

  val create : string -> t
  (** Create a time statistics, with the given name. *)

  val print : Format.formatter -> t -> unit
  (** Print the statistic's current values. *)

  val add : t -> float -> unit
  (** Add a time lapse to a time statistics. *)

end

