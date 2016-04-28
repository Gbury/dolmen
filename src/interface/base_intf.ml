
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** Base interface.
    Base interface for most signatures used throughout Dolmen. *)

(** {2 Basic signature} *)

module type S = sig

  (** Signature common to all other signatures *)

  type t
  (** The type of elements of this module. *)

  type location
  (** The type of locations attached to elements. *)

end


