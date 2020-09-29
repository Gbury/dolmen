
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** Messages

    This module deines an abstraction for text/messages. Using strings to pass around
    text is certainly useful, but suffers from a few problems, mainly the fact that
    id doesn't compose well with Format's boxes: particularly, if a message is generated,
    and then included in another larger message, it is problematic to format the first
    message alone, because then when inlcuded in the larger one, boxes and break hints
    can no longer be used. Fortunaltely, there is a solution: mainly delay printing
    using closures and use format's %t. *)

type t = Format.formatter -> unit
(** A message is a delayed format printer. Such values can be created using
    {Foormat.dprintf}, and consumed using "%t" in format strings *)

