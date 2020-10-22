
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** Standard implementation of file locations. *)

(** {2 Interface definition} *)

type loc = {
  file : string;
  start_line : int;
  start_column : int;
  stop_line : int;
  stop_column : int;
}
(** A full location, including file, start position and end position.
    Dummy positions (i.e. with [start_line = stop_line] and
    [start_column = stop_column]) are allowed to represent unknown
    locations. *)

type file
(** Meta-data about files to enable more compact location storage. *)

type t
(** A compact representation of locations. *)

type full = {
  file : file;
  loc : t;
}
(* Convenient alias to store a compact location and file info *)


(** {2 Interface definition} *)

module type S = Dolmen_intf.Location.S
(** An anstract module type for providing locations. Used
    as argumentby much of the functors provided in Dolmen. *)

include S with type t := t and type file := file
(** This module implements the signature {S}. *)

val hash : t -> int
(** Hashing function. *)

val eq : t -> t -> bool
(** Location equality. *)

val no_loc : t
(** An dummy location pointing at the first byte of a file. *)

val is_dummy : loc -> bool
(** Is the location ana ctual location, or a dummy one ? *)


(** {2 Compact location handling} *)

val mk_file : string -> file
(** Create a new set of meta-data for the given filename. *)

val new_line : file -> int -> unit
(** Register a new line whose first char is at the given offset. *)


(** {2 Compact<->full translations} *)

val loc : file -> t -> loc
val full_loc : full -> loc
(** Return a complete location from a compact location and meta-data. *)

val compact : loc -> file * t
(** Compactify a full location into a compact representation. *)


(** {2 Printing locations} *)

val pp : Buffer.t -> loc -> unit
val fmt : Format.formatter -> loc -> unit
val fmt_pos : Format.formatter -> loc -> unit
val fmt_hint : Format.formatter -> loc -> unit
(** Printing functions *)

val file_name : file -> string
(** Filename for a file *)

