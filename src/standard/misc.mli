
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(** {2 Misc functions} *)

val get_extension : string -> string
(** Returns the extension of a file, i.e the shortest suffix containing
    the character '.'. Returns an empty string if such a suffix does not exists. *)

val replicate : int -> 'a -> 'a list
(** Returns a list with [n] times the given value. Returns an empty list if [n] *)

(** {2 Printing helpers} *)

val pp_opt :
  ?none:string -> (Buffer.t -> 'a -> unit) ->
  Buffer.t -> 'a option -> unit
(** Print an option *)

val pp_list :
  pp_sep:(Buffer.t -> 'a -> unit) -> sep:'a ->
  pp:(Buffer.t -> 'b -> unit) -> Buffer.t -> 'b list -> unit
(** Print a list with separator into a buffer *)

val print_opt :
  ?none:string -> (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a option -> unit
(** Print an option. *)

val print_list :
  print_sep:(Format.formatter -> 'a -> unit) -> sep:'a ->
  print:(Format.formatter -> 'b -> unit) -> Format.formatter -> 'b list -> unit
(** Print a list with separator into a buffer *)

