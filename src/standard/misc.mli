
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(** {2 Comparison helpers} *)

val (<?>) : int -> (('a -> 'a -> int) * 'a * 'a) -> int
(** Composition of comparison functions. *)

val lexicographic : ('a -> 'a -> int) -> 'a list -> 'a list -> int
(** Lexicogrphic comparison on lsits. *)


(** {2 Hash helpers} *)

val hash_int : int -> int
val hash_string : string -> int
val hash_list : ('a -> int) -> 'a list -> int
val hash2 : int -> int -> int
val hash3 : int -> int -> int -> int
val hash4 : int -> int -> int -> int -> int
(** Hash combinators. *)


(** {2 Misc functions} *)

val get_extension : string -> string
(** Returns the extension of a file, i.e the shortest suffix containing
    the character '.'. Returns an empty string if such a suffix does not exists. *)

val split_on_char : char -> string -> string list
(** Split on characters in a string (see Stdlib's split_on_char). *)

val replicate : int -> 'a -> 'a list
(** Returns a list with [n] times the given value. Returns an empty list if [n] *)

val opt_map : 'a option -> ('a -> 'b) -> 'b option
(** Map on option. *)

val opt_bind : 'a option -> ('a -> 'b option) -> 'b option
(** Bind on option. *)

val string_for_all : (char -> bool) -> string -> bool
(** Equivalent to [String.for_all] (which is only available on ocaml >= 4.10) *)


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


(** {2 Iteration helpers} *)

val foldn : int -> ('a -> 'a) -> 'a -> 'a
(** Applies the given function [n] times. *)

val list_concat_map : ('a -> 'b list) -> 'a list -> 'b list
(** Same as {List.concat_map} (which is not available on ocaml.4.08). *)

(** {2 Lexbuf helpers} *)

val filename_of_input :
  [ `Stdin | `File of string | `Contents of string * string ] ->
  string
(** Filename string of an input. *)

val filename_of_input_source :
  [ `Stdin | `File of string | `Raw of string * string ] ->
  string
(** Filename string of an input source. *)

val mk_lexbuf :
  [ `Stdin | `File of string | `Contents of string * string ] ->
  Lexing.lexbuf * (unit -> unit)
(** Returns the lexbuf associetd with the given file or stdin,
    with the correct filename, together with a function to close
    the associated file descriptor.
    The [`Contents] constructor expect first a name for the input
    stream (to report errors), and then a string with the actual
    contents to be parsed. *)

