
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(** Paths

    Paths are used to identify constants and variables after typechecking.
    They are meant to identify the abstract location of where constants and
    variables come from. Variables are always local, whereas constants
    are identified by a full absolute path including all module names
    leading to the module defining the constant.
*)

(** {2 Type definition} *)

type path = string list
(** A path of module names. A {path} identifies a module, or
    the toplevel/implicitly global module if empty. *)

type t = private
  | Local of {
      name : string;
    }
  (** A local path, mainly used for variables. *)
  | Absolute of {
      path : path;
      name : string;
    }
  (** An absolute path, containing a path to a module,
      and a basename. *)
(** Paths used for variables and constants. *)


(** {2 Std functions} *)

val print : Format.formatter -> t -> unit
(** Printing function. *)


(** {2 Creation function} *)

val local : string -> t
(** Create a local path. *)

val global : string -> t
(** Create a global path. *)

val absolute : path -> string -> t
(** Create an absolute path. *)

val rename : (string -> string) -> t -> t
(** Change the basename of a path. *)
