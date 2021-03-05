
(* This file is free software, part of Dolmen. See file "LICENSE" for more details. *)

(** {2 Exit codes} *)

type t
(** An exit code, i.e. an integer between 0 and 126. *)

val hash : t -> int
val equal : t -> t -> bool
val compare : t -> t -> int


(** {2 Manipulating error codes} *)

val create : string -> t
(** Create a new exit code. The string given is used as a description
    for the exit code. The create code is active by default. *)

val descr : t -> int * string
(** Return the actual integer associated to the code, *)

val errors : unit -> t list
(** Return the list of all created error exit codes. *)


(** {2 Special exit codes} *)

val ok : t
(** The [0] exit code, signalling everything ran fine. *)

val bug : t
(** Unexpected errors. This uses retcode [125] since this is also
    what cmdliner uses. This code will not appear in the list
    returned by {errors}. *)



(** {2 Predefined exit codes} *)

val generic : t
(** Generic exit code. *)

val limit : t
(* Exit code for when limits are exceeded. *)

val parsing : t
(** Exit code for parsing errors. *)

val typing : t
(** Exit codes for typing errors. *)



(** {2 Exit code status} *)

val exit : t -> _
(** Exit with the given code.
    Note: exit codes can be silenced, in which case the process
    will exit with an exit code of [0]. *)

val is_abort : t -> bool
(** Whether an exit code is active. *)

val abort : t -> unit
(** Make the exit code abort instead of properly exiting. *)

val error : t -> unit
(** Make the exit code properly exit. *)


