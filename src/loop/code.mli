
(* This file is free software, part of Dolmen. See file "LICENSE" for more details. *)

(** {2 Exit codes} *)

type t
(** An abstraction over exit status.

    Concretely this corresponds to an exit/return code for the binary (in the case of
    the dolmen binary or solver-like binaries).

    In practice, the exact error code corresponding to a value of this type can be set
    later. This is useful so that users of the library can decide the codes, and among
    other things, ensure that error codes are stable. *)

val hash : t -> int
val equal : t -> t -> bool
val compare : t -> t -> int
(** Usual functions *)


(** {2 Manipulating error codes} *)

val init : ?full:bool -> (t * int) list -> unit
(** Initialise all retcodes with the given association list.
    All codes that are not present in the list are assigned a arbitrary
    free return code.
    If [~full] is [true], and not all retcodes are present in the association
    list, then this function with raise a [Failure _] exception. *)

val create : category:string -> descr:string -> t
(** Create a new exit code. The string given is used as a description
    for the exit code. The create code is active by default. *)

val descr : t -> int * string
(** Return the actual integer associated to the code, *)

val category : t -> string
(** Category (used mainly for report documentation). *)

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
(** Exit with the given code (or abort if the exit code is marked as
    an abort code). *)

val is_abort : t -> bool
(** Whether an exit code is active. *)

val abort : t -> unit
(** Make the exit code abort instead of properly exiting. *)

val error : t -> unit
(** Make the exit code properly exit. *)


