
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** Interfaces for statements.
    This module defines interfaces for statements, i.e top-level
    declarations in files. *)

module type Logic = sig

  include Base_intf.S

  type term
  (** The type of terms used in statements. *)

  (** {2 Optional nfos for statements} *)

  val attr  : ?loc:location -> string -> term
  val annot : ?loc:location -> term -> term list -> term

  (** {2 Generic statements} *)

  val include_ : ?loc:location -> string -> term list -> t

  (** {2 Dimacs Statements} *)

  val clause      : ?loc:location -> term list -> t

  (** {2 Smtlib statements} *)

  val pop         : ?loc:location -> int -> t
  val push        : ?loc:location -> int -> t

  val assert_     : ?loc:location -> term -> t
  val check_sat   : ?loc:location -> unit -> t

  val set_logic   : ?loc:location -> string -> t

  val get_info    : ?loc:location -> string -> t
  val set_info    : ?loc:location -> string * term option -> t

  val get_option  : ?loc:location -> string -> t
  val set_option  : ?loc:location -> string * term option -> t

  val new_type    : ?loc:location -> string -> int -> t
  val type_alias  : ?loc:location -> string -> term list -> term -> t
  val type_cstr   : ?loc:location -> string -> term list -> term -> t
  val fun_def     : ?loc:location -> string -> term list -> term -> term -> t

  val get_proof       : ?loc:location -> unit -> t
  val get_unsat_core  : ?loc:location -> unit -> t
  val get_value       : ?loc:location -> term list -> t
  val get_assignment  : ?loc:location -> unit -> t
  val get_assertions  : ?loc:location -> unit -> t

  val exit : ?loc:location -> unit -> t

  (** {2 TPTP Statements} *)

  val tpi : ?loc:location -> ?annot:term -> term -> string -> term -> t
  val thf : ?loc:location -> ?annot:term -> term -> string -> term -> t
  val tff : ?loc:location -> ?annot:term -> term -> string -> term -> t
  val fof : ?loc:location -> ?annot:term -> term -> string -> term -> t
  val cnf : ?loc:location -> ?annot:term -> term -> string -> term -> t

  (** {2 Zipperposition statements} *)

  val data        : ?loc:location -> t list -> t
  val decl        : ?loc:location -> string -> term -> t
  val definition  : ?loc:location -> string -> term -> term -> t
  val inductive   : ?loc:location -> string -> term list -> (string * term list) list -> t

  val goal        : ?loc:location -> ?attr:term -> term -> t
  val assume      : ?loc:location -> ?attr:term -> term -> t
  val rewrite     : ?loc:location -> ?attr:term -> term -> t

end
