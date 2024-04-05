
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Printing of identifiers *)
(* ************************************************************************* *)

exception Cannot_print of string
(** Exception raised when some input cannot be printed due to lexical
    conventions. In that case, the string contains a message explaining
    why the printing failed. *)

val symbol : Format.formatter -> Dolmen_std.Name.t -> unit
(** Print an identifier, quoting it if necessary. *)

val keyword : Format.formatter -> Dolmen_std.Name.t -> unit
(** Print a keyword. *)

val sanitize : _ Dolmen_intf.Scope.id -> Dolmen_std.Name.t -> Dolmen_std.Name.t
(** Sanitization function to ensure that a name can be printed. *)


(* Printing of Terms and statements *)
(* ************************************************************************* *)

module Make
    (Env : Dolmen_intf.Env.Print
     with type name := Dolmen_std.Name.t)
    (S : Dolmen_intf.View.Sexpr.S
     with type id := Dolmen_std.Id.t)
    (V : Dolmen_intf.View.TFF.S
     with type ty = Env.ty
      and type ty_var = Env.ty_var
      and type ty_cst = Env.ty_cst
      and type term = Env.term
      and type term_var = Env.term_var
      and type term_cst = Env.term_cst
      and type formula = Env.formula)
  : sig

  (** {2 Types and terms} *)

  val ty : Env.t -> Format.formatter -> V.Ty.t -> unit
  (** Printer for types *)


  (** {2 Statements} *)

  val echo : Env.t -> Format.formatter -> string -> unit
  (** *)

  val set_logic : Env.t -> Format.formatter -> string -> unit
  (** Print a set-logic statement. *)

  val set_info : Env.t -> Format.formatter -> S.t -> unit
  (** *)

  val set_option : Env.t -> Format.formatter-> S.t -> unit
  (** *)

  val get_info : Env.t -> Format.formatter -> S.t -> unit
  (** *)

  val get_option : Env.t -> Format.formatter -> S.t -> unit
  (** *)

  val get_value : Env.t -> Format.formatter -> V.Term.t list -> unit
  (** *)

  val pop : Env.t -> Format.formatter -> int -> unit
  (** [pop fmt n] prints a statement that pops `n` levels.
      @raise Cannot_print if the provided level is non-positive *)

  val push : Env.t -> Format.formatter -> int -> unit
  (** [push fmt n] prints a statement that pushes `n` levels.
      @raise Cannot_print if the provided level is non-positive *)

  val declare_sort : Env.t -> Format.formatter -> V.Ty.Cst.t -> unit
  (** Declare a sort, i.e. a type constant. *)

  val declare_datatype :
    Env.t -> Format.formatter ->
    V.Ty.Cst.t * V.Ty.Var.t list *
    (V.Term.Cst.t * (V.Ty.t * V.Term.Cst.t) list) list ->
    unit
  (** Declare a single datatype. *)

  val declare_datatypes :
    Env.t -> Format.formatter ->
    (V.Ty.Cst.t * V.Ty.Var.t list * (V.Term.Cst.t * (V.Ty.t * V.Term.Cst.t) list) list) list ->
    unit
  (** Declare multiple mutually recursive datatypes. *)

  val declare_fun : Env.t -> Format.formatter -> V.Term.Cst.t -> unit
  (** Declare a function, i.e. a term constant. This will use
      either the `declare-fun` or the `declare-const` statement
      depending on the actualy type of the function. *)

  val define_sort :
    Env.t -> Format.formatter ->
    (V.Ty.Cst.t * V.Ty.Var.t list * V.Ty.t) -> unit
  (** *)

  val define_fun :
    Env.t -> Format.formatter ->
    (V.Term.Cst.t * V.Term.Var.t list * V.Term.t) -> unit
  (** *)

  val define_fun_rec :
    Env.t -> Format.formatter ->
    (V.Term.Cst.t * V.Term.Var.t list * V.Term.t) -> unit
  (** *)

  val define_funs_rec :
    Env.t -> Format.formatter ->
    (V.Term.Cst.t * V.Term.Var.t list * V.Term.t) list -> unit
  (** *)

  val assert_ : Env.t -> Format.formatter -> V.Formula.t -> unit
  (** *)

  val check_sat : Env.t -> Format.formatter -> unit -> unit
  (** *)

  val check_sat_assuming : Env.t -> Format.formatter -> V.Formula.t list -> unit
  (** *)

  val reset : Env.t -> Format.formatter -> unit -> unit
  (** Print a `reset` statement. *)

  val reset_assertions : Env.t -> Format.formatter -> unit -> unit
  (** Print a `reset-assertion` statement. *)

  val get_unsat_core : Env.t -> Format.formatter -> unit -> unit
  (** Print a `get-unsat-core` statement. *)

  val get_unsat_assumptions : Env.t -> Format.formatter -> unit -> unit
  (** Print a `get-unsat-assumptions` statement. *)

  val get_proof : Env.t -> Format.formatter -> unit -> unit
  (** Print a `get-proof` statement. *)

  val get_model : Env.t -> Format.formatter -> unit -> unit
  (** Print a `get-model` statement. *)

  val get_assertions : Env.t -> Format.formatter -> unit -> unit
  (** Print a `get-assertions` statement. *)

  val get_assignment : Env.t -> Format.formatter -> unit -> unit
  (** Print a `get-assignment` statement. *)

  val exit : Env.t -> Format.formatter -> unit -> unit
  (** Print an `exit` statement. *)

end
