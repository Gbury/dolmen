
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Smtlib2 printing *)
(* ************************************************************************* *)

module type Smtlib2 = sig

  type env
  type sexpr
  type ty
  type ty_var
  type ty_cst
  type term
  type term_var
  type term_cst


  (** {2 Env} *)

  val add_named : env -> term_cst -> term -> env
  (** Add a `:named` definition to the env. *)

  val set_split_dec : env -> (string -> ([ `Pos | `Neg ] * string * string) option) -> env
  (** Set a splitting function for decimals. *)


  (** {2 Helpers} *)

  val match_prop_literal : term -> [
      | `Cst of term_cst
      | `Neg of term_cst
      | `Not_a_prop_literal ]
  (** Match against prop literals. *)


  (** {2 Types and terms} *)

  val ty : env -> Format.formatter -> ty -> unit
  (** Printer for types *)

  val term : env -> Format.formatter -> term -> unit
  (** Printer for terms *)


  (** {2 Statements} *)

  val echo : env -> Format.formatter -> string -> unit
  (** *)

  val set_logic : env -> Format.formatter -> string -> unit
  (** Print a set-logic statement. *)

  val set_info : env -> Format.formatter -> sexpr -> unit
  (** *)

  val set_option : env -> Format.formatter-> sexpr -> unit
  (** *)

  val get_info : env -> Format.formatter -> sexpr -> unit
  (** *)

  val get_option : env -> Format.formatter -> sexpr -> unit
  (** *)

  val get_value : env -> Format.formatter -> term list -> unit
  (** *)

  val pop : env -> Format.formatter -> int -> unit
  (** [pop fmt n] prints a statement that pops `n` levels.
      @raise Cannot_print if the provided level is non-positive *)

  val push : env -> Format.formatter -> int -> unit
  (** [push fmt n] prints a statement that pushes `n` levels.
      @raise Cannot_print if the provided level is non-positive *)

  val declare_sort : env -> Format.formatter -> ty_cst -> unit
  (** Declare a sort, i.e. a type constant. *)

  val declare_datatype :
    env -> Format.formatter ->
    ty_cst * ty_var list *
    (term_cst * (ty * term_cst) list) list ->
    unit
  (** Declare a single datatype. *)

  val declare_datatypes :
    env -> Format.formatter ->
    (ty_cst * ty_var list * (term_cst * (ty * term_cst) list) list) list ->
    unit
  (** Declare multiple mutually recursive datatypes. *)

  val declare_fun : env -> Format.formatter -> term_cst -> unit
  (** Declare a function, i.e. a term constant. This will use
      either the `declare-fun` or the `declare-const` statement
      depending on the actualy type of the function. *)

  val define_sort :
    env -> Format.formatter ->
    (ty_cst * ty_var list * ty) -> unit
  (** *)

  val define_fun :
    env -> Format.formatter ->
    (term_cst * ty_var list * term_var list * term) -> unit
  (** *)

  val define_fun_rec :
    env -> Format.formatter ->
    (term_cst * ty_var list * term_var list * term) -> unit
  (** *)

  val define_funs_rec :
    env -> Format.formatter ->
    (term_cst * ty_var list * term_var list * term) list -> unit
  (** *)

  val assert_ : env -> Format.formatter -> term -> unit
  (** *)

  val check_sat : env -> Format.formatter -> unit -> unit
  (** *)

  val check_sat_assuming : env -> Format.formatter -> term list -> unit
  (** *)

  val reset : env -> Format.formatter -> unit -> unit
  (** Print a `reset` statement. *)

  val reset_assertions : env -> Format.formatter -> unit -> unit
  (** Print a `reset-assertion` statement. *)

  val get_unsat_core : env -> Format.formatter -> unit -> unit
  (** Print a `get-unsat-core` statement. *)

  val get_unsat_assumptions : env -> Format.formatter -> unit -> unit
  (** Print a `get-unsat-assumptions` statement. *)

  val get_proof : env -> Format.formatter -> unit -> unit
  (** Print a `get-proof` statement. *)

  val get_model : env -> Format.formatter -> unit -> unit
  (** Print a `get-model` statement. *)

  val get_assertions : env -> Format.formatter -> unit -> unit
  (** Print a `get-assertions` statement. *)

  val get_assignment : env -> Format.formatter -> unit -> unit
  (** Print a `get-assignment` statement. *)

  val exit : env -> Format.formatter -> unit -> unit
  (** Print an `exit` statement. *)

end

