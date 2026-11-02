
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** {2 Typechecker state} *)

module T : Dolmen_type.Thf.S
  with type 'a Tag.t = 'a Dolmen.Std.Tag.t
   and type Ty.t = Dolmen.Std.Expr.ty
   and type Ty.Var.t = Dolmen.Std.Expr.Ty.Var.t
   and type Ty.Const.t = Dolmen.Std.Expr.Ty.Const.t
   and type T.t = Dolmen.Std.Expr.term
   and type T.Var.t = Dolmen.Std.Expr.Term.Var.t
   and type T.Const.t = Dolmen.Std.Expr.Term.Const.t
   and type T.Cstr.t = Dolmen.Std.Expr.Term.Const.t
(** The raw type-checker module. *)

type ty_state
(** The type of state used by the {!Make} functor. *)

val new_state : unit -> ty_state
(** Generate a fresh typing state. *)

val typer_state : ty_state -> T.state
(** Return the underlying typer state. *)

module Ext : sig
  (** Define typing extensions.

      These extensions are typically extensions used by some community,
      but not yet part of the standard.

      @since 0.11 *)

  type t
  (** The type of typing extensions. *)

  val name : t -> string
  (** Extension name, sould be suitable for cli options. *)

  val builtins : t -> Typer_intf.lang -> T.builtin_symbols
  (** Returns the typing builtins from an extension. *)

  val create :
    name:string -> builtins:(Typer_intf.lang -> T.builtin_symbols) -> t
  (** Create a new extension. *)

  val iter : (t -> unit) -> unit
  (** Iterate over all the registered extensions. *)

  val find_all : string -> t list
  (** Returns the extensions that have been registered with the given name.

      @since 0.11 *)

  val bvconv : t
    (** Typing extension to add `bv2nat` and `int2bv`. *)
end

(** {2 Typechecker Functor} *)

module type Typer_Full = Typer_intf.Typer_Full

module Typer(State : State.S)
  : Typer_Full with type state = State.t
                and type 'a key := 'a State.key
                and type ty_state = ty_state
                and type env = T.env
                and type 'a fragment = 'a T.fragment
                and type error = T.error
                and type warning = T.warning
                and type builtin_symbols = T.builtin_symbols
                and type extension = Ext.t

(** {2 Convenience Types functor} *)

type +'a stmt = 'a Typer_intf.stmt = {
  id : Dolmen.Std.Id.t;
  loc : Dolmen.Std.Loc.t;
  contents  : 'a;
  attrs       : Dolmen.Std.Term.t list;
  implicit    : bool;
}
(** Re-export of the typed statement type. *)

module type Types = Typer_intf.Types

module Types
    (Expr : Expr_intf.S)
    : Typer_intf.Types
      with type ty = Expr.ty
       and type ty_var = Expr.ty_var
       and type ty_cst = Expr.ty_cst
       and type term = Expr.term
       and type term_var = Expr.term_var
       and type term_cst = Expr.term_cst
       and type formula = Expr.formula

(** {2 Typechecker Pipe} *)

module type S = Typer_intf.S
module type Typer = Typer_intf.Typer

module Make
    (Expr : Expr_intf.S)
    (Print : Expr_intf.Print
     with type ty := Expr.ty
      and type ty_var := Expr.ty_var
      and type ty_cst := Expr.ty_cst
      and type ty_def := Expr.ty_def
      and type term := Expr.term
      and type term_var := Expr.term_var
      and type term_cst := Expr.term_cst
      and type formula := Expr.formula)
    (State : State.S)
    (Typer : Typer
     with type state := State.t
      and type ty := Expr.ty
      and type ty_var := Expr.ty_var
      and type ty_cst := Expr.ty_cst
      and type ty_def := Expr.ty_def
      and type term := Expr.term
      and type term_var := Expr.term_var
      and type term_cst := Expr.term_cst
      and type formula := Expr.formula)
  : S with type state = State.t
       and type 'a key = 'a State.key
       and type ty = Expr.ty
       and type ty_var = Expr.ty_var
       and type ty_cst = Expr.ty_cst
       and type ty_def = Expr.ty_def
       and type term = Expr.term
       and type term_var = Expr.term_var
       and type term_cst = Expr.term_cst
       and type formula = Expr.formula


(** {2 Warnings} *)

val unused_type_variable : (T.var_kind * Dolmen.Std.Expr.Ty.Var.t) Report.Warning.t
(** *)

val unused_term_variable : (T.var_kind * Dolmen.Std.Expr.Term.Var.t) Report.Warning.t
(** *)

val unknown_attribute : Dolmen.Std.Id.t Report.Warning.t
(** *)

val superfluous_destructor : unit Report.Warning.t
(** *)

val shadowing : (Dolmen.Std.Id.t * T.binding) Report.Warning.t
(** *)

val redundant_pattern : Dolmen.Std.Expr.term Report.Warning.t
(** *)

val bad_arith_expr : (Dolmen_type.Arith.Smtlib2.config * string) Report.Warning.t
(** Warning for arithmetic expressions that do not strictly conform to the
    specification (e.g. linear arithmetic), but are close enough that they
    should probably be accepted. *)

val array_extension : Dolmen.Std.Id.t Report.Warning.t
(** *)

val logic_reset : Dolmen.Std.Loc.loc Report.Warning.t
(** *)

val unknown_logic : string Report.Warning.t
(** Unknown logic warning *)

val set_logic_not_supported : unit Report.Warning.t
(** *)

val unknown_warning : string Report.Warning.t
(** *)

val dumb_polymorphism : (T.env * Dolmen.Std.Expr.ty * Dolmen.Std.Expr.ty_var * T.wildcard_source list) Report.Warning.t
(** *)


(** {2 Errors} *)

val bad_option : (Dolmen.Std.Term.t * (Format.formatter -> unit)) Report.Error.t
(** *)

val not_well_founded_datatype : unit Report.Error.t
(** *)

val expect_error : (string * T.res option) Report.Error.t
(** *)

val bad_index_arity : (string * T.arity * int) Report.Error.t
(** *)

val bad_type_arity : (Dolmen.Std.Expr.Ty.Const.t * int) Report.Error.t
(** *)

val bad_op_arity : (T.symbol * T.arity * int) Report.Error.t
(** *)

val bad_cstr_arity : (Dolmen.Std.Expr.Term.Field.t * T.arity * int) Report.Error.t
(** *)

val bad_term_arity : (Dolmen.Std.Expr.Term.Const.t * T.arity * int) Report.Error.t
(** *)

val bad_poly_arity : (Dolmen.Std.Expr.ty_var list * Dolmen.Std.Expr.ty list) Report.Error.t
(** *)

val over_application : Dolmen.Std.Expr.term list Report.Error.t
(** *)

val partial_pattern_match : Dolmen.Std.Expr.term list Report.Error.t
(** *)

val mismatch_sum_type : (Dolmen.Std.Expr.Term.Cstr.t * Dolmen.Std.Expr.ty) Report.Error.t
(** *)

val repeated_record_field : Dolmen.Std.Expr.Term.Field.t Report.Error.t
(** *)

val missing_record_field : Dolmen.Std.Expr.Term.Field.t Report.Error.t
(** *)

val mismatch_record_type : (Dolmen.Std.Expr.Term.Field.t * Dolmen.Std.Expr.Ty.Const.t) Report.Error.t
(** *)

val ty_var_application : Dolmen.Std.Expr.ty_var Report.Error.t
(** *)

val var_application : Dolmen.Std.Expr.term_var Report.Error.t
(** *)

val type_mismatch : (Dolmen.Std.Expr.term * Dolmen.Std.Expr.ty) Report.Error.t
(** *)

val var_in_binding_pos_underspecified : unit Report.Error.t
(** *)

val overlapping_parallel_bindings : (Dolmen.Std.Id.t * Dolmen.Std.Loc.full) Report.Error.t
(** *)

val overlapping_smt_fun_def_params : (Dolmen.Std.Id.t * T.binding) Report.Error.t
(** *)

val unhandled_builtin : Dolmen.Std.Term.builtin Report.Error.t
(** *)

val cannot_tag_tag : unit Report.Error.t
(** *)

val cannot_tag_ttype : unit Report.Error.t
(** *)

val unbound_identifier : (Dolmen.Std.Id.t * string * bool) Report.Error.t
(** *)

val multiple_declarations : (Dolmen.Std.Id.t * T.binding) Report.Error.t
(** *)

val forbidden_quant : unit Report.Error.t
(** *)

val missing_destructor : Dolmen.Std.Id.t Report.Error.t
(** *)

val type_def_rec : Dolmen.Std.Statement.def Report.Error.t
(** *)

val id_definition_conflict : (Dolmen.Std.Id.t * T.binding) Report.Error.t
(** *)

val higher_order_app : unit Report.Error.t
(** *)

val higher_order_type : unit Report.Error.t
(** *)

val higher_order_env_in_tff_typer : unit Report.Error.t
(** *)

val poly_arg : unit Report.Error.t
(** *)

val non_prenex_polymorphism : Dolmen.Std.Expr.ty Report.Error.t
(** *)

val inference_forbidden : (T.env * T.wildcard_source * Dolmen.Std.Expr.ty) Report.Error.t
(** *)

val inference_conflict : (T.env * T.wildcard_source * Dolmen.Std.Expr.ty * Dolmen.Std.Expr.ty list) Report.Error.t
(** *)

val inference_scope_escape : (T.env * T.wildcard_source * Dolmen.Std.Expr.ty_var * T.reason option) Report.Error.t
(** *)

val unbound_type_wildcards : (T.env * (Dolmen.Std.Expr.ty_var * T.wildcard_source list) list) Report.Error.t
(** *)

val unbound_implicit_type_vars : (Dolmen.Std.Expr.ty_var * T.reason) list Report.Error.t
(** *)

val incoherent_type_redefinition : (Dolmen.Std.Id.t * Dolmen.Std.Expr.Ty.Const.t * T.reason * int) Report.Error.t
(** *)

val incoherent_term_redefinition : (Dolmen.Std.Id.t * Dolmen.Std.Expr.Term.Const.t * T.reason * Dolmen.Std.Expr.ty) Report.Error.t
(** *)

val unhandled_ast : (T.env * Dolmen.Std.Term.t T.fragment) Report.Error.t
(** *)

val bad_farray_arity : unit Report.Error.t
(** *)

val expected_arith_type : (Dolmen.Std.Expr.ty * string) Report.Error.t
(** *)

val expected_specific_arith_type : Dolmen.Std.Expr.ty Report.Error.t
(** *)

val forbidden_array_sort: string Report.Error.t
(** *)

val forbidden_arith_expr : (Dolmen_type.Arith.Smtlib2.config * string) Report.Error.t
(** Error for arithmetic expressions that do not respect the specification (e.g.
    non-linear expressions, expressions not in difference logic, etc...). *)

val bitvector_app_expected_nat_lit : Dolmen.Std.Term.t Report.Error.t
(** *)

val non_positive_bitv_size : int Report.Error.t
(** *)

val invalid_smt2_bitv_extract : (int * int * int) Report.Error.t
(** *)

val invalid_bin_bitvector_char : char Report.Error.t
(** *)

val invalid_hex_bitvector_char : char Report.Error.t
(** *)

val invalid_dec_bitvector_char : char Report.Error.t
(** *)

val invalid_hex_string_char : string Report.Error.t
(** *)

val invalid_string_char : char Report.Error.t
(** *)

val invalid_string_escape_sequence : (string * int) Report.Error.t
(** *)

val bad_tptp_kind : string option Report.Error.t
(** *)

val missing_smtlib_logic : unit Report.Error.t
(** *)

val illegal_decl : unit Report.Error.t
(** *)

val illegal_def : unit Report.Error.t
(** *)

val invalid_push : unit Report.Error.t
(** *)

val invalid_pop : unit Report.Error.t
(** *)

val empty_pop : unit Report.Error.t
(** *)

val reserved : (Dolmen.Std.Id.t * [ `Solver of string ]) Report.Error.t
(** *)

val incorrect_sexpression : (Format.formatter -> unit) Report.Error.t
(** *)

val non_closed_named_term : (Dolmen.Std.Expr.Ty.Var.t list * Dolmen.Std.Expr.Term.Var.t list) Report.Error.t
(** *)

val unknown_error : string Report.Error.t
(** *)

