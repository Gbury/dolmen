
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** External Typechecker interface

    This module defines the external typechcker interface, that is,
    the interface of an instantiated typechecker. *)

(** {1 Typechecker interface} *)

(** Typechecker interface *)
module type S = sig

  (** {2 Module aliases} *)
  module Tag: Dolmen.Intf.Tag.S
  module Ty: Dolmen.Intf.Ty.Tff
    with type 'a tag := 'a Tag.t
  module T: Dolmen.Intf.Term.Tff
    with type ty := Ty.t
     and type ty_var := Ty.Var.t
     and type ty_const := Ty.Const.t
     and type 'a tag := 'a Tag.t

  (** {2 Type definitions} *)

  type poly =
    | Explicit
    (** Type arguments must be explicitly given in funciton applications *)
    | Implicit
    (** Type arguments are not given in funciton applications, and instead
        type annotations/coercions are used to disambiguate applications
        of polymorphic symbols. *)
    | Flexible
    (** Mix between explicit and implicit: depending on the arity of a
        symbol and the number of arguments provided, either the provided
        type arguments are used, or wildcards are generated for all of them,
        and later instantiated when needed. *)
  (** The various polymorphism mode for the typechecker *)

  type expect =
    | Nothing
    | Type
    | Typed of Ty.t
    (** The type of expected result when typing an expression, used to infer
        non-declared symbols. *)

  type tag = Any : 'a Tag.t * 'a -> tag
  (** Existencial wrapper around tags *)

  type res =
    | Ttype
    | Ty    of Ty.t
    | Term  of T.t
    | Tags  of tag list (**)
  (** The results of parsing an untyped term.  *)

  type builtin_res = [
    | `Ttype of (Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> unit)
    | `Ty    of (Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> Ty.t)
    | `Term  of (Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> T.t)
    | `Tags  of (Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> tag list)
  ]
  (** The result of parsing a symbol by the theory *)

  type not_found = [ `Not_found ]
  (** Not bound bindings *)

  type var = [
    | `Ty_var of Ty.Var.t
    | `Term_var of T.Var.t
    | `Letin of Dolmen.Std.Term.t * T.Var.t * T.t
  ]
  (** Variable bindings *)

  type cst = [
    | `Cstr of T.Cstr.t
    | `Field of T.Field.t
    | `Ty_cst of Ty.Const.t
    | `Term_cst of T.Const.t
  ]
  (** Constant bindings *)

  type builtin = [
    | `Builtin of builtin_res
  ]
  (** Builtin binding *)

  type bound = [ var | cst | builtin ]
  (* All internal bindings *)

  type inferred =
    | Ty_fun of Ty.Const.t
    | Term_fun of T.Const.t (**)
  (** The type for inferred symbols. *)

  type reason =
    | Builtin
    | Bound of Dolmen.Std.Loc.file * Dolmen.Std.Term.t
    | Inferred of Dolmen.Std.Loc.file * Dolmen.Std.Term.t
    | Defined of Dolmen.Std.Loc.file * Dolmen.Std.Statement.def
    | Declared of Dolmen.Std.Loc.file * Dolmen.Std.Statement.decl
  (** The type of reasons for constant typing *)

  type binding = [
    | `Not_found
    | `Builtin of [
        | `Ttype
        | `Ty
        | `Term
        | `Tag
      ]
    | `Variable of [
        | `Ty of Ty.Var.t * reason option
        | `Term of T.Var.t * reason option
      ]
    | `Constant of [
        | `Ty of Ty.Const.t * reason option
        | `Cstr of T.Cstr.t * reason option
        | `Term of T.Const.t * reason option
        | `Field of T.Field.t * reason option
      ]
  ]
  (** The bindings that can occur. *)


  (** {2 Errors and warnings} *)

  type _ fragment =
    | Ast : Dolmen.Std.Term.t -> Dolmen.Std.Term.t fragment
    | Def : Dolmen.Std.Statement.def -> Dolmen.Std.Statement.def fragment
    | Defs : Dolmen.Std.Statement.defs -> Dolmen.Std.Statement.defs fragment
    | Decl : Dolmen.Std.Statement.decl -> Dolmen.Std.Statement.decl fragment
    | Decls : Dolmen.Std.Statement.decls -> Dolmen.Std.Statement.decls fragment
    | Located : Dolmen.Std.Loc.t -> Dolmen.Std.Loc.t fragment (**)
  (** Fragments of input that represent the sources of warnings/errors *)

  type _ warn = ..
  (** The type of warnings, parameterized by the type of fragment they can
      trigger on *)

  type _ warn +=
    | Unused_type_variable : Ty.Var.t -> Dolmen.Std.Term.t warn
    (** Unused quantified type variable *)
    | Unused_term_variable : T.Var.t -> Dolmen.Std.Term.t warn
    (** Unused quantified term variable *)
    | Error_in_attribute : exn -> Dolmen.Std.Term.t warn
    (** An error occurred wile parsing an attribute *)
    | Superfluous_destructor :
        Dolmen.Std.Id.t * Dolmen.Std.Id.t * T.Const.t -> Dolmen.Std.Term.t warn
    (** The user implementation of typed terms returned a destructor where
        was asked for. This warning can very safely be ignored. *)
  (** Warnings that cna trigger on regular parsed terms. *)

  type _ warn +=
    | Shadowing : Dolmen.Std.Id.t * binding * binding -> _ warn
    (** Shadowing of the given identifier,
        together with the old and current binding. *)
  (** Special case of warnings for shadowing, as it can happen both from a
      term but also a declaration, hence why the type variable of [warn] is
      left wild. *)

  type _ err = ..
  (** The type of errors, parameterized by the type of fragment they can
      trigger on *)

  type _ err +=
    | Not_well_founded_datatypes :
        Dolmen.Std.Statement.decl list -> Dolmen.Std.Statement.decls err
    (** Not well-dounded datatypes definitions. *)
  (** Errors that occur on declaration(s) *)

  type _ err +=
    | Infer_type_variable : Dolmen.Std.Term.t err
    (** The type of a bound variable had to be inferred which is forbidden. *)
    | Expected : string * res option -> Dolmen.Std.Term.t err
    (** The parsed term didn't match the expected shape *)
    | Bad_index_arity : string * int * int -> Dolmen.Std.Term.t err
    (** [Bad_index_arity (name, expected, actual)] denotes an error where
        an indexed family of operators (based on [name]) expect to be indexed
        by [expected] arguments but got [actual] instead. *)
    | Bad_ty_arity : Ty.Const.t * int -> Dolmen.Std.Term.t err
    (** [Bad_ty_arity (cst, actual)] denotes a type constant that was applied
        to [actual] arguments, but which has a different arity (which should
        be accessible by getting its type/sort/arity). *)
    | Bad_op_arity : string * int list * int -> Dolmen.Std.Term.t err
    (** [Bad_op_arity (name, expected, actual)] denotes a named operator
        (which may be a builtin operator, a top-level defined constant which
        is being subtituted, etc...) expecting a number of arguments among
        the [expected] list, but instead got [actual] number of arguments. *)
    | Bad_cstr_arity : T.Cstr.t * int list * int -> Dolmen.Std.Term.t err
    (** [Bad_cstr_arity (cstr, expected, actual)] denotes an ADT constructor,
        which was expecting one of [expected] arguments, but which was applied
        to [actual] arguments. *)
    | Bad_term_arity : T.Const.t * int list * int -> Dolmen.Std.Term.t err
    (** [Bad_term_arity (func, expected, actual)] denotes a funciton symbol,
        which was expecting one of [expected] arguments, but which was applied
        to [actual] arguments. *)
    | Repeated_record_field : T.Field.t -> Dolmen.Std.Term.t err
    (** [Repeated_record_field f] denotes an error within an expression
        that builds a record by giving values to all fields, but where the
        field [f] appears more than once. *)
    | Missing_record_field : T.Field.t -> Dolmen.Std.Term.t err
    (** [Missing_record_field f] denotes an error within an expression
        that builds a record by giving values to all fields, but where the
        field [f] does not appear. *)
    | Mismatch_record_type : T.Field.t * Ty.Const.t -> Dolmen.Std.Term.t err
    (** [Mismatch_record_type (f, r)] denotes an error where while building
        a record expression for a record of type [c], a field [f] belonging
        to another record type was used. *)
    | Var_application : T.Var.t -> Dolmen.Std.Term.t err
    (** [Var_application v] denotes a variable which was applied to other
        terms, which is forbidden in first-order formulas. *)
    | Ty_var_application : Ty.Var.t -> Dolmen.Std.Term.t err
    (** [Ty_var_application v] denotes a type variable which was applied to
        other terms, which is forbidden in first-order formulas. *)
    | Type_mismatch : T.t * Ty.t -> Dolmen.Std.Term.t err
    (** *)
    | Quantified_var_inference : Dolmen.Std.Term.t err
    (** Quantified variable without a type *)
    | Unhandled_builtin : Dolmen.Std.Term.builtin -> Dolmen.Std.Term.t err
    (** *)
    | Cannot_tag_tag : Dolmen.Std.Term.t err
    (** *)
    | Cannot_tag_ttype : Dolmen.Std.Term.t err
    (** *)
    | Cannot_find : Dolmen.Std.Id.t -> Dolmen.Std.Term.t err
    (** *)
    | Type_var_in_type_constructor : Dolmen.Std.Term.t err
    (** *)
    | Forbidden_quantifier : Dolmen.Std.Term.t err
    (** *)
    | Missing_destructor : Dolmen.Std.Id.t -> Dolmen.Std.Term.t err
    (** *)
    | Type_def_rec : Dolmen.Std.Statement.def -> Dolmen.Std.Statement.defs err
    (** *)
    | Higher_order_application : Dolmen.Std.Term.t err
    (** *)
    | Higher_order_type : Dolmen.Std.Term.t err
    (** *)
    | Unbound_variables : Ty.Var.t list * T.Var.t list * T.t -> Dolmen.Std.Term.t err
    (** *)
    | Uncaught_exn : exn * Printexc.raw_backtrace -> Dolmen.Std.Term.t err
    (** *)
    | Unhandled_ast : Dolmen.Std.Term.t err
    (** *)
  (** Errors that occur on regular parsed terms. *)


  (** {2 Global State} *)

  type state
  (** The type of mutable state for typechecking. *)

  val new_state : unit -> state
  (** Create a new state. *)

  val copy_state : state -> state
  (** Make a copy of the global state included in the env *)


  (** {2 Typing Environment} *)

  type env
  (** The type of environments for typechecking. *)

  type 'a typer = env -> Dolmen.Std.Term.t -> 'a
  (** A general type for typers. Takes a local environment and the current untyped term,
      and return a value. The typer may need additional information for parsing,
      in which case the return value will be a function.
      @raise Typing_error *)

  type symbol =
    | Id of Dolmen.Std.Id.t
    | Builtin of Dolmen.Std.Term.builtin
    (** Wrapper around potential function symbols from the Dolmen AST. *)

  type builtin_symbols = env -> symbol -> [ builtin_res | not_found ]
  (** The type of a typer for builtin symbols. Given the environment and a symbol,
      the theory should return a typing function if the symbol belongs to the
      theory. This typing function takes first the ast term of the whole
      application that is beign typechecked, and the list of arguments to the
      symbol. *)

  type warning =
    | Warning : env * 'a fragment * 'a warn -> warning (**)
  (** Existential wrapper around warnings *)

  type error =
    | Error : env * 'a fragment * 'a err -> error (**)
  (** Existential wrapper around errors *)

  exception Typing_error of error
  (** Exception for typing errors *)

  val empty_env :
    ?st:state -> ?expect:expect ->
    ?infer_hook:(env -> inferred -> unit) ->
    ?infer_base:Ty.t -> ?poly:poly -> ?quants:bool ->
    warnings:(warning -> unit) ->
    file:Dolmen.Std.Loc.file ->
    builtin_symbols -> env
  (** Create a new environment. *)

  val expect : ?force:bool -> env -> expect -> env
  (** Returns the same environment but with the given expectation,
      except if the environnement already except [Nothing]. *)


  (** {2 Location helpers} *)

  val fragment_loc : env -> _ fragment -> Dolmen.Std.Loc.full
  (** Convenient function to get the location of a fragment. *)

  val binding_reason : binding -> reason option
  (** Extract the reason from a binding
      @raise Invalid_argument if the binding is [`Not_found] *)


  (** {2 Builtin helpers} *)

  val find_var : env -> Dolmen.Std.Id.t -> [ var | not_found ]
  (** Try and find the given id in the set of locally bound variables. *)

  val find_global : env -> Dolmen.Std.Id.t -> [ cst | not_found ]
  (** Try and find the given id in the set of globally bound constants. *)

  val find_builtin : env -> Dolmen.Std.Id.t -> [ builtin | not_found ]
  (** Try and find the given id in the set of bound builtin symbols. *)

  val find_bound : env -> Dolmen.Std.Id.t -> [ bound | not_found ]
  (** Try and find a bound identifier in the env, whetehr it be locally bound
      (such as bound variables), constants bound at top-level, or builtin
      symbols bound by the builtin theory. *)

  val get_global_custom : env -> 'a Dolmen.Std.Tag.t -> 'a option
  (** Get a custom value from the global environment. *)

  val set_global_custom : env -> 'a Dolmen.Std.Tag.t -> 'a -> unit
  (** Set a custom value in the global environment. *)


  (** {2 Errors & Warnings} *)

  val _warn : env -> 'a fragment -> 'a warn -> unit
  (** Emit a warning *)

  val _error : env -> 'a fragment -> 'a err -> _
  (** Raise an error *)

  val suggest : limit:int -> env -> Dolmen.Std.Id.t -> Dolmen.Std.Id.t list
  (** From a dolmen identifier, return a list of existing bound identifiers
      in the env that are up to [~limit] in terms of distance of edition. *)


  (** {2 Parsing functions} *)

  val parse_expr : res typer
  (** Main parsing function. *)

  val parse_ty : Ty.t typer
  val parse_term : T.t typer
  val parse_prop : T.t typer
  (** Wrappers around {parse_expr} to set the expect field of the env,
      and unwrap an expected return value. *)

  val parse_app_ty : (Ty.Const.t -> Dolmen.Std.Term.t list -> res) typer
  val parse_app_term : (T.Const.t -> Dolmen.Std.Term.t list -> res) typer
  (** Function used for parsing applications. The first dolmen term given
      is the application term being parsed (used for reporting errors). *)

  val unwrap_ty : env -> Dolmen.Std.Term.t -> res -> Ty.t
  val unwrap_term : env -> Dolmen.Std.Term.t -> res -> T.t
  (** Unwrap a result, raising the adequate typing error
      if the result if not as expected. *)


  (** {2 High-level functions} *)

  val decls :
    env -> ?attr:Dolmen.Std.Term.t ->
    Dolmen.Std.Statement.decls -> [
      | `Type_decl of Ty.Const.t
      | `Term_decl of T.Const.t
    ] list
  (** Parse a list of potentially mutually recursive declarations. *)

  val defs :
    env -> ?attr:Dolmen.Std.Term.t ->
    Dolmen.Std.Statement.defs -> [
      | `Type_def of Dolmen.Std.Id.t * Ty.Const.t * Ty.Var.t list * Ty.t
      | `Term_def of Dolmen.Std.Id.t * T.Const.t * Ty.Var.t list * T.Var.t list * T.t
    ] list
  (** Parse a definition *)

  val parse : T.t typer
  (** Parse a formula *)

end

