
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** External Typechecker interface

    This module defines the external typechcker interface, that is,
    the interface of an instantiated typechecker. *)

(** {1 Typechecker interface} *)

type symbol =
  | Id of Dolmen.Std.Id.t
  | Builtin of Dolmen.Std.Term.builtin (**)
(** Wrapper around potential function symbols from the Dolmen AST. *)


(** Typechecker interface *)
module type Formulas = sig

  (** {2 types} *)
  type ty
  type ty_var
  type ty_cst
  type ty_def

  type term
  type term_var
  type term_cst
  type term_cstr
  type term_field

  type 'a ast_tag

  (** {2 Type definitions} *)

  type order =
    | First_order   (** First-oreder typechecking *)
    | Higher_order  (** Higher-order typechecking *)
  (** Control whether the typechecker should type *)


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

  type sym_inference_source = {
    symbol : Dolmen.Std.Id.t;
    symbol_loc : Dolmen.Std.Loc.t;
    mutable inferred_ty : ty;
  }
  (** *)

  type var_inference_source = {
    variable : Dolmen.Std.Id.t;
    variable_loc : Dolmen.Std.Loc.t;
    mutable inferred_ty : ty;
  }
  (** *)

  type wildcard_source =
    | Arg_of of wildcard_source
    | Ret_of of wildcard_source
    | From_source of Dolmen.Std.Term.t
    | Added_type_argument of Dolmen.Std.Term.t
    | Symbol_inference of sym_inference_source
    | Variable_inference of var_inference_source (**)
  (** *)

  type wildcard_shape =
    | Forbidden
    | Any_in_scope
    | Any_base of {
        allowed : ty list;
        preferred : ty;
      }
    | Arrow of {
        arg_shape : wildcard_shape;
        ret_shape : wildcard_shape;
      } (**)
  (** *)

  type infer_unbound_var_scheme =
    | No_inference
    | Unification_type_variable (**)
  (** *)

  type infer_term_scheme =
    | No_inference
    | Wildcard of wildcard_shape (**)
  (** *)

  type var_infer = {
    infer_unbound_vars              : infer_unbound_var_scheme;
    infer_type_vars_in_binding_pos  : bool;
    infer_term_vars_in_binding_pos  : infer_term_scheme;
    var_hook : [ `Ty_var of ty_var | `Term_var of term_var ] -> unit;
  }
  (** Specification of how to infer variables. *)

  type sym_infer = {
    infer_type_csts   : bool;
    infer_term_csts   : infer_term_scheme;
    sym_hook : [ `Ty_cst of ty_cst | `Term_cst of term_cst ] -> unit;
  }
  (** Specification of how to infer symbols. *)

  type free_wildcards =
    | Forbidden
    | Implicitly_universally_quantified (**)
  (** *)

  type expect =
    | Type
    | Term
    | Anything (**)
  (** *)

  type tag =
    | Set : 'a ast_tag * 'a -> tag
    | Add : 'a list ast_tag * 'a -> tag
  (** Existencial wrapper around tags *)

  type res =
    | Ttype
    | Ty    of ty
    | Term  of term
    | Tags  of tag list (**)
  (** The results of parsing an untyped term.  *)

  type builtin_meta_ttype = unit
  type builtin_meta_ty = unit
  type builtin_meta_tags = unit
  (** Some type aliases *)

  type term_semantics = [
    | `Total
    | `Partial of (ty_var list -> term_var list -> ty -> term_cst)
  ]
  (** Semantics of term constants. Some term constants have only partially
      defined semantics (for instance division by zero), and these constants
      can have their semantics/interpretation extended/completed by later
      definitions. *)

  type builtin_meta_term = term_semantics
  (** Meta data for term builtins. *)

  type ('res, 'meta) builtin_common_res =
    'meta * (Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> 'res)
  (** Small record to hold the results of builtin parsing by theories. *)

  type builtin_res = [
    | `Ttype of (unit, builtin_meta_ttype) builtin_common_res
    | `Ty    of (ty, builtin_meta_ty) builtin_common_res
    | `Term  of (term, builtin_meta_term) builtin_common_res
    | `Tags  of (tag list, builtin_meta_tags) builtin_common_res
    | `Reserved of string * [
        | `Solver
        | `Term_cst of (ty_var list -> term_var list -> ty -> term_cst)
      ]
    | `Infer of string * var_infer * sym_infer
  ]
  (** The result of parsing a symbol by the theory *)

  type not_found = [ `Not_found ]
  (** Not bound bindings *)

  type reason =
    | Builtin
    | Reserved of string
    | Bound of Dolmen.Std.Loc.file * Dolmen.Std.Term.t
    | Inferred of Dolmen.Std.Loc.file * Dolmen.Std.Term.t
    | Defined of Dolmen.Std.Loc.file * Dolmen.Std.Statement.def
    | Declared of Dolmen.Std.Loc.file * Dolmen.Std.Statement.decl
    | Implicit_in_def of Dolmen.Std.Loc.file * Dolmen.Std.Statement.def
    | Implicit_in_decl of Dolmen.Std.Loc.file * Dolmen.Std.Statement.decl
    | Implicit_in_term of Dolmen.Std.Loc.file * Dolmen.Std.Term.t
  (** The type of reasons for constant typing *)

  type binding = [
    | `Not_found
    | `Reserved of string
    | `Builtin of [
        | `Ttype
        | `Ty
        | `Term
        | `Tag
      ]
    | `Variable of [
        | `Ty of ty_var * reason option
        | `Term of term_var * reason option
      ]
    | `Constant of [
        | `Ty of ty_cst * reason option
        | `Cstr of term_cstr * reason option
        | `Dstr of term_cst * reason option
        | `Term of term_cst * reason option
        | `Field of term_field * reason option
      ]
  ]
  (** The bindings that can occur. *)

  type var_kind = [
    | `Let_bound
    | `Quantified
    | `Function_param
    | `Type_alias_param
  ]
  (** The type of kinds of variables *)

  type nonrec symbol = symbol =
    | Id of Dolmen.Std.Id.t
    | Builtin of Dolmen.Std.Term.builtin (**)
  (** Wrapper around potential function symbols from the Dolmen AST. *)


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
    | Unused_type_variable : var_kind * ty_var -> Dolmen.Std.Term.t warn
    (** Unused quantified type variable *)
    | Unused_term_variable : var_kind * term_var -> Dolmen.Std.Term.t warn
    (** Unused quantified term variable *)
    | Error_in_attribute : exn -> Dolmen.Std.Term.t warn
    (** An error occurred wile parsing an attribute *)
    | Superfluous_destructor :
        Dolmen.Std.Id.t * Dolmen.Std.Id.t * term_cst -> Dolmen.Std.Term.t warn
    (** The user implementation of typed terms returned a destructor where
        was asked for. This warning can very safely be ignored. *)
    | Redundant_pattern : term -> Dolmen.Std.Term.t warn
    (** Redundant cases in pattern matching *)
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
    | Expected : string * res option -> Dolmen.Std.Term.t err
    (** The parsed term didn't match the expected shape *)
    | Bad_index_arity : string * int * int -> Dolmen.Std.Term.t err
    (** [Bad_index_arity (name, expected, actual)] denotes an error where
        an indexed family of operators (based on [name]) expect to be indexed
        by [expected] arguments but got [actual] instead. *)
    | Bad_ty_arity : ty_cst * int -> Dolmen.Std.Term.t err
    (** [Bad_ty_arity (cst, actual)] denotes a type constant that was applied
        to [actual] arguments, but which has a different arity (which should
        be accessible by getting its type/sort/arity). *)
    | Bad_op_arity : symbol * int list * int -> Dolmen.Std.Term.t err
    (** [Bad_op_arity (symbol, expected, actual)] denotes a named operator
        (which may be a builtin operator, a top-level defined constant which
        is being substituted, etc...) expecting a number of arguments among
        the [expected] list, but instead got [actual] number of arguments. *)
    | Bad_cstr_arity : term_cstr * int list * int -> Dolmen.Std.Term.t err
    (** [Bad_cstr_arity (cstr, expected, actual)] denotes an ADT constructor,
        which was expecting one of [expected] arguments, but which was applied
        to [actual] arguments. *)
    | Bad_term_arity : term_cst * int list * int -> Dolmen.Std.Term.t err
    (** [Bad_term_arity (func, expected, actual)] denotes a function symbol,
        which was expecting one of [expected] arguments, but which was applied
        to [actual] arguments. *)
    | Bad_poly_arity : ty_var list * ty list -> Dolmen.Std.Term.t err
    (** [Bad_poly_arity (ty_vars, ty_args) denotes a polymorphic term
        application, where the function term being applied was provided with
        the type arguments [ty_args], but the function type expected
        a number of arguments that is the length of [ty_vars], and the
        two lengths differ. Under application is allowed, so in the cases
        where there are less provided arguments than expected type arguments,
        the presence of term arguments after the type arguments forced
        the raising of this exception. *)
    | Over_application : term list -> Dolmen.Std.Term.t err
    (** [Over_application over_args] denotes an application where after applying
        the provided arguments, the application resulted in a term with a
        non-function type, but that term was still provided with [over_args]. *)
    | Repeated_record_field : term_field -> Dolmen.Std.Term.t err
    (** [Repeated_record_field f] denotes an error within an expression
        that builds a record by giving values to all fields, but where the
        field [f] appears more than once. *)
    | Missing_record_field : term_field -> Dolmen.Std.Term.t err
    (** [Missing_record_field f] denotes an error within an expression
        that builds a record by giving values to all fields, but where the
        field [f] does not appear. *)
    | Mismatch_record_type : term_field * ty_cst -> Dolmen.Std.Term.t err
    (** [Mismatch_record_type (f, r)] denotes an error where while building
        a record expression for a record of type [c], a field [f] belonging
        to another record type was used. *)
    | Mismatch_sum_type : term_cstr * ty -> Dolmen.Std.Term.t err
    (** *)
    | Partial_pattern_match : term list -> Dolmen.Std.Term.t err
    (** [Partial_pattern_match missing] denotes an error within a pattern
        matching in which the list of patterns do not cover all of the values
        of the type being matched. A list of non-matched terms is given
        to help users complete the pattern matching. *)
    | Var_application : term_var -> Dolmen.Std.Term.t err
    (** [Var_application v] denotes a variable which was applied to other
        terms, which is forbidden in first-order formulas. *)
    | Ty_var_application : ty_var -> Dolmen.Std.Term.t err
    (** [Ty_var_application v] denotes a type variable which was applied to
        other terms, which is forbidden in first-order formulas. *)
    | Type_mismatch : term * ty -> Dolmen.Std.Term.t err
    (** [Type_mismatch (term, expected)] denotes a context where [term] was
        expected to have type [expected], but it is not the case. *)
    | Var_in_binding_pos_underspecified  : Dolmen.Std.Term.t err
    (** Variable in a binding pos (e.g. quantifier) without a type,
        and no configured way to infer its type. *)
    | Unhandled_builtin : Dolmen.Std.Term.builtin -> Dolmen.Std.Term.t err
    (** *)
    | Cannot_tag_tag : Dolmen.Std.Term.t err
    (** *)
    | Cannot_tag_ttype : Dolmen.Std.Term.t err
    (** *)
    | Cannot_find : Dolmen.Std.Id.t * string -> Dolmen.Std.Term.t err
    (** *)
    | Forbidden_quantifier : Dolmen.Std.Term.t err
    (** *)
    | Missing_destructor : Dolmen.Std.Id.t -> Dolmen.Std.Term.t err
    (** *)
    | Type_def_rec : Dolmen.Std.Statement.def -> Dolmen.Std.Statement.defs err
    (** *)
    | Id_definition_conflict : Dolmen.Std.Id.t * binding -> Dolmen.Std.Loc.t err
    (** *)
    | Higher_order_application : Dolmen.Std.Term.t err
    (** *)
    | Higher_order_type : Dolmen.Std.Term.t err
    (** *)
    | Higher_order_env_in_tff_typechecker : Dolmen.Std.Loc.t err
    (** Programmer error *)
    | Polymorphic_function_argument : Dolmen.Std.Term.t err
    (** *)
    | Non_prenex_polymorphism : ty -> Dolmen.Std.Term.t err
    (** *)
    | Inference_forbidden :
        ty_var * wildcard_source * ty -> Dolmen.Std.Term.t err
    (** *)
    | Inference_conflict :
        ty_var * wildcard_source * ty * ty list -> Dolmen.Std.Term.t err
    (** *)
    | Inference_scope_escape :
        ty_var * wildcard_source * ty_var * reason option -> Dolmen.Std.Term.t err
    (** [Inference_scope_escape (w, w_src, v, reason)] denotes a situation where
        the wildcard variable [w] (which comes from [w_src]), was instantiated
        with a type that would lead to the variable [v] from escaping its scope;
        [reason] is the reason of the binding for [v]. *)
    | Unbound_type_wildcards :
        (ty_var * wildcard_source list) list -> Dolmen.Std.Term.t err
    (** *)
    | Incoherent_type_redefinition :
        Dolmen.Std.Id.t * ty_cst * reason * int -> Dolmen.Std.Statement.def err
    (** *)
    | Incoherent_term_redefinition :
        Dolmen.Std.Id.t * term_cst * reason * ty -> Dolmen.Std.Statement.def err
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
    ?st:state ->
    ?expect:expect ->
    ?var_infer:var_infer ->
    ?sym_infer:sym_infer ->
    ?order:order ->
    ?poly:poly ->
    ?quants:bool ->
    ?free_wildcards:free_wildcards ->
    warnings:(warning -> unit) ->
    file:Dolmen.Std.Loc.file ->
    builtin_symbols -> env
  (** Create a new environment. *)

  val state : env -> state
  (** Get the mutable state for an env. *)


  (** {2 Inference for vars and syms} *)

  val var_infer : env -> var_infer
  (** Getter for an env's var infer. *)

  val with_var_infer : env -> var_infer -> env
  (** Set the variable inference configuation *)

  val sym_infer : env -> sym_infer
  (** Getter for an env's sym infer. *)

  val with_sym_infer : env -> sym_infer -> env
  (** Set the symbol inference configuration *)


  (** {2 Errors & Warnings} *)

  val _warn : env -> 'a fragment -> 'a warn -> unit
  (** Emit a warning *)

  val _error : env -> 'a fragment -> 'a err -> _
  (** Raise an error *)

  val suggest : limit:int -> env -> Dolmen.Std.Id.t -> Dolmen.Std.Id.t list
  (** From a dolmen identifier, return a list of existing bound identifiers
      in the env that are up to [~limit] in terms of distance of edition. *)

  val _wrap : env -> Dolmen.Std.Term.t -> ('a -> 'b) -> 'a -> 'b
  val _wrap2 : env -> Dolmen.Std.Term.t -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
  val _wrap3 : env -> Dolmen.Std.Term.t -> ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
  (* Convenient wrapping function to catch exceptions and raise the appropriate
     typing error. *)


  (** {2 Location helpers} *)

  val loc : env -> Dolmen.Std.Loc.t -> Dolmen.Std.Loc.full
  (** Completes the location with the file name form the env. *)

  val fragment_loc : env -> _ fragment -> Dolmen.Std.Loc.full
  (** Convenient function to get the location of a fragment. *)

  val binding_reason : binding -> reason option
  (** Extract the reason from a binding
      @raise Invalid_argument if the binding is [`Not_found] *)


  (** {2 Name/Path helpers} *)

  val var_name : env -> Dolmen.Std.Name.t -> string
  (** Extract a variable name from a standard name. *)

  val cst_path : env -> Dolmen.Std.Name.t -> Dolmen.Std.Path.t
  (** Build a path from a standard name. *)


  (** {2 Bindings helpers} *)

  type var = [
    | `Ty_var of ty_var
    | `Term_var of term_var
    | `Letin of env * Dolmen.Std.Term.t * term_var * term
  ]
  (** Variable bindings *)

  type cst = [
    | `Cstr of term_cstr
    | `Dstr of term_cst
    | `Field of term_field
    | `Ty_cst of ty_cst
    | `Term_cst of term_cst
  ]
  (** Constant bindings *)

  type builtin = [
    | `Builtin of builtin_res
  ]
  (** Builtin binding *)

  type bound = [ var | cst | builtin ]
  (* All internal bindings *)

  val find_var : env -> Dolmen.Std.Id.t -> [ var | not_found ]
  (** Try and find the given id in the set of locally bound variables. *)

  val find_global : env -> Dolmen.Std.Id.t -> [ cst | not_found ]
  (** Try and find the given id in the set of globally bound constants. *)

  val find_global_st : state -> Dolmen.Std.Id.t -> [ cst | not_found ]
  (** Try and find the given id in the set of globally bound constants. *)

  val find_builtin : env -> Dolmen.Std.Id.t -> [ builtin | not_found ]
  (** Try and find the given id in the set of bound builtin symbols. *)

  val find_bound : env -> Dolmen.Std.Id.t -> [ bound | not_found ]
  (** Try and find a bound identifier in the env, whether it be locally bound
      (such as bound variables), constants bound at top-level, or builtin
      symbols bound by the builtin theory. *)

  val find_reason : env -> bound -> reason option
  (** Return the reason (if any) for the given typed symbol. *)

  val decl_ty_const :
    env -> _ fragment -> Dolmen.Std.Id.t -> ty_cst -> reason -> unit
  (** Declare a new type constant in the global environment used by the
      given environment *)

  val decl_term_const :
    env -> _ fragment -> Dolmen.Std.Id.t -> term_cst -> reason -> unit
  (** Declare a new term constant in the global environment used by the
      given environment *)



  (** {2 Custom global state} *)

  val get_global_custom : env -> 'a Dolmen.Std.Tag.t -> 'a option
  val get_global_custom_state : state -> 'a Dolmen.Std.Tag.t -> 'a option
  (** Get a custom value from the global environment or state. *)

  val set_global_custom : env -> 'a Dolmen.Std.Tag.t -> 'a -> unit
  val set_global_custom_state : state -> 'a Dolmen.Std.Tag.t -> 'a -> unit
  (** Set a custom value in the global environment or state. *)


  (** {2 Builtin helpers} *)

  val builtin_ttype :
    ?meta:builtin_meta_ttype ->
    (Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> unit) ->
    [> builtin_res]

  val builtin_ty :
    ?meta:builtin_meta_ty ->
    (Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> ty) ->
    [> builtin_res]

  val builtin_term :
    ?meta:builtin_meta_term ->
    (Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> term) ->
    [> builtin_res]

  val builtin_tags :
    ?meta:builtin_meta_tags ->
    (Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> tag list) ->
    [> builtin_res]


  (** {2 Parsing functions} *)

  val wildcard : env -> wildcard_source -> wildcard_shape -> ty
  (** Create a wildcard type. *)

  val monomorphize : env -> Dolmen.Std.Term.t -> term -> term
  (** Monomorphize a term. *)

  val parse_expr : res typer
  (** Main parsing function. *)

  val parse_ty : ty typer
  val parse_term : term typer
  val parse_prop : term typer
  (** Wrappers around {parse_expr} to set the expect field of the env,
      and unwrap an expected return value. *)

  val parse_app_ty_cst : (ty_cst -> Dolmen.Std.Term.t list -> res) typer
  val parse_app_term_cst : (term_cst -> Dolmen.Std.Term.t list -> res) typer
  (** Function used for parsing applications. The first dolmen term given
      is the application term being parsed (used for reporting errors). *)

  val parse_app_ho_term : (term -> Dolmen.Std.Term.t list -> res) typer
  (** Function used for parsing an higher-order application. *)

  val unwrap_ty : env -> Dolmen.Std.Term.t -> res -> ty
  val unwrap_term : env -> Dolmen.Std.Term.t -> res -> term
  (** Unwrap a result, raising the adequate typing error
      if the result if not as expected. *)


  (** {2 High-level functions} *)

  val decls :
    env -> ?attrs:Dolmen.Std.Term.t list ->
    Dolmen.Std.Statement.decls -> [
      | `Type_decl of ty_cst * ty_def option
      | `Term_decl of term_cst
    ] list
  (** Parse a list of potentially mutually recursive declarations. *)

  val defs :
    ?mode:[`Create_id | `Use_declared_id] ->
    env -> ?attrs:Dolmen.Std.Term.t list ->
    Dolmen.Std.Statement.defs -> [
      | `Type_alias of Dolmen.Std.Id.t * ty_cst * ty_var list * ty
      | `Term_def of Dolmen.Std.Id.t * term_cst * ty_var list * term_var list * term
      | `Instanceof of Dolmen.Std.Id.t * term_cst * ty list * ty_var list * term_var list * term
    ] list
  (** Parse a definition *)

  val parse : term typer
  (** Parse a formula *)

end
