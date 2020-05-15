
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** External Typechecker interface

    This module defines the external typechcker interface, that is,
    the interface of an instantiated typechecker. *)

(** {1 Typechecker interface} *)

(** Typechecker interface *)
module type S = sig

  (** {2 Module aliases} *)
  module Tag: Dolmen_intf.Tag.S
  module Ty: Dolmen_intf.Type.Tff
    with type 'a tag := 'a Tag.t
  module T: Dolmen_intf.Term.Tff
    with type ty := Ty.t
     and type ty_var := Ty.Var.t
     and type ty_const := Ty.Const.t
     and type 'a tag := 'a Tag.t

  (** {2 Type definitions} *)

  type expect =
    | Nothing
    | Type
    | Typed of Ty.t
    (** The type of expected result when typing an expression, used to infer
        non-declared symbols. *)

  type tag = Any : 'a Tag.t * 'a -> tag
  (** Existencial wrapper around tags *)

  type res =
    | Ttype   : res
    | Ty      : Ty.t -> res
    | Term    : T.t -> res
    | Tags    : tag list -> res (**)
  (** The results of parsing an untyped term.  *)

  type inferred =
    | Ty_fun of Ty.Const.t
    | Term_fun of T.Const.t (**)
  (** The type for inferred symbols. *)

  type reason =
    | Builtin
    | Bound of Dolmen.Term.t
    | Inferred of Dolmen.Term.t
    | Declared of Dolmen.Statement.decl
  (** The type of reasons for constant typing *)

  type binding = [
    | `Not_found
    | `Builtin
    | `Variable of [
        | `Ty of Ty.Var.t * reason
        | `Term of T.Var.t * reason
      ]
    | `Constant of [
        | `Ty of Ty.Const.t * reason
        | `Cstr of T.Cstr.t * reason
        | `Term of T.Const.t * reason
        | `Field of T.Field.t * reason
      ]
  ]
  (** The bindings that can occur. *)


  (** {2 Errors and warnings} *)

  type _ fragment =
    | Ast : Dolmen.Term.t -> Dolmen.Term.t fragment
    | Decl : Dolmen.Statement.decl -> Dolmen.Statement.decl fragment
    | Decls : Dolmen.Statement.decl list -> Dolmen.Statement.decl list fragment
    | Located : Dolmen.ParseLocation.t -> Dolmen.ParseLocation.t fragment (**)
  (** Fragments of input that represent the sources of warnings/errors *)

  type _ warn = ..
  (** The type of warnings, parameterized by the type of fragment they can
      trigger on *)

  type _ warn +=
    | Unused_type_variable : Ty.Var.t -> Dolmen.Term.t warn
    (** Unused quantified type variable *)
    | Unused_term_variable : T.Var.t -> Dolmen.Term.t warn
    (** Unused quantified term variable *)
    | Error_in_attribute : exn -> Dolmen.Term.t warn
    (** An error occurred wile parsing an attribute *)
    | Superfluous_destructor :
        Dolmen.Id.t * Dolmen.Id.t * T.Const.t -> Dolmen.Term.t warn
    (** The user implementation of typed terms returned a destructor where
        was asked for. This warning can very safely be ignored. *)
  (** Warnings that cna trigger on regular parsed terms. *)

  type _ warn +=
    | Shadowing : Dolmen.Id.t * binding * binding -> _ warn
    (** Shadowing of the given identifier,
        together with the old and current binding. *)
  (** Special case of warnings for shadowing, as it can happen both from a
      term but also a declaration, hence why the type variable of [warn] is
      left wild. *)

  type _ err = ..
  (** The type of errors, parameterized by the type of fragment they can
      trigger on *)

  type _ err +=
    | Not_well_founded_datatypes : Dolmen.Statement.decl list err
    (** Not well-dounded datatypes definitions. *)
  (** Errors that occur on declaration(s) *)

  type _ err +=
    | Infer_type_variable : Dolmen.Term.t err
    (** The type of a bound variable had to be inferred which is forbidden. *)
    | Expected : string * res option -> Dolmen.Term.t err
    (** The parsed term didn't match the expected shape *)
    | Bad_index_arity : string * int * int -> Dolmen.Term.t err
    (** [Bad_index_arity (name, expected, actual)] denotes an error where
        an indexed family of operators (based on [name]) expect to be indexed
        by [expected] arguments but got [actual] instead. *)
    | Bad_op_arity : string * int list * int -> Dolmen.Term.t err
    (** [Bad_op_arity (name, expected, actual)] denotes a named operator
        (which may be a builtin operator, a top-level defined constant which
        is being subtituted, etc...) expecting a number of arguments among
        the [expected] list, but instead got [actual] number of arguments. *)
    | Bad_ty_arity : Ty.Const.t * int -> Dolmen.Term.t err
    (** [Bad_ty_arity (cst, actual)] denotes a type constant that was applied
        to [actual] arguments, but which has a different arity (which should
        be accessible by getting its type/sort/arity). *)
    | Bad_cstr_arity : T.Cstr.t * int -> Dolmen.Term.t err
    (** [Bad_cstr_arity (cstr, actual)] denotes an ADT constructor applied
        to [actual] arguments, but whose arity does not match that. *)
    | Bad_term_arity : T.Const.t * int * int -> Dolmen.Term.t err
    (** *)
    | Repeated_record_field : T.Field.t -> Dolmen.Term.t err
    (** *)
    | Missing_record_field : T.Field.t -> Dolmen.Term.t err
    (** *)
    | Mismatch_record_type : T.Field.t * Ty.Const.t -> Dolmen.Term.t err
    (** *)
    | Var_application : T.Var.t -> Dolmen.Term.t err
    (** *)
    | Ty_var_application : Ty.Var.t -> Dolmen.Term.t err
    (** *)
    | Type_mismatch : T.t * Ty.t -> Dolmen.Term.t err
    (** *)
    | Quantified_var_inference : Dolmen.Term.t err
    (** Quantified variable without a type *)
    | Unhandled_builtin : Dolmen.Term.builtin -> Dolmen.Term.t err
    (** *)
    | Cannot_tag_tag : Dolmen.Term.t err
    (** *)
    | Cannot_tag_ttype : Dolmen.Term.t err
    (** *)
    | Cannot_find : Dolmen.Id.t -> Dolmen.Term.t err
    (** *)
    | Type_var_in_type_constructor : Dolmen.Term.t err
    (** *)
    | Missing_destructor : Dolmen.Id.t -> Dolmen.Term.t err
    (** *)
    | Higher_order_application : Dolmen.Term.t err
    (** *)
    | Higher_order_type : Dolmen.Term.t err
    (** *)
    | Unbound_variables : Ty.Var.t list * T.Var.t list * T.t -> Dolmen.Term.t err
    (** *)
    | Uncaught_exn : exn -> Dolmen.Term.t err
    (** *)
    | Unhandled_ast : Dolmen.Term.t err
    (** *)
  (** Errors that occur on regular parsed terms. *)


  (** {2 State & Environment} *)

  type state
  (** The type of mutable state for typechecking. *)

  type env
  (** The type of environments for typechecking. *)

  type 'a typer = env -> Dolmen.Term.t -> 'a
  (** A general type for typers. Takes a local environment and the current untyped term,
      and return a value. The typer may need additional information for parsing,
      in which case the return value will be a function.
      @raise Typing_error *)

  type symbol =
    | Id of Dolmen.Id.t
    | Builtin of Dolmen.Term.builtin
    (** Wrapper around potential function symbols from the Dolmen AST. *)

  type builtin_symbols =
    env -> symbol -> (Dolmen.Term.t -> Dolmen.Term.t list -> res) option
  (** The type of a typer for builtin symbols. Given the environment and a symbol,
      the theory should return a typing function if the symbol belongs to the
      theory. This typing function takes first the ast term of the whole
      application that is beign typechecked, and the list of arguments to the
      symbol. *)

  type warning =
    | Warning : env * 'a fragment * 'a warn -> warning (**)
  (** Exitencial wrapper around warnings *)

  type error =
    | Error : env * 'a fragment * 'a err -> error (**)
  (** Exitencial wrapper around errors *)

  exception Typing_error of error
  (** Exception for typing errors *)

  val new_state : unit -> state
  (** Create a new state. *)

  val empty_env :
    ?st:state -> ?expect:expect ->
    ?infer_hook:(env -> inferred -> unit) ->
    ?infer_base:Ty.t -> warnings:(warning -> unit) ->
    builtin_symbols -> env
  (** Create a new environment. *)

  val expect : ?force:bool -> env -> expect -> env
  (** Returns the same environment but with the given expectation,
      except if the environnement already except [Nothing]. *)


  (** {2 Error helpers} *)

  val fragment_loc : _ fragment -> Dolmen.ParseLocation.t option
  (** Convenient function to get the location of a fragment. *)

  val binding_reason : binding -> reason
  (** Extract the reason from a binding
      @raise Invalid_argument if the binding is [`Not_found] *)

  val _warn : env -> 'a fragment -> 'a warn -> unit
  (** Emit a warning *)

  val _error : env -> 'a fragment -> 'a err -> _
  (** Raise an error *)

  val suggest : limit:int -> env -> Dolmen.Id.t -> Dolmen.Id.t list
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

  val parse_app_ty : (Ty.Const.t -> Dolmen.Term.t list -> res) typer
  val parse_app_term : (T.Const.t -> Dolmen.Term.t list -> res) typer
  (** Function used for parsing applications. The first dolmen term given
      is the application term being parsed (used for reporting errors). *)

  (** {2 High-level functions} *)

  val decls :
    env -> ?attr:Dolmen.Term.t ->
    Dolmen.Statement.decl list ->
    [ `Type_decl of Ty.Const.t
    | `Term_decl of T.Const.t
    ] list
  (** Parse a list of potentially mutually recursive declarations. *)

  val new_def :
    (?attr:Dolmen.Term.t -> Dolmen.Id.t ->
     [ `Type_def of Dolmen.Id.t * tag list * Ty.Var.t list * Ty.t
     | `Term_def of Dolmen.Id.t * tag list * Ty.Var.t list * T.Var.t list * T.t
     ]) typer
  (** Parse a definition *)

  val parse : T.t typer
  (** Parse a formula *)

end

