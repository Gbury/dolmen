
(** Definitions

    Helpful helpers to handle defined symbols.
*)


(** {2 Definitions as declarations} *)

(** Handle definitions as declaring new constants. *)
module Declare(Type : Tff_intf.S) : sig

  val add_definition :
    Type.env -> Dolmen.Std.Id.t ->
    [ `Ty of Type.Ty.Const.t | `Term of Type.T.Const.t ] -> unit
  (** Add a declaration binding. *)

  val define_ty :
    Type.env -> Dolmen.Std.Id.t ->
    Type.Ty.Var.t list -> Type.Ty.t -> Type.Ty.Const.t
  (** Define a type constant. *)

  val define_term :
    Type.env -> Dolmen.Std.Id.t ->
    Type.Ty.Var.t list -> Type.T.Var.t list -> Type.T.t -> Type.T.Const.t
  (** Define a term constant. *)

  val parse : Type.builtin_symbols
  (** Adequate builtin symbols function for constants defined/declared. *)

end

(** {2 Definitions using substitution} *)

(** Signature for substitution functions over types and terms *)
module type Subst_arg = sig

  type ty
  type ty_var

  type term
  type term_var

  val ty_subst :
    (ty_var * ty) list -> ty -> ty
  val term_subst :
    (ty_var * ty) list -> (term_var * term) list -> term -> term

end

(** Handle definitions by subsituting in the bodies during type-checking. *)
module Subst(Type : Tff_intf.S)
    (T : Subst_arg with type ty := Type.Ty.t
                    and type ty_var := Type.Ty.Var.t
                    and type term := Type.T.t
                    and type term_var := Type.T.Var.t) : sig

  val define_ty :
    Type.env -> Dolmen.Std.Id.t ->
    Type.Ty.Var.t list -> Type.Ty.t -> unit
  (** Define a type constant. *)

  val define_term :
    Type.env -> Dolmen.Std.Id.t ->
    Type.Ty.Var.t list -> Type.T.Var.t list -> Type.T.t -> unit
  (** Define a term constant. *)

  val parse : Type.builtin_symbols
  (** Adequate builtin symbols function for constants defined/declared. *)

end
