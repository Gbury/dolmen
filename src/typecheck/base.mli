
(** {2 Helpers} *)

val merge :
  ('a -> 'b -> 'c -> 'd -> 'e option) list ->
  'a -> 'b -> 'c -> 'd -> 'e option
(** A convenient function for merging a list of
    builtin parser functions into a single builtin function. *)


type ('env, 'args, 'ret) helper =
  (module Tff_intf.S with type env = 'env) ->
  'env -> Dolmen.Term.t -> string -> Dolmen.Term.t list ->
  ('args -> 'ret) -> 'ret option

val make_op0: (_, unit, _) helper
(** [make_op (module Type) env ast op arity args ret] checks
    that args is the empty list and returns [Some (ret ())],
    else it raises the appropriate exception from the
    typechecking module. *)

val make_op1: (_, Dolmen.Term.t, _) helper
(** Same as {!make_op0} but the returning function
    takes a term as argument. *)

val make_op2 : (_, Dolmen.Term.t * Dolmen.Term.t, _) helper
(** Same as {!make_op0} but the returning function
    takes a couple of terms as argument. *)

val make_op3 :
  (_, Dolmen.Term.t * Dolmen.Term.t * Dolmen.Term.t, _) helper
(** Same as {!make_op0} but the returning function
    takes a triple of terms as argument. *)

val make_opn :
  int -> (_, Dolmen.Term.t list, _) helper
(** Same as {!make_op0} but takes an arity first,
    and the returning function takes a list of terms as argument.
    The list is guaranteed to have the same length as the given arity. *)

val make_assoc : (_, Dolmen.Term.t list, _) helper
(** Ensures the list of arguments is at least of size 2 (used for associative
    symbols). *)

val fold_left_assoc : ('a -> 'a -> 'a) -> 'a list -> 'a
(** Fold application of a left-associative function on a list.
    @raise Invalid_argument if the list is empty. *)

val fold_right_assoc : ('a -> 'a -> 'a) -> 'a list -> 'a
(** Fold application of a right-associative function on a list.
    @raise Invalid_argument if the list is empty. *)

val make_chain : (_, Dolmen.Term.t list, _) helper
(** Ensures the list of arguments is at least of size 2 (used for chainable
    symbols). *)

val map_chain :
  (module Tff_intf.S with type T.t = 't) ->
  ('t -> 't -> 't) -> 't list -> 't
(** Map a function on succesive pairs of elements in the arguments lists,
    and build the resulting conjunction.
    [map_chain (module Type) mk \[t1; t2; ..; tn\]]
    is
    [Type.T._and \[mk t1 t2; mk t2 t3; ..\]] *)

(** {2 Languages base builtins} *)

(** TPTP builtins ($i, $o, etc..) *)
module Tptp : sig

  (** Type constants required to typecheck tptp builtins *)
  module type Ty = sig
    type t
    (** The type of types *)
    val prop : t
    (** The type constructor of propositions. *)
    module Const : sig
      type t
      (** The type of type constants (i.e. type constructors) *)
      val base : t
      (** An arbitrary already existing type constructor *)
    end
  end

  (** Term constants, aka function symbols. *)
  module type T = sig
    module Const : sig
      type t
      (** The type of function symbols *)
      val _true : t
      (** The smybol for [true] *)
      val _false : t
      (** The symbol for [false] *)
    end
  end

  (** Builtin symbols for tptp's tff *)
  module Tff
      (Type : Tff_intf.S)
      (Ty : Ty with type t = Type.Ty.t
                and type Const.t = Type.Ty.Const.t)
      (T : T with type Const.t = Type.T.Const.t) : sig

    val parse : Type.builtin_symbols

  end

end


(** Smtlib builtin *)
module Smtlib : sig

  (** Tags *)
  module type Tag = sig
    type 'a t
    (** Polymorphic tags *)
    val rwrt : unit t
    (** A flag (i.e. unit tag), indicatgin that the tagged term/formula
        is to be considered as a rewrite rule. *)
  end

  (** Type constants required to typecheck tptp builtins *)
  module type Ty = sig
    type t
    (** The type of type constants (i.e. type constructors) *)
    val prop : t
    (** The type constructor of propositions. *)
  end

  (** Terms *)
  module type T = sig
    type t
    (** The type fo terms. *)
    val eqs : t list -> t
    (** Create a chain of equalities. *)
  end

  (** Builtins for smtlib's core theory *)
  module Tff
      (Type : Tff_intf.S)
      (Tag : Tag with type 'a t = 'a Type.Tag.t)
      (Ty : Ty with type t = Type.Ty.t)
      (T : T with type t = Type.T.t) : sig

    val parse : Type.builtin_symbols
  end
end

(** Zf builtins *)
module Zf : sig

  (** Tags *)
  module type Tag = sig
    type 'a t
    (** Polymorphic tags *)
    val rwrt : unit t
    (** A flag (i.e. unit tag), indicatgin that the tagged term/formula
        is to be considered as a rewrite rule. *)

    type name
    (** A type used to specify a name for printing identifiers *)
    val name : name t
    (** A tag used to specify what to print when printing an identifier *)
    val exact : string -> name
    (** Print the identifier with this exact string. *)

    type pos
    (** A type to indicate how to print identifiers *)
    val pos : pos t
    (** A tag to specify how to print identifiers*)
    val infix : pos
    (** The tagged identifier is an infix symbol *)
    val prefix : pos
    (** The tagged identifier is a prefix symbol *)
  end

  (** Builtins for smtlib's core theory *)
  module Tff
      (Type : Tff_intf.S)
      (Tag : Tag with type 'a t = 'a Type.Tag.t) : sig

    val parse : Type.builtin_symbols
  end
end
