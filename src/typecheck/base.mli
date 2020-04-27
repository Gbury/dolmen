
(** {2 Helpers} *)

val noop : 'a -> 'b -> 'c -> 'd -> 'e option
(** Noop builtins function. *)

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


(** {2 Smtlib logic detection} *)

type smtlib_theory = [
  | `Core
  | `Arrays
  | `Bitvectors
  | `Ints
  | `Reals
  | `Reals_Ints
]
(** Smtlib theories. *)

type smtlib_features = {
  uninterpreted   : bool;
  datatypes       : bool;
  quantifiers     : bool;
  arithmetic      : [ `Linear | `Difference | `Regular ];
}
(** Smtlib features. *)

type smtlib_logic = {
  theories      : smtlib_theory list;
  features      : smtlib_features;
}
(** Structured representation of an smtlib logic. *)

val smtlib_logic : string -> smtlib_logic option
(** Parses an smtlib logic string and returns its structured version. *)


(** {2 Languages base builtins} *)

(** AE builtins *)
module Ae : sig

  (** Builtin symbols for tptp's tff *)
  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Ae_Base with type t = Type.Ty.t)
      (T : Dolmen.Intf.Term.Ae_Base with type t = Type.T.t) : sig

    val parse : Type.builtin_symbols

  end
end

(** TPTP builtins ($i, $o, etc..) *)
module Tptp : sig

  (** Builtin symbols for tptp's tff *)
  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Tptp_Base with type t = Type.Ty.t)
      (T : Dolmen.Intf.Term.Tptp_Base with type t = Type.T.t) : sig

    val parse : Dolmen_tptp.version -> Type.builtin_symbols

  end
end


(** Smtlib builtin *)
module Smtlib2 : sig

  (** Builtins for smtlib's core theory *)
  module Tff
      (Type : Tff_intf.S)
      (Tag : Dolmen.Intf.Tag.Smtlib_Base with type 'a t = 'a Type.Tag.t)
      (Ty : Dolmen.Intf.Ty.Smtlib_Base with type t = Type.Ty.t)
      (T : Dolmen.Intf.Term.Smtlib_Base with type t = Type.T.t) : sig

    val parse : Dolmen_smtlib2.version -> Type.builtin_symbols
  end
end

(** Zf builtins *)
module Zf : sig

  (** Builtins for smtlib's core theory *)
  module Tff
      (Type : Tff_intf.S)
      (Tag : Dolmen.Intf.Tag.Zf_Base with type 'a t = 'a Type.Tag.t) : sig

    val parse : Type.builtin_symbols
  end
end
