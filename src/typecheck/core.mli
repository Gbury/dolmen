
(** {2 Languages Core builtins} *)

(** AE builtins *)
module Ae : sig

  (** Builtin symbols for tptp's tff *)
  module Tff
      (Type : Tff_intf.S)
      (Tag : Dolmen.Intf.Tag.Ae_Base with type 'a t = 'a Type.Tag.t
                                      and type term := Type.T.t)
      (Ty : Dolmen.Intf.Ty.Ae_Base with type t = Type.Ty.t)
      (T : Dolmen.Intf.Term.Ae_Base with type t = Type.T.t
                                     and type term_var := Type.T.Var.t) : sig

    val parse : Type.builtin_symbols

  end
end

(** Dimacs builtins *)
module Dimacs : sig

  (** Builtin symbols for tptp's tff *)
  module Tff
      (Type : Tff_intf.S)
      (T : Dolmen.Intf.Term.Dimacs with type t = Type.T.t) : sig

    val parse : Type.builtin_symbols

  end
end

(** TPTP builtins ($i, $o, etc..) *)
module Tptp : sig

  (** Builtin symbols for tptp's tff *)
  module Tff
      (Type : Tff_intf.S)
      (Ty : Dolmen.Intf.Ty.Tptp_Base with type t = Type.Ty.t)
      (T : Dolmen.Intf.Term.Tptp_Tff_Core with type t = Type.T.t) : sig

    val parse : Dolmen.Tptp.version -> Type.builtin_symbols

  end

  (** Builtin symbols for tptp's tff *)
  module Thf
      (Type : Thf_intf.S)
      (Ty : Dolmen.Intf.Ty.Tptp_Base with type t = Type.Ty.t)
      (T : Dolmen.Intf.Term.Tptp_Thf_Core with type t = Type.T.t
                                           and type Const.t = Type.T.Const.t) : sig

    val parse : Dolmen.Tptp.version -> Type.builtin_symbols

  end
end


(** Smtlib builtin *)
module Smtlib2 : sig

  exception Empty_sexpr of Dolmen.Std.Term.t
  exception Bad_index_in_sexpr of Dolmen.Std.Term.t
  exception Unexpected_structure_in_sexpr of Dolmen.Std.Term.t
  exception Uninterpreted_reserved_word_in_sexpr of Dolmen.Std.Id.t * Dolmen.Std.Term.t

  val sexpr_as_term : Dolmen.Std.Term.t -> Dolmen.Std.Term.t
  val sexpr_as_sort : Dolmen.Std.Term.t -> Dolmen.Std.Term.t

  (** Builtins for smtlib's core theory *)
  module Tff
      (Type : Tff_intf.S)
      (Tag : Dolmen.Intf.Tag.Smtlib_Base with type 'a t = 'a Type.Tag.t
                                          and type term := Type.T.t)
      (Ty : Dolmen.Intf.Ty.Smtlib_Base with type t = Type.Ty.t)
      (T : Dolmen.Intf.Term.Smtlib_Base with type t = Type.T.t
                                         and type cstr := Type.T.Cstr.t) : sig

    type _ Type.warn +=
      | Unknown_attribute : Dolmen.Std.Id.t -> Dolmen.Std.Term.t Type.warn

    type _ Type.err +=
      | Incorrect_sexpression : Dolmen.Intf.Msg.t -> Dolmen.Std.Term.t Type.err
      | Non_closed_named_term : Type.Ty.Var.t list * Type.T.Var.t list -> Dolmen.Std.Term.t Type.err

    val inferred_model_constants : Type.T.Const.t list Dolmen.Std.Tag.t

    val parse : Dolmen.Smtlib2.version -> Type.builtin_symbols
  end
end

(** Zf builtins *)
module Zf : sig

  (** Builtins for smtlib's core theory *)
  module Tff
      (Type : Tff_intf.S)
      (Tag : Dolmen.Intf.Tag.Zf_Base with type 'a t = 'a Type.Tag.t)
      (Ty : Dolmen.Intf.Ty.Zf_Base with type t = Type.Ty.t)
      (T : Dolmen.Intf.Term.Zf_Base with type t = Type.T.t) : sig

    val parse : Type.builtin_symbols
  end
end
