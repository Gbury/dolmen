
(** {2 Builtin functions manipulations} *)

val noop : _ -> _ -> [> `Not_found ]
(** Noop builtins function. *)

val merge :
  ('a -> 'b -> ([> `Not_found ] as 'c)) list ->
  'a -> 'b -> 'c
(** A convenient function for merging a list of
    builtin parser functions into a single builtin function. *)

(** {2 Smtlib Indexed id helpers} *)

type 'ret indexed = [
  | `Unary of (string -> 'ret)
  | `Binary of (string -> string -> 'ret)
  | `Ternary of (string -> string -> string -> 'ret)
  | `Nary of int * (string list -> 'ret)
]
(** The type of indexed family of operators. *)

val parse_id : Dolmen.Id.t ->
  (string * 'ret indexed) list ->
  err:(string -> int -> int -> 'ret) ->
  k:(string list -> 'ret) ->
  'ret
(** [parse_id id l ~err ~k] splits [id] (using {split_id})
    into a list. If the list has a head [s] and a tail [l], it tries and find
    in the list [l] a pair (s', indexed) such that [s = s'].
    If the length of [l] matches the arity of [indexed], the provided function
    is called, else [err] is called with [s], the arity of [indexed],
    and the lenght of [l].
    If no match is found or the split list does not contain a head and a
    tail, [k] is called wiht the split list. *)

val bad_ty_index_arity :
  (module Tff_intf.S with type env = 'env and type Ty.t = 'ty) ->
  'env -> string -> int -> int ->
  [> `Ty of (Dolmen.Term.t -> Dolmen.Term.t list -> 'ty) ]
(** Suitable [err] function for {parse_id} for typing sort indexed families. *)

val bad_term_index_arity :
  (module Tff_intf.S with type env = 'env and type T.t = 'term) ->
  'env -> string -> int -> int ->
  [> `Term of (Dolmen.Term.t -> Dolmen.Term.t list -> 'term) ]
(** Suitable [err] function for {parse_id} for typing term indexed families. *)



(** {2 Low-level helpers} *)

type ('env, 'args, 'ret) helper =
  (module Tff_intf.S with type env = 'env) ->
  'env -> string -> (Dolmen.Term.t -> 'args -> 'ret) ->
  (Dolmen.Term.t -> Dolmen.Term.t list -> 'ret)

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

val make_op4 :
  (_, Dolmen.Term.t * Dolmen.Term.t * Dolmen.Term.t * Dolmen.Term.t, _) helper
(** Same as {!make_op0} but the returning function
    takes a quadruple of terms as argument. *)

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


(** {2 High level helpers} *)

val app0 :
  (module Tff_intf.S with type env = 'env) ->
  ?check:(Dolmen.Term.t -> unit) ->
  'env -> string -> ('ret) ->
  (Dolmen.Term.t -> Dolmen.Term.t list -> 'ret)

val app0_ast :
  (module Tff_intf.S with type env = 'env) ->
  ?check:(Dolmen.Term.t -> unit) ->
  'env -> string -> (Dolmen.Term.t -> 'ret) ->
  (Dolmen.Term.t -> Dolmen.Term.t list -> 'ret)


val ty_app1 :
  (module Tff_intf.S with type env = 'env and type Ty.t = 'ty) ->
  ?check:(Dolmen.Term.t -> Dolmen.Term.t -> unit) ->
  'env -> string -> ('ty -> 'ty) ->
  (Dolmen.Term.t -> Dolmen.Term.t list -> 'ty)

val ty_app1_ast :
  (module Tff_intf.S with type env = 'env and type Ty.t = 'ty) ->
  ?check:(Dolmen.Term.t -> Dolmen.Term.t -> unit) ->
  'env -> string -> (Dolmen.Term.t -> 'ty -> 'ty) ->
  (Dolmen.Term.t -> Dolmen.Term.t list -> 'ty)

val term_app1 :
  (module Tff_intf.S with type env = 'env and type T.t = 'term) ->
  ?check:(Dolmen.Term.t -> Dolmen.Term.t -> unit) ->
  'env -> string -> ('term -> 'term) ->
  (Dolmen.Term.t -> Dolmen.Term.t list -> 'term)

val term_app1_ast :
  (module Tff_intf.S with type env = 'env and type T.t = 'term) ->
  ?check:(Dolmen.Term.t -> Dolmen.Term.t -> unit) ->
  'env -> string -> (Dolmen.Term.t -> 'term -> 'term) ->
  (Dolmen.Term.t -> Dolmen.Term.t list -> 'term)


val ty_app2 :
  (module Tff_intf.S with type env = 'env and type Ty.t = 'ty) ->
  ?check:(Dolmen.Term.t -> Dolmen.Term.t -> Dolmen.Term.t -> unit) ->
  'env -> string -> ('ty -> 'ty -> 'ty) ->
  (Dolmen.Term.t -> Dolmen.Term.t list -> 'ty)

val ty_app2_ast :
  (module Tff_intf.S with type env = 'env and type Ty.t = 'ty) ->
  ?check:(Dolmen.Term.t -> Dolmen.Term.t -> Dolmen.Term.t -> unit) ->
  'env -> string -> (Dolmen.Term.t -> 'ty -> 'ty -> 'ty) ->
  (Dolmen.Term.t -> Dolmen.Term.t list -> 'ty)

val term_app2 :
  (module Tff_intf.S with type env = 'env and type T.t = 'term) ->
  ?check:(Dolmen.Term.t -> Dolmen.Term.t -> Dolmen.Term.t -> unit) ->
  'env -> string -> ('term -> 'term -> 'term) ->
  (Dolmen.Term.t -> Dolmen.Term.t list -> 'term)

val term_app2_ast :
  (module Tff_intf.S with type env = 'env and type T.t = 'term) ->
  ?check:(Dolmen.Term.t -> Dolmen.Term.t -> Dolmen.Term.t -> unit) ->
  'env -> string -> (Dolmen.Term.t -> 'term -> 'term -> 'term) ->
  (Dolmen.Term.t -> Dolmen.Term.t list -> 'term)


val ty_app3 :
  (module Tff_intf.S with type env = 'env and type Ty.t = 'ty) ->
  ?check:(Dolmen.Term.t -> Dolmen.Term.t ->
          Dolmen.Term.t -> Dolmen.Term.t -> unit) ->
  'env -> string -> ('ty -> 'ty -> 'ty -> 'ty) ->
  (Dolmen.Term.t -> Dolmen.Term.t list -> 'ty)

val ty_app3_ast :
  (module Tff_intf.S with type env = 'env and type Ty.t = 'ty) ->
  ?check:(Dolmen.Term.t -> Dolmen.Term.t ->
          Dolmen.Term.t -> Dolmen.Term.t -> unit) ->
  'env -> string -> (Dolmen.Term.t -> 'ty -> 'ty -> 'ty -> 'ty) ->
  (Dolmen.Term.t -> Dolmen.Term.t list -> 'ty)

val term_app3 :
  (module Tff_intf.S with type env = 'env and type T.t = 'term) ->
  ?check:(Dolmen.Term.t -> Dolmen.Term.t ->
          Dolmen.Term.t -> Dolmen.Term.t -> unit) ->
  'env -> string -> ('term -> 'term -> 'term -> 'term) ->
  (Dolmen.Term.t -> Dolmen.Term.t list -> 'term)

val term_app3_ast :
  (module Tff_intf.S with type env = 'env and type T.t = 'term) ->
  ?check:(Dolmen.Term.t -> Dolmen.Term.t ->
          Dolmen.Term.t -> Dolmen.Term.t -> unit) ->
  'env -> string -> (Dolmen.Term.t -> 'term -> 'term -> 'term -> 'term) ->
  (Dolmen.Term.t -> Dolmen.Term.t list -> 'term)


val ty_app4 :
  (module Tff_intf.S with type env = 'env and type Ty.t = 'ty) ->
  ?check:(Dolmen.Term.t -> Dolmen.Term.t -> Dolmen.Term.t ->
          Dolmen.Term.t -> Dolmen.Term.t -> unit) ->
  'env -> string -> ('ty -> 'ty -> 'ty -> 'ty -> 'ty) ->
  (Dolmen.Term.t -> Dolmen.Term.t list -> 'ty)

val ty_app4_ast :
  (module Tff_intf.S with type env = 'env and type Ty.t = 'ty) ->
  ?check:(Dolmen.Term.t -> Dolmen.Term.t -> Dolmen.Term.t ->
          Dolmen.Term.t -> Dolmen.Term.t -> unit) ->
  'env -> string -> (Dolmen.Term.t -> 'ty -> 'ty -> 'ty -> 'ty -> 'ty) ->
  (Dolmen.Term.t -> Dolmen.Term.t list -> 'ty)

val term_app4 :
  (module Tff_intf.S with type env = 'env and type T.t = 'term) ->
  ?check:(Dolmen.Term.t -> Dolmen.Term.t -> Dolmen.Term.t ->
          Dolmen.Term.t -> Dolmen.Term.t -> unit) ->
  'env -> string -> ('term -> 'term -> 'term -> 'term -> 'term) ->
  (Dolmen.Term.t -> Dolmen.Term.t list -> 'term)

val term_app4_ast :
  (module Tff_intf.S with type env = 'env and type T.t = 'term) ->
  ?check:(Dolmen.Term.t -> Dolmen.Term.t -> Dolmen.Term.t ->
          Dolmen.Term.t -> Dolmen.Term.t -> unit) ->
  'env -> string -> (Dolmen.Term.t -> 'term -> 'term -> 'term -> 'term -> 'term) ->
  (Dolmen.Term.t -> Dolmen.Term.t list -> 'term)


val term_app_left :
  (module Tff_intf.S with type env = 'env and type T.t = 'term) ->
  ?check:(Dolmen.Term.t -> Dolmen.Term.t list -> unit) ->
  'env -> string -> ('term -> 'term -> 'term) ->
  (Dolmen.Term.t -> Dolmen.Term.t list -> 'term)

val term_app_left_ast :
  (module Tff_intf.S with type env = 'env and type T.t = 'term) ->
  ?check:(Dolmen.Term.t -> Dolmen.Term.t list -> unit) ->
  'env -> string -> (Dolmen.Term.t -> 'term -> 'term -> 'term) ->
  (Dolmen.Term.t -> Dolmen.Term.t list -> 'term)

val term_app_right :
  (module Tff_intf.S with type env = 'env and type T.t = 'term) ->
  ?check:(Dolmen.Term.t -> Dolmen.Term.t list -> unit) ->
  'env -> string -> ('term -> 'term -> 'term) ->
  (Dolmen.Term.t -> Dolmen.Term.t list -> 'term)

val term_app_right_ast :
  (module Tff_intf.S with type env = 'env and type T.t = 'term) ->
  ?check:(Dolmen.Term.t -> Dolmen.Term.t list -> unit) ->
  'env -> string -> (Dolmen.Term.t -> 'term -> 'term -> 'term) ->
  (Dolmen.Term.t -> Dolmen.Term.t list -> 'term)


val term_app_chain :
  (module Tff_intf.S with type env = 'env and type T.t = 'term) ->
  ?check:(Dolmen.Term.t -> Dolmen.Term.t list -> unit) ->
  'env -> string -> ('term -> 'term -> 'term) ->
  (Dolmen.Term.t -> Dolmen.Term.t list -> 'term)

val term_app_chain_ast :
  (module Tff_intf.S with type env = 'env and type T.t = 'term) ->
  ?check:(Dolmen.Term.t -> Dolmen.Term.t list -> unit) ->
  'env -> string -> (Dolmen.Term.t -> 'term -> 'term -> 'term) ->
  (Dolmen.Term.t -> Dolmen.Term.t list -> 'term)


