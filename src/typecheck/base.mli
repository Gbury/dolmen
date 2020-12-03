
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

val parse_id :
  Dolmen.Std.Id.t ->
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

val bad_ty_index_arity : Type.env -> string -> int -> int -> Type.builtin_res
(** Suitable [err] function for {parse_id} for typing sort indexed families. *)

val bad_term_index_arity : Type.env -> string -> int -> int -> Type.builtin_res
(** Suitable [err] function for {parse_id} for typing term indexed families. *)



(** {2 Low-level helpers} *)

type ('args, 'ret) helper =
  Type.env -> string -> (Dolmen.Std.Term.t -> 'args -> 'ret) ->
  (Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> 'ret)

val make_op0: (unit, _) helper
(** [make_op (module Type) env ast op arity args ret] checks
    that args is the empty list and returns [Some (ret ())],
    else it raises the appropriate exception from the
    typechecking module. *)

val make_op1: (Dolmen.Std.Term.t, _) helper
(** Same as {!make_op0} but the returning function
    takes a term as argument. *)

val make_op2 : (Dolmen.Std.Term.t * Dolmen.Std.Term.t, _) helper
(** Same as {!make_op0} but the returning function
    takes a couple of terms as argument. *)

val make_op3 :
  (Dolmen.Std.Term.t * Dolmen.Std.Term.t * Dolmen.Std.Term.t, _) helper
(** Same as {!make_op0} but the returning function
    takes a triple of terms as argument. *)

val make_op4 :
  (Dolmen.Std.Term.t * Dolmen.Std.Term.t * Dolmen.Std.Term.t * Dolmen.Std.Term.t, _) helper
(** Same as {!make_op0} but the returning function
    takes a quadruple of terms as argument. *)

val make_opn :
  int -> (Dolmen.Std.Term.t list, _) helper
(** Same as {!make_op0} but takes an arity first,
    and the returning function takes a list of terms as argument.
    The list is guaranteed to have the same length as the given arity. *)

val make_assoc : (Dolmen.Std.Term.t list, _) helper
(** Ensures the list of arguments is at least of size 2 (used for associative
    symbols). *)

val fold_left_assoc : ('a -> 'a -> 'a) -> 'a list -> 'a
(** Fold application of a left-associative function on a list.
    @raise Invalid_argument if the list is empty. *)

val fold_right_assoc : ('a -> 'a -> 'a) -> 'a list -> 'a
(** Fold application of a right-associative function on a list.
    @raise Invalid_argument if the list is empty. *)

val make_chain : (Dolmen.Std.Term.t list, _) helper
(** Ensures the list of arguments is at least of size 2 (used for chainable
    symbols). *)

val map_chain :
  (Dolmen.Std.Expr.Term.t -> Dolmen.Std.Expr.Term.t -> Dolmen.Std.Expr.Term.t)
  -> Dolmen.Std.Expr.Term.t list -> Dolmen.Std.Expr.Term.t
(** Map a function on succesive pairs of elements in the arguments lists,
    and build the resulting conjunction.
    [map_chain (module Type) mk \[t1; t2; ..; tn\]]
    is
    [Type.T._and \[mk t1 t2; mk t2 t3; ..\]] *)


(** {2 High level helpers} *)

val app0 :
  ?check:(Dolmen.Std.Term.t -> unit) ->
  Type.env -> string -> ('ret) ->
  (Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> 'ret)

val app0_ast :
  ?check:(Dolmen.Std.Term.t -> unit) ->
  Type.env -> string -> (Dolmen.Std.Term.t -> 'ret) ->
  (Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> 'ret)


val ty_app1 :
  ?check:(Dolmen.Std.Term.t -> Dolmen.Std.Term.t -> unit) ->
  Type.env -> string -> (Dolmen.Std.Expr.Ty.t -> Dolmen.Std.Expr.Ty.t) ->
  (Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> Dolmen.Std.Expr.Ty.t)

val ty_app1_ast :
  ?check:(Dolmen.Std.Term.t -> Dolmen.Std.Term.t -> unit) ->
  Type.env -> string -> (Dolmen.Std.Term.t -> Dolmen.Std.Expr.Ty.t -> Dolmen.Std.Expr.Ty.t) ->
  (Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> Dolmen.Std.Expr.Ty.t)

val term_app1 :
  ?check:(Dolmen.Std.Term.t -> Dolmen.Std.Term.t -> unit) ->
  Type.env -> string -> (Dolmen.Std.Expr.Term.t -> Dolmen.Std.Expr.Term.t) ->
  (Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> Dolmen.Std.Expr.Term.t)

val term_app1_ast :
  ?check:(Dolmen.Std.Term.t -> Dolmen.Std.Term.t -> unit) ->
  Type.env -> string -> (Dolmen.Std.Term.t -> Dolmen.Std.Expr.Term.t -> Dolmen.Std.Expr.Term.t) ->
  (Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> Dolmen.Std.Expr.Term.t)


val ty_app2 :
  ?check:(Dolmen.Std.Term.t -> Dolmen.Std.Term.t -> Dolmen.Std.Term.t -> unit) ->
  Type.env -> string -> (Dolmen.Std.Expr.Ty.t -> Dolmen.Std.Expr.Ty.t -> Dolmen.Std.Expr.Ty.t) ->
  (Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> Dolmen.Std.Expr.Ty.t)

val ty_app2_ast :
  ?check:(Dolmen.Std.Term.t -> Dolmen.Std.Term.t -> Dolmen.Std.Term.t -> unit) ->
  Type.env -> string -> (Dolmen.Std.Term.t -> Dolmen.Std.Expr.Ty.t -> Dolmen.Std.Expr.Ty.t -> Dolmen.Std.Expr.Ty.t) ->
  (Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> Dolmen.Std.Expr.Ty.t)

val term_app2 :
  ?check:(Dolmen.Std.Term.t -> Dolmen.Std.Term.t -> Dolmen.Std.Term.t -> unit) ->
  Type.env -> string -> (Dolmen.Std.Expr.Term.t -> Dolmen.Std.Expr.Term.t -> Dolmen.Std.Expr.Term.t) ->
  (Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> Dolmen.Std.Expr.Term.t)

val term_app2_ast :
  ?check:(Dolmen.Std.Term.t -> Dolmen.Std.Term.t -> Dolmen.Std.Term.t -> unit) ->
  Type.env -> string -> (Dolmen.Std.Term.t -> Dolmen.Std.Expr.Term.t -> Dolmen.Std.Expr.Term.t -> Dolmen.Std.Expr.Term.t) ->
  (Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> Dolmen.Std.Expr.Term.t)


val ty_app3 :
  ?check:(Dolmen.Std.Term.t -> Dolmen.Std.Term.t ->
          Dolmen.Std.Term.t -> Dolmen.Std.Term.t -> unit) ->
  Type.env -> string -> (Dolmen.Std.Expr.Ty.t -> Dolmen.Std.Expr.Ty.t -> Dolmen.Std.Expr.Ty.t -> Dolmen.Std.Expr.Ty.t) ->
  (Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> Dolmen.Std.Expr.Ty.t)

val ty_app3_ast :
  ?check:(Dolmen.Std.Term.t -> Dolmen.Std.Term.t ->
          Dolmen.Std.Term.t -> Dolmen.Std.Term.t -> unit) ->
  Type.env -> string -> (Dolmen.Std.Term.t -> Dolmen.Std.Expr.Ty.t -> Dolmen.Std.Expr.Ty.t -> Dolmen.Std.Expr.Ty.t -> Dolmen.Std.Expr.Ty.t) ->
  (Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> Dolmen.Std.Expr.Ty.t)

val term_app3 :
  ?check:(Dolmen.Std.Term.t -> Dolmen.Std.Term.t ->
          Dolmen.Std.Term.t -> Dolmen.Std.Term.t -> unit) ->
  Type.env -> string -> (Dolmen.Std.Expr.Term.t -> Dolmen.Std.Expr.Term.t -> Dolmen.Std.Expr.Term.t -> Dolmen.Std.Expr.Term.t) ->
  (Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> Dolmen.Std.Expr.Term.t)

val term_app3_ast :
  ?check:(Dolmen.Std.Term.t -> Dolmen.Std.Term.t ->
          Dolmen.Std.Term.t -> Dolmen.Std.Term.t -> unit) ->
  Type.env -> string -> (Dolmen.Std.Term.t -> Dolmen.Std.Expr.Term.t -> Dolmen.Std.Expr.Term.t -> Dolmen.Std.Expr.Term.t -> Dolmen.Std.Expr.Term.t) ->
  (Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> Dolmen.Std.Expr.Term.t)


val ty_app4 :
  ?check:(Dolmen.Std.Term.t -> Dolmen.Std.Term.t -> Dolmen.Std.Term.t ->
          Dolmen.Std.Term.t -> Dolmen.Std.Term.t -> unit) ->
  Type.env -> string -> (Dolmen.Std.Expr.Ty.t -> Dolmen.Std.Expr.Ty.t -> Dolmen.Std.Expr.Ty.t -> Dolmen.Std.Expr.Ty.t -> Dolmen.Std.Expr.Ty.t) ->
  (Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> Dolmen.Std.Expr.Ty.t)

val ty_app4_ast :
  ?check:(Dolmen.Std.Term.t -> Dolmen.Std.Term.t -> Dolmen.Std.Term.t ->
          Dolmen.Std.Term.t -> Dolmen.Std.Term.t -> unit) ->
  Type.env -> string -> (Dolmen.Std.Term.t -> Dolmen.Std.Expr.Ty.t -> Dolmen.Std.Expr.Ty.t -> Dolmen.Std.Expr.Ty.t -> Dolmen.Std.Expr.Ty.t -> Dolmen.Std.Expr.Ty.t) ->
  (Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> Dolmen.Std.Expr.Ty.t)

val term_app4 :
  ?check:(Dolmen.Std.Term.t -> Dolmen.Std.Term.t -> Dolmen.Std.Term.t ->
          Dolmen.Std.Term.t -> Dolmen.Std.Term.t -> unit) ->
  Type.env -> string -> (Dolmen.Std.Expr.Term.t -> Dolmen.Std.Expr.Term.t -> Dolmen.Std.Expr.Term.t -> Dolmen.Std.Expr.Term.t -> Dolmen.Std.Expr.Term.t) ->
  (Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> Dolmen.Std.Expr.Term.t)

val term_app4_ast :
  ?check:(Dolmen.Std.Term.t -> Dolmen.Std.Term.t -> Dolmen.Std.Term.t ->
          Dolmen.Std.Term.t -> Dolmen.Std.Term.t -> unit) ->
  Type.env -> string -> (Dolmen.Std.Term.t -> Dolmen.Std.Expr.Term.t -> Dolmen.Std.Expr.Term.t -> Dolmen.Std.Expr.Term.t -> Dolmen.Std.Expr.Term.t -> Dolmen.Std.Expr.Term.t) ->
  (Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> Dolmen.Std.Expr.Term.t)


val term_app_left :
  ?check:(Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> unit) ->
  Type.env -> string -> (Dolmen.Std.Expr.Term.t -> Dolmen.Std.Expr.Term.t -> Dolmen.Std.Expr.Term.t) ->
  (Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> Dolmen.Std.Expr.Term.t)

val term_app_left_ast :
  ?check:(Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> unit) ->
  Type.env -> string -> (Dolmen.Std.Term.t -> Dolmen.Std.Expr.Term.t -> Dolmen.Std.Expr.Term.t -> Dolmen.Std.Expr.Term.t) ->
  (Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> Dolmen.Std.Expr.Term.t)

val term_app_right :
  ?check:(Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> unit) ->
  Type.env -> string -> (Dolmen.Std.Expr.Term.t -> Dolmen.Std.Expr.Term.t -> Dolmen.Std.Expr.Term.t) ->
  (Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> Dolmen.Std.Expr.Term.t)

val term_app_right_ast :
  ?check:(Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> unit) ->
  Type.env -> string -> (Dolmen.Std.Term.t -> Dolmen.Std.Expr.Term.t -> Dolmen.Std.Expr.Term.t -> Dolmen.Std.Expr.Term.t) ->
  (Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> Dolmen.Std.Expr.Term.t)


val term_app_chain :
  ?check:(Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> unit) ->
  Type.env -> string -> (Dolmen.Std.Expr.Term.t -> Dolmen.Std.Expr.Term.t -> Dolmen.Std.Expr.Term.t) ->
  (Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> Dolmen.Std.Expr.Term.t)

val term_app_chain_ast :
  ?check:(Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> unit) ->
  Type.env -> string -> (Dolmen.Std.Term.t -> Dolmen.Std.Expr.Term.t -> Dolmen.Std.Expr.Term.t -> Dolmen.Std.Expr.Term.t) ->
  (Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> Dolmen.Std.Expr.Term.t)


