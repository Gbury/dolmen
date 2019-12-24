(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** Pipelines

    This module implements functorised pipelines. A pipeline is basically a
    series of computations that is meant to be executed on a stream of elements.
    Each computation in a pipeline is called a pipe. A pipe can be as simple as
    a mapping function, transforming the input element into an output element,
    or more complex, expanding an element into a sequence of elemnents, and
    performing a fixpoint expansion.
*)

module Make(Util : Util.S) : sig
  (** Concrete pipelines. *)


  (** {2 Type definitions } *)

  type ('a, 'b) op
  (** An operator from values of type ['a] to value sof type ['b]. *)

  type ('a, 'b) t
  (** The type of pipelines from values of type ['a] to values of type ['b]. *)

  type 'a fix = [ `Ok | `Gen of bool * 'a Gen.t ]
  (** Type used to fixpoint expanding statements such as includes. *)

  type ('a, 'b) cont = [ `Continue of 'a | `Done of 'b ]
  (** Type used for continuation operators, allowing to leave the pipeline early. *)


  (** {2 Creating operators} *)

  val apply : ?name:string -> ('a -> 'b) -> ('a, 'b) op
  (** Create an operator from a function *)

  val f_map :
    ?name:string ->
    ?test:('a -> bool) ->
    ('a * 'b -> 'c) ->
    ('a * 'b, ('a * 'c, 'a) cont) op
  (** TODO: doc *)

  val iter_ : ?name:string -> ('a -> unit) -> ('a, 'a) op
  (** Perform the function's side-effect and return the same input. *)


  (** {2 Creating pipelines} *)

  val _end : ('a, 'a) t

  val (@>>>) : ('a, 'b) op -> ('b, 'c) t -> ('a, 'c) t
  (** Add an operator at the beginning of a pipeline. *)

  val (@>|>) : ('a, ('b, 'c) cont) op -> ('b, 'c) t -> ('a, 'c) t
  (** Add a continuation operator, allowing to stop evaluation of the
      pipeline early. *)

  val (@|||) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  (** Concatenate two pipeline. Whenever possible it is best to use [(@>>>)],
      which creates tail-rec pipelines. *)

  val fix : ('a * 'b, 'a * 'b fix) op -> ('a * 'b, 'a) t -> ('a * 'b, 'a) t
  (** Perform a fixpoint expansion *)


  (** {2 Evaluating pipelines} *)

  val eval : ('a, 'b) t -> 'a -> 'b
  (** Evaluate a pipeline to a function. *)

  val run :
    ?finally:(Util.opt -> exn option -> Util.opt) ->
    (Util.opt -> 'a option) -> Util.opt ->
    (Util.opt * 'a, Util.opt) t ->
    Util.opt
    (** Loop the evaluation of a pipeline over a generator, and starting options.
        @param finally a function called at the end of every iteration (even if
        an exception has been raised) *)

end
