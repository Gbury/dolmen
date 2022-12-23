(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(** Pipelines

    This module implements functorised pipelines. A pipeline is basically a
    series of computations that is meant to be executed on a stream of elements.
    Each computation in a pipeline is called a pipe. A pipe can be as simple as
    a mapping function, transforming the input element into an output element,
    or more complex, expanding an element into a sequence of elemnents, and
    performing a fixpoint expansion.
*)

exception Sigint
(** Alias to {Sys.Break}. Raised upon user interrupt / when a
    {Sys.sigint} signal is received. *)

exception Out_of_time
(** Alias to {Alarm.Out_of_time}. *)

exception Out_of_space
(** Alias to {Alarm.Out_of_space}. *)

module Make(State : State.S) : sig
  (** Concrete pipelines. *)

  (** {2 Type definitions } *)

  type ('st, 'a, 'b) op
  (** An operator from values of type ['a] to value sof type ['b],
      and where a state value of type ['st] is carried throughout. *)

  type ('st, 'a, 'b) t
  (** The type of pipelines from values of type ['a] to values of type ['b],
      with state values of type ['st]. *)

  type 'st merge = 'st -> 'st -> 'st
  (** Merge function used at the end of a fixpoint to get the resulting state. *)

  type ('st, 'a) fix = [ `Ok | `Gen of 'st merge * ('st -> ('st * 'a option)) ]
  (** Type used to fixpoint expanding statements such as includes. *)

  type ('a, 'b) cont = [ `Done of 'a | `Continue of 'b ]
  (** Type used for continuation operators, allowing to leave the pipeline early. *)

  type 'st k_exn = { k : 'a. 'st -> Printexc.raw_backtrace -> exn -> 'a; }
  (** Exception continuation to provide when evaluating a pipeline manually,
      in order to evaluate an exception handler with the most up-to-date state. *)


  (** {2 Creating operators} *)

  val op : ?name:string -> ('st -> 'a -> 'st * 'b) -> ('st, 'a, 'b) op
  (** Base constructor function for operators. *)

  val apply : ?name:string -> ('a -> 'b) -> (_, 'a, 'b) op
  (** Create an operator from a function *)

  val iter_ : ?name:string -> ('a -> unit) -> (_, 'a, 'a) op
  (** Perform the function's side-effect and return the same input. *)

  val f_map :
    ?name:string ->
    ?test:('st -> 'a -> bool) ->
    ('st -> 'a -> 'st * 'b) ->
    ('st, 'a, ('a, 'b) cont) op
  (** TODO: doc *)



  (** {2 Creating pipelines} *)

  val _end : (_, 'a, 'a) t

  val (@>>>) : ('st, 'a, 'b) op -> ('st, 'b, 'c) t -> ('st, 'a, 'c) t
  (** Add an operator at the beginning of a pipeline. *)

  val (@>|>) : ('st, 'a, ('b, 'c) cont) op -> ('st, 'c, 'b) t -> ('st, 'a, 'b) t
  (** Add a continuation operator, allowing to stop evaluation of the
      pipeline early. *)

  val (@|||) : ('st, 'a, 'b) t -> ('st, 'b, 'c) t -> ('st, 'a, 'c) t
  (** Concatenate two pipeline. Whenever possible it is best to use [(@>>>)],
      which creates tail-rec pipelines. *)

  val fix : ('st, 'a, ('st, 'a) fix) op -> ('st, 'a, unit) t -> ('st, 'a, unit) t
  (** Perform a fixpoint expansion *)


  (** {2 Evaluating pipelines} *)

  val eval : exn:'st k_exn -> ('st, 'a, 'b) t -> 'st -> 'a -> 'st * 'b
  (** Evaluate a pipeline to a function. *)

  val run :
    finally:(State.t -> (Printexc.raw_backtrace * exn) option -> State.t) ->
    ?alarm:Alarm.t -> (State.t -> State.t * 'a option) -> State.t ->
    (State.t, 'a, unit) t -> State.t
    (** Loop the evaluation of a pipeline over a generator, and starting options.
        @param finally a function called at the end of every iteration (even if
        an exception has been raised) *)

end
