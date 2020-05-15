
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

module Make(State : State_intf.Pipeline) = struct

  exception Sigint
  exception Out_of_time
  exception Out_of_space

  (* GC alarm for time/space limits *)
  (* ************************************************************************ *)

  (* This function analyze the current size of the heap *)
  let check size_limit = function () ->
    let heap_size = (Gc.quick_stat ()).Gc.heap_words in
    let s = float heap_size *. float Sys.word_size /. 8. in
    if s > size_limit then
      raise Out_of_space

  (* There are two kinds of limits we want to enforce:
     - a size limit: we use the Gc's alarm function to enforce the limit
       on the size of the RAM used
     - a time limit: the Gc alarm is not reliable to enforce this, so instead
       we use the Unix.timer facilities
     TODO: this does not work on windows.
     TODO: allow to use the time limit only for some passes *)
  let setup_alarm t s =
    let _ = Unix.setitimer Unix.ITIMER_REAL
        Unix.{it_value = t; it_interval = 0.01 } in
    Gc.create_alarm (check s)

  let delete_alarm alarm =
    let _ = Unix.setitimer Unix.ITIMER_REAL
        Unix.{it_value = 0.; it_interval = 0. } in
    Gc.delete_alarm alarm

  (* The Unix.timer works by sending a Sys.sigalrm, so in order to use it,
     we catch it and raise the Out_of_time exception. *)
  let () =
    Sys.set_signal Sys.sigalrm (
      Sys.Signal_handle (fun _ ->
          raise Out_of_time)
    )

  (* We also want to catch user interruptions *)
  let () =
    Sys.set_signal Sys.sigint (
      Sys.Signal_handle (fun _ ->
          raise Sigint)
    )

  (* Pipeline and execution *)
  (* ************************************************************************ *)

  type 'a gen = 'a Gen.t
  type 'a fix = [ `Ok | `Gen of bool * 'a gen ]
  type ('a, 'b) cont = [ `Continue of 'a | `Done of 'b ]

  type ('a, 'b) op = {
    name : string;
    f : 'a -> 'b;
  }

  (* Type for pipelines, i.e a series of transformations to
      apply to the input. An ('a, 'b) t is a pipeline that
      takes an input of type ['a] and returns a value of type
      ['b]. *)
  type (_, _) t =
    (* The end of the pipeline, the identity/reflexive constructor *)
    | End :
        ('a, 'a) t
    (* Apply a single function and then proceed with the rest of the pipeline *)
    | Map :
        ('a, 'c) op * ('c, 'b) t -> ('a, 'b) t
    | Cont :
        ('a, ('c, 'b) cont) op * ('c, 'b) t -> ('a, 'b) t
    (* Concat two pipeline. Not tail recursive. *)
    | Concat :
        ('a, 'b) t * ('b, 'c) t -> ('a, 'c) t
    (* Fixpoint expansion *)
    | Fix :
        ('st * 'a, 'st * 'a fix) op * ('st * 'a, 'st) t -> ('st * 'a, 'st) t

  (* Creating pipelines. *)

  let apply ?(name="") f =
    { name; f; }
  let iter_ ?(name="") f =
    { name; f = (fun x -> f x; x); }
  let f_map ?(name="") ?(test=(fun _ -> true)) f =
    { name; f = (fun ((st, _) as x) ->
          if test st then `Continue (f x) else `Done st); }

  let _end = End
  let (@>>>) op t = Map(op, t)
  let (@>|>) op t = Cont(op, t)
  let (@|||) t t' = Concat (t, t')

  let fix op t = Fix(op, t)

  (* Eval a pipeline into the corresponding function *)
  let rec eval : type a b. (a, b) t -> a -> b =
    fun pipe x ->
    match pipe with
    | End -> x
    | Map (op, t) ->
      eval t (op.f x)
    | Cont (op, t) ->
      begin match op.f x with
        | `Continue res -> eval t res
        | `Done res -> res
      end
    | Concat (t, t') ->
      let y = eval t x in
      eval t' y
    | Fix (op, t) ->
      let st, y = x in
      begin match op.f x with
        | st', `Ok -> eval t (st', y)
        | st', `Gen (flat, g) ->
          let aux st c = eval pipe (st, c) in
          let st'' = Gen.fold aux st' g in
          if flat then st'' else st
      end

  (* Aux function to eval a pipeline on the current value of a generator. *)
  let run_aux : type a.
    (State.t * a, State.t) t ->
    (State.t -> a option) ->
    State.t -> State.t option =
    fun pipe g st ->
    match g st with
    | None -> None
    | Some x -> Some (eval pipe (st, x))

  (* Effectively run a pipeline on all values that come from a generator.
     Time/size limits apply for the complete evaluation of each input
     (so all expanded values count toward the same limit). *)
  let rec run :
    type a.
    finally:(State.t -> exn option -> State.t) ->
    (State.t -> a option) -> State.t -> (State.t * a, State.t) t -> State.t
    = fun ~finally g st pipe ->
      let time = State.time_limit st in
      let size = State.size_limit st in
      let al = setup_alarm time size in
      begin
        match run_aux pipe g st with
        | None ->
          let () = delete_alarm al in
          st
        | Some st' ->
          let () = delete_alarm al in
          let st'' = try finally st' None with _ -> st' in
          run ~finally g st'' pipe
        | exception exn ->
          let bt = Printexc.get_raw_backtrace () in
          (* delete alarm *)
          let () = delete_alarm al in
          (* Flush stdout and print a newline in case the exn was
             raised in the middle of printing *)
          Format.pp_print_flush Format.std_formatter ();
          (* Print the backtrace if requested *)
          if Printexc.backtrace_status () then
            Printexc.print_raw_backtrace stdout bt;
          (* Go on running the rest of the pipeline. *)
          let st' = finally st (Some exn) in
          run ~finally g st' pipe
      end

end
