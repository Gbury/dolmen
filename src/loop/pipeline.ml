
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
  type 'st merge = 'st -> 'st -> 'st
  type ('a, 'b) cont = [ `Done of 'a | `Continue of 'b ]
  type ('st, 'a) fix = [ `Ok | `Gen of 'st merge * 'a gen ]

  type ('st, 'a, 'b) op = {
    name : string;
    f : 'st -> 'a -> 'st * 'b;
  }

  (* Type for pipelines, i.e a series of transformations to
      apply to the input. An ('st, 'a, 'b) t is a pipeline that
      takes an input of type ['a] and returns a value of type
      ['b]. *)
  type (_, _, _) t =
    (* The end of the pipeline, the identity/reflexive constructor *)
    | End :
        ('st, 'a, 'a) t
    (* Apply a single function and then proceed with the rest of the pipeline *)
    | Map :
        ('st, 'a, 'c) op * ('st, 'c, 'b) t -> ('st, 'a, 'b) t
    (* Allow early exiting from the loop *)
    | Cont :
        ('st, 'a, ('b, 'c) cont) op * ('st, 'c, 'b) t -> ('st, 'a, 'b) t
    (* Concat two pipeline. Not tail recursive. *)
    | Concat :
        ('st, 'a, 'b) t * ('st, 'b, 'c) t -> ('st, 'a, 'c) t
    (* Fixpoint expansion *)
    | Fix :
        ('st, 'a, ('st, 'a) fix) op * ('st, 'a, unit) t -> ('st, 'a, unit) t

  (* Creating operators. *)

  let op ?(name="") f = { name; f; }

  let apply ?name f = op ?name (fun st x -> st, f x)

  let iter_ ?name f = op ?name (fun st x -> f x; st, x)

  let f_map ?name ?(test=(fun _ _ -> true)) f =
    op ?name (fun st x ->
        if test st x then begin
          let st', y = f st x in
          st', `Continue y
        end else
          st, `Done x
      )

  (* Creating pipelines. *)

  let _end = End
  let (@>>>) op t = Map(op, t)
  let (@>|>) op t = Cont(op, t)
  let (@|||) t t' = Concat (t, t')

  let fix op t = Fix(op, t)

  (* Eval a pipeline into the corresponding function *)
  let rec eval : type st a b.
    (st, a, b) t -> st -> a -> st * b =
    fun pipe st x ->
    match pipe with
    | End -> st, x
    | Map (op, t) ->
      let st', y = op.f st x in
      eval t st' y
    | Cont (op, t) ->
      let st', y = op.f st x in
      begin match y with
        | `Continue res -> eval t st' res
        | `Done res -> st', res
      end
    | Concat (t, t') ->
      let st', y = eval t st x in
      eval t' st' y
    | Fix (op, t) ->
      let st', y = op.f st x in
      begin match y with
        | `Ok -> eval t st' x
        | `Gen (merge, g) ->
          let aux st c =
            let st', () = eval pipe st c in
            st'
          in
          let st'' = Gen.fold aux st' g in
          let st''' = merge st st'' in
          st''', ()
      end

  (* Aux function to eval a pipeline on the current value of a generator. *)
  let run_aux : type st a b.
    (st, a, b) t ->
    (st -> a option) ->
    st -> (st * b) option =
    fun pipe g st ->
    match g st with
    | None -> None
    | Some x -> Some (eval pipe st x)

  (* Effectively run a pipeline on all values that come from a generator.
     Time/size limits apply for the complete evaluation of each input
     (so all expanded values count toward the same limit). *)
  let rec run :
    type a.
    finally:(State.t -> exn option -> State.t) ->
    (State.t -> a option) -> State.t -> (State.t, a, unit) t -> State.t
    = fun ~finally g st pipe ->
      let time = State.time_limit st in
      let size = State.size_limit st in
      let al = setup_alarm time size in
      begin
        match run_aux pipe g st with
        | None ->
          let () = delete_alarm al in
          st
        | Some (st', ()) ->
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
