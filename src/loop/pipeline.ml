
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

module Make(State : State_intf.Pipeline) = struct

  exception Sigint
  exception Out_of_time
  exception Out_of_space

  (* GC alarm for time/space limits *)
  (* ************************************************************************ *)

  (* This function analyze the current size of the heap
     TODO: take into account the minor heap size
     TODO: should we only consider the live words ? *)
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
    if t <> infinity then
      ignore (Unix.setitimer Unix.ITIMER_REAL
                Unix.{it_value = t; it_interval = 0.01 });
    if s <> infinity then (Some (Gc.create_alarm (check s)))
    else None

  let delete_alarm t alarm =
    if t <> infinity then
    ignore (Unix.setitimer Unix.ITIMER_REAL
              Unix.{it_value = 0.; it_interval = 0. });
    match alarm with None -> () | Some alarm -> Gc.delete_alarm alarm

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
  type 'st k_exn = { k : 'a. 'st -> Printexc.raw_backtrace -> exn -> 'a; }

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

  (* Eval an operator *)
  let eval_op ~exn op st x =
    try op.f st x
    with e ->
      let bt = Printexc.get_raw_backtrace () in
      exn.k st bt e

  (* Eval a pipeline into the corresponding function *)
  let rec eval : type st a b.
    exn:st k_exn -> (st, a, b) t -> st -> a -> st * b =
    fun ~exn pipe st x ->
    match pipe with
    | End -> st, x
    | Map (op, t) ->
      let st', y = eval_op ~exn op st x in
      eval ~exn t st' y
    | Cont (op, t) ->
      let st', y = eval_op ~exn op st x in
      begin match y with
        | `Continue res -> eval ~exn t st' res
        | `Done res -> st', res
      end
    | Concat (t, t') ->
      let st', y = eval ~exn t st x in
      eval ~exn t' st' y
    | Fix (op, t) ->
      let st', y = eval_op ~exn op st x in
      begin match y with
        | `Ok -> eval ~exn t st' x
        | `Gen (merge, g) ->
          let st'' = eval_gen_fold ~exn pipe st' g in
          let st''' = merge st st'' in
          st''', ()
      end

  and eval_gen_fold : type st a.
    exn: st k_exn -> (st, a, unit) t -> st -> a gen -> st =
    fun ~exn pipe st g ->
    match g () with
    | None -> st
    | Some x ->
      let st', () = eval ~exn pipe st x in
      eval_gen_fold ~exn pipe st' g
    | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      exn.k st bt e

  (* Aux function to eval a pipeline on the current value of a generator. *)
  let run_aux ~exn pipe g st =
    match g st with
    | None -> None
    | Some x -> Some (eval ~exn pipe st x)
    | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      exn.k st bt e

  (* Effectively run a pipeline on all values that come from a generator.
     Time/size limits apply for the complete evaluation of each input
     (so all expanded values count toward the same limit). *)
  let rec run :
    type a.
    finally:(State.t -> exn option -> State.t) ->
    (State.t -> a option) -> State.t -> (State.t, a, unit) t -> State.t
    = fun ~finally g st pipe ->
      let exception Exn of State.t * Printexc.raw_backtrace * exn in
      let time = State.time_limit st in
      let size = State.size_limit st in
      let al = setup_alarm time size in
      let exn = { k = fun st bt e ->
          (* delete alamr as soon as possible *)
          let () = delete_alarm time al in
          (* go the the correct handler *)
          raise (Exn (st, bt, e));
        }
      in
      begin
        match run_aux ~exn pipe g st with

        (* End of the run, yay ! *)
        | None ->
          let () = delete_alarm time al in
          st

        (* Regular case, we finished running the pipeline on one input
           value, let's get to the next one. *)
        | Some (st', ()) ->
          let () = delete_alarm time al in
          let st'' = try finally st' None with _ -> st' in
          run ~finally g st'' pipe

        (* "Normal" exception case: the exn was raised by an operator, and caught
           then re-raised by the {exn} cotinuation passed to run_aux *)
        | exception Exn (st, bt, e) ->
          (* Flush stdout and print a newline in case the exn was
             raised in the middle of printing *)
          Format.pp_print_flush Format.std_formatter ();
          Format.pp_print_flush Format.err_formatter ();
          (* Print the backtrace if requested *)
          if Printexc.backtrace_status () then
            Printexc.print_raw_backtrace stdout bt;
          (* Go on running the rest of the pipeline. *)
          let st' = finally st (Some e) in
          run ~finally g st' pipe

        (* Exception case for exceptions, that can realisically occur for all
           asynchronous exceptions, or if some operator was not properly wrapped.
           In this error case, we might use a rather old and outdate state, but
           this should not happen often, and should not matter for asynchronous
           exceptions. *)
        | exception e ->
          let bt = Printexc.get_raw_backtrace () in
          (* delete alarm *)
          let () = delete_alarm time al in
          (* Flush stdout and print a newline in case the exn was
             raised in the middle of printing *)
          Format.pp_print_flush Format.std_formatter ();
          Format.pp_print_flush Format.err_formatter ();
          (* Print the backtrace if requested *)
          if Printexc.backtrace_status () then
            Printexc.print_raw_backtrace stdout bt;
          (* Go on running the rest of the pipeline. *)
          let st' = finally st (Some e) in
          run ~finally g st' pipe
      end

end
