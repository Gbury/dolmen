
(* Create state *)

let create config output_state =
  State.with_out ~output_state ~f:(fun () ->
      State.empty config
    )


(* Run the tests in config *)

let gen_test_list ~cmd
    ~minor_heap_size
    ~major_heap_increment
    ~space_overhead
    ~max_overhead
    ~allocation_policy
  =
  Seq.return (Params.default ())
  |> Params.add_int_range
    Params.minor_heap_size minor_heap_size
  |> Params.add_int_range
    Params.major_heap_increment major_heap_increment
  |> Params.add_int_range
     Params.space_overhead space_overhead
  |> Params.add_int_range
    Params.max_overhead max_overhead
  |> Params.add_int_range
    Params.allocation_policy allocation_policy
  |> Seq.map (fun params -> params, cmd)
  |> List.of_seq

let create_progress_bar n =
  let total = Int64.of_int n in
  let bar =
    Progress_unix.counter
      ~total ~message:"Tests finished" ()
  in
  let f, display = Progress_unix.start bar in
  let report () =
    f 1L
  in
  let stop () =
    Progress_unix.finalise display
  in
  report, stop

let run_test report (params, cmd) =
  let env = Params.env params in
  let extra_args = Params.args params in
  let res = Exec.Full.run ~env (cmd ^ extra_args) in
  let () = report () in
  params, res

let split_result (info, res) =
  match res with
  | Ok res -> Either.Left (info, res)
  | Error error -> Either.Right (info, error)

let run input_state output_state j =
  State.with_ ~input_state ~output_state ~f:(fun (state : State.t) ->
      let config = state.config in
      let tests =
        gen_test_list
          ~cmd:config.cmd
          ~minor_heap_size:config.minor_heap_size
          ~major_heap_increment:config.major_heap_increment
          ~space_overhead:config.space_overhead
          ~max_overhead:config.max_overhead
          ~allocation_policy:config.allocation_policy
      in
      let report, stop_bar = create_progress_bar (List.length tests) in
      let results = Parmap.map ~j (run_test report) tests in
      let () = stop_bar () in
      let ok_results, error_results = List.partition_map split_result results in
      begin match error_results with
        | [] -> ()
        | (_info, msg) :: _ ->
          Format.eprintf "Error:@\n%s@." msg;
          Format.eprintf "ok: %d / errors: %d@."
            (List.length ok_results) (List.length error_results);
          ()
      end;
      let state = State.set_results ok_results state in
      state
    )


(* Generate the graphs *)

let res_real_time (_info, res) = (Exec.Full.stats res).real_time
let res_peak_size (_info, res) = float ((Exec.Full.stats res).peak_mem)
let res_minor_size (info, _res) = float info.Params.minor_heap_size
let res_major_increment (info, _res) = float info.Params.major_heap_increment
let res_space_overhead (info, _res) = float info.Params.space_overhead
let res_max_overhead (info, _res) = float info.Params.max_overhead
let res_allocation_policy (info, _res) = float info.Params.allocation_policy

let graph input_state =
  State.with_in ~input_state ~f:(fun (state : State.t) ->
      let file name = Format.asprintf "tune.%s.png" name in
      (* Time/Size graph *)
      Graph.mk ~file:(file "time-size") state.results
        ~x:res_real_time ~y:res_peak_size;
      (* Time+Size/minor_size graphs *)
      Graph.mk ~file:(file "time-minor") state.results
        ~x:res_minor_size ~y:res_real_time;
      Graph.mk ~file:(file "size-minor") state.results
        ~x:res_minor_size ~y:res_peak_size;
      (* Time+Size/major_increment graphs *)
      Graph.mk ~file:(file "time-major") state.results
        ~x:res_major_increment ~y:res_real_time;
      Graph.mk ~file:(file "size-major") state.results
        ~x:res_major_increment ~y:res_peak_size;
      (* Time+Size/space_overhead graphs *)
      Graph.mk ~file:(file "time-space-overhead") state.results
        ~x:res_space_overhead ~y:res_real_time;
      Graph.mk ~file:(file "size-space-overhead") state.results
        ~x:res_space_overhead ~y:res_peak_size;
      (* Time+Size/max_overhead graphs *)
      Graph.mk ~file:(file "time-max-overhead") state.results
        ~x:res_max_overhead ~y:res_real_time;
      Graph.mk ~file:(file "size-max-overhead") state.results
        ~x:res_max_overhead ~y:res_peak_size;
      (* Time+Size/policy graphs *)
      Graph.mk ~file:(file "time-policy") state.results
        ~x:res_allocation_policy ~y:res_real_time;
      Graph.mk ~file:(file "size-policy") state.results
        ~x:res_allocation_policy ~y:res_peak_size;
      (* end *)
      ()
    )
