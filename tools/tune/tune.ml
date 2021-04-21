
let create_term =
  let info = Cmdliner.Term.info "create" in
  Cmdliner.Term.(
    const Run.create
    $ Options.config
    $ Options.output_state
  ), info

let run_term =
  let info = Cmdliner.Term.info "run" in
  Cmdliner.Term.(
    const Run.run
    $ Options.input_state
    $ Options.output_state
    $ Options.j
  ), info

let graph_term =
  let info = Cmdliner.Term.info "graph" in
  Cmdliner.Term.(
    const Run.graph
    $ Options.input_state
  ), info

let () =
  let info = Cmdliner.Term.info ~version:"dev" "tune" in
  let default = fst run_term, info in
  let choices = [create_term; run_term; graph_term] in
  match Cmdliner.Term.eval_choice default choices with
  | `Ok () -> ()
  | `Version | `Help -> exit 0
  | `Error `Parse | `Error `Term | `Error `Exn ->
    exit Cmdliner.Term.exit_status_cli_error



