
let () =
  QCheck_runner.run_tests_main [
    Print.smtlib2_id;
    Print.smtlib2_id_printable;
    (* Maps.add_find; *)
  ]


