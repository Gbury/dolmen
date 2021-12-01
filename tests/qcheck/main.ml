
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)
module M = Map.Make(String)
module T = Dolmen_std.Maps.String

let () =
  QCheck_runner.run_tests_main [
    Print.smtlib2_id;
    Print.smtlib2_id_printable;
    Print.poly_smtlib2_id;
    Print.poly_smtlib2_id_printable;
    Maps.add_find;
  ]


