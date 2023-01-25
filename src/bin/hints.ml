
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

let model ~check_model ~check_model_mode =
  (* Model mode hints *)
  if check_model && (check_model_mode = Dolmen_model.Loop.Full) then
      Dolmen_loop.Report.add_hint Dolmen_loop.Report.Error.spaceout
        (Format.dprintf "%a" Format.pp_print_text
           "The full mode for checking model can use up a lot of memory, \
            you might want to try using the `--check-model-mode=interleave` option.");
  ()


