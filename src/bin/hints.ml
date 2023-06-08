
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)


let model ~check_model:_ =
  (*
  (* Model mode hints *)
  if check_model && (check_model_mode = Dolmen_model.Loop.Full) then
      Dolmen_loop.Report.add_hint Dolmen_loop.Report.Error.spaceout
        (Format.dprintf "%a" Format.pp_print_text
           "The full mode for checking model can use up a lot of memory, \
            you might want to try using the `--check-model-mode=interleave` option.");
  if check_model && (check_model_mode = Dolmen_model.Loop.Interleave) then
    Dolmen_loop.Report.add_hint Dolmen_model.Loop.undefined_constant
      (Format.dprintf "%a" Format.pp_print_text
         "The interleave mode for checking model requires constants in the \
          model file to be defined in the same order as they are declared in \
          the input problem file, \
          you might want to try using the `--check-model-mode=full` option.");
  *)
  ()

