
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

include Dolmen_loop.State.Make(struct
    type ty_state = Typer.T.state
  end)

