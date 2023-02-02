
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Some helpers *)
(* ************************************************************************* *)

let file_size file_path =
  let st = Unix.stat file_path in
  st.st_size

(* Stats & progress bars *)
(* ************************************************************************* *)

module type S = Stats_intf.S

module Noop(State : State.S) = struct

  type input = unit
  type counter = unit

  let start_counter _ = None

  let new_input st _ _ = (), st

  let record_parsed st _ _ _ = st

  let record_typed st _ _ _ = st

  let record_checked st _ _ _ = st

end

