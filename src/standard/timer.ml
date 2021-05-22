
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Simple timer *)
(* ************************************************************************* *)

type t = {
  mutable start : float; (* start of the timer *)
}

let start () =
  (* allocate the recors before calling gettimeofday to
     avoid counting the time of the record allocation in the
     timer. *)
  let t = { start = 0.; } in
  t.start <- Unix.gettimeofday ();
  t

let stop t =
  let stop = Unix.gettimeofday () in
  stop -. t.start



