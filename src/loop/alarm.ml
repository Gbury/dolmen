
(* Signature & type definition *)
(* ************************************************************************ *)

exception Out_of_time
exception Out_of_space

module type S = sig

  type t

  val setup : time:float -> size:float -> t

  val delete : t -> unit

end

type t = (module S)

(* Dummy alarms *)
(* ************************************************************************ *)

module Dummy : S = struct

  type t = unit

  let setup ~time:_ ~size:_ = ()

  let delete () = ()

end

let dummy : t = (module Dummy)

(* helper functions *)
(* ************************************************************************ *)

let check_time time_limit =
  let start_time = Unix.gettimeofday () in
  function () ->
    let t = Unix.gettimeofday () -. start_time in
    if t > time_limit then raise Out_of_time

(* This function analyze the current size of the heap
   TODO: take into account the minor heap size + stack size
   TODO: should we only consider the live words ? *)
let check_size size_limit () =
  let heap_size = (Gc.quick_stat ()).Gc.heap_words in
  let s = float heap_size *. float Sys.word_size /. 8. in
  if s > size_limit then raise Out_of_space

(* Linux alarms *)
(* ************************************************************************ *)

module Linux : S = struct

  type t = Gc.alarm option

  (* There are two kinds of limits we want to enforce:
     - a size limit: we use the Gc's alarm function to enforce the limit
       on the size of the RAM used
     - a time limit: the Gc alarm is not reliable to enforce this, so instead
       we use the Unix.timer facilities *)
  let setup ~time:t ~size:s =
    (* The Unix.timer works by sending a Sys.sigalrm, so in order to use it,
       we catch it and raise the Out_of_time exception. *)
    Sys.set_signal Sys.sigalrm (
      Sys.Signal_handle (fun _ ->
          raise Out_of_time)
    );
    if t <> infinity then
      ignore (Unix.setitimer Unix.ITIMER_REAL
                Unix.{it_value = t; it_interval = 0.01 });
    if s <> infinity then (Some (Gc.create_alarm (check_size s)))
    else None

  let delete alarm =
    (* it's alwyas safe to delete the timer here,
       even if none was present before. *)
    ignore (Unix.setitimer Unix.ITIMER_REAL
              Unix.{it_value = 0.; it_interval = 0. });
    match alarm with None -> () | Some alarm -> Gc.delete_alarm alarm

end

let linux : t = (module Linux)


(* Windows alarms *)
(* ************************************************************************ *)

module Windows : S = struct

  type t = Gc.alarm option

  (* This function analyze the current size of the heap
     TODO: take into account the minor heap size + stack size
     TODO: should we only consider the live words ? *)
  let check time_limit size_limit =
    let check_t = check_time time_limit in
    function () ->
      check_t ();
      check_size size_limit ()

  (* There are two kinds of limits we want to enforce:
     - a size limit: we use the Gc's alarm function to enforce the limit
       on the size of the RAM used
     - a time limit: the Gc alarm is not reliable to enforce this, but the
       Unix.timer facilities are not emulated on windows, so we still use
       the GC alarm for this, even if it's not that great.
     TODO: allow to use the time limit only for some passes *)
  let setup ~time:t ~size:s =
    if s <> infinity || t <> infinity
    then (Some (Gc.create_alarm (check t s)))
    else None

  let delete = function
    | None -> ()
    | Some alarm -> Gc.delete_alarm alarm

end

let windows : t = (module Windows)

(* Default alarms *)
(* ************************************************************************ *)

let default =
  match Sys.os_type with
  | "Unix" -> linux
  | "Win32" -> windows
  | "Cygwin" -> windows (* maybe linux would work, but better safe than sorry *)
  | _ -> dummy


