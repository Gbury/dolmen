
(* Ensure that children are killed on sigterm. *)
let die_on_sigterm : unit -> unit =
  let thunk = lazy (
    Sys.set_signal 15
      (Sys.Signal_handle
         (fun _ ->
            print_endline "received sigterm, exiting";
            Unix.kill 0 15; (* kill children *)
            exit 1)))
  in fun () -> Lazy.force thunk

(* parralel map *)
let map ~j f l =
  if j<1 then invalid_arg "Parmap.map";
  die_on_sigterm();
  (* NOTE: for some reason the pool seems to spawn one too many thread
     in some cases. So we add a guard to respect [-j] properly. *)
  let sem = CCSemaphore.create j in
  let f_with_sem x =
    CCSemaphore.with_acquire ~n:1 sem ~f:(fun () -> f x)
  in
  let module P = CCPool.Make(struct
      let max_size = j
    end) in
  let res = match l with
    | [] -> []
    | _ ->
      P.Fut.map_l (fun x -> P.Fut.make1 f_with_sem x) l
      |> P.Fut.get
  in
  P.stop();
  res

