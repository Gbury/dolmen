
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Global things *)
(* ************************************************************************* *)

let enabled = ref false

let wrap_at_exit print t =
  at_exit (fun () ->
      if !enabled then Format.printf "%a@." print t
    );
  t

(* One-value statistics *)
(* ************************************************************************* *)

module Float = struct

  type t = {
    name : string;
    mutable value : float;
  }

  let print fmt t =
      Format.fprintf fmt
        "* %s: @[<hov>%f@]"
        t.name t.value

  let create name =
    wrap_at_exit print { name; value = nan; }

  let set t v =
    t.value <- v

end

(* Cumulative statistics *)
(* ************************************************************************* *)

module Floats = struct

  type t = {
    name : string;
    mutable count : int;
    mutable total_time : float;
  }

  let print fmt t =
      Format.fprintf fmt
        "* @[<v>%s@;\
          + count: %d@;\
          + total: %f@;\
          + mean : %f@]"
        t.name
        t.count
        t.total_time
        (t.total_time /. float t.count)

  let create name =
    wrap_at_exit print { name; count = 0; total_time = 0.; }

  let add t time =
    t.total_time <- t.total_time +. time;
    t.count <- t.count + 1;
    ()

end

