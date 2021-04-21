
type ('set, 'v) t = {
  set : 'set -> 'v -> 'set;
}

let add_param_range seq param range =
  Seq.flat_map (fun s ->
      Seq.map (fun param_val ->
          param.set s param_val
        ) range
    ) seq

(* GC controls *)
module GC = struct

  type param =
    | Minor_heap_size
    | Major_heap_increment
    | Space_overhead
    | Max_overhead
    | Allocation_policy

  type t = Gc.control

  let default () = Gc.get ()

  let set param control value =
    match param with
    | Minor_heap_size -> { control with Gc.minor_heap_size = value; }
    | Major_heap_increment -> { control with Gc.major_heap_increment = value; }
    | Space_overhead -> { control with Gc.space_overhead = value; }
    | Max_overhead -> { control with Gc.max_overhead = value; }
    | Allocation_policy -> { control with Gc.allocation_policy = value; }

  let minor_heap_size = {
    set = set Minor_heap_size;
  }

  let major_hepa_increment = {
    set = set Major_heap_increment;
  }

  let space_overhead = {
    set = set Space_overhead;
  }

  let max_overhead = {
    set = set Space_overhead;
  }

  let allocation_policy = {
    set = set Allocation_policy;
  }


end
