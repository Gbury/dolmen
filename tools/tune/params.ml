
type t = {
  (* gc options *)
  minor_heap_size : int;
  major_heap_increment : int;
  space_overhead : int;
  max_overhead : int;
  allocation_policy : int;
  (* cli options ? *)
}


(* Translation to concret env and args *)

let to_ocamlrunparam_string
    { minor_heap_size; major_heap_increment;
      space_overhead; max_overhead; allocation_policy; } =
  Format.sprintf
    "a=%d,s=%d,i=%d,o=%d,O=%d"
    allocation_policy minor_heap_size major_heap_increment
    space_overhead max_overhead

let env t =
  let ocamlrunparam =
    Format.asprintf "OCAMLRUNPARAM=%s"
      (to_ocamlrunparam_string t)
  in
  Array.append (Unix.environment ()) [| ocamlrunparam |]

let args _t =
  ""


(* default *)

let default () =
  let gc = Gc.get() in {
    minor_heap_size = gc.minor_heap_size;
    major_heap_increment = gc.major_heap_increment;
    space_overhead = gc.space_overhead;
    max_overhead = gc.max_overhead;
    allocation_policy = gc.allocation_policy;
  }


(* Generic param handling *)

type 'v param = {
  set : t -> 'v -> t;
}

let add_seq param range seq =
  Seq.flat_map (fun s ->
      Seq.map (fun param_val ->
          param.set s param_val
        ) range
    ) seq

let add_int_range param range seq =
  add_seq param (Range.Int.to_seq range) seq


(* Params *)

let minor_heap_size = {
  set = fun t minor_heap_size -> { t with minor_heap_size; };
}
let major_heap_increment = {
  set = fun t major_heap_increment -> { t with major_heap_increment; };
}
let space_overhead = {
  set = fun t space_overhead -> { t with space_overhead; };
}
let max_overhead = {
  set = fun t max_overhead -> { t with max_overhead; };
}
let allocation_policy = {
  set = fun t allocation_policy -> { t with allocation_policy; };
}

