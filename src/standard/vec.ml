
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)


(* Modules and aliases *)
(* ************************************************************************* *)

type 'a t = {
  mutable data : 'a array;
  mutable sz : int;
}

let make n x =
  { data = Array.make n x; sz = 0 }

let[@inline] create () =
  { data = [||]; sz = 0}

let[@inline] clear s =
  s.data <- [||];
  s.sz <- 0

let[@inline] shrink t i =
  assert (i >= 0);
  assert (i<=t.sz);
  t.sz <- i

let[@inline] pop t =
  if t.sz = 0 then invalid_arg "vec.pop";
  let x = Array.unsafe_get t.data (t.sz - 1) in
  t.sz <- t.sz - 1;
  x

let[@inline] size t = t.sz

let[@inline] is_empty t = t.sz = 0

let[@inline] is_full t = Array.length t.data = t.sz

let[@inline] copy t : _ t =
  let data = Array.copy t.data in
  {t with data}

(* grow the array *)
let[@inline never] grow_to_double_size t x : unit =
  if Array.length t.data = Sys.max_array_length then (
    failwith "vec: cannot resize";
  );
  let size =
    min Sys.max_array_length (max 4 (2 * Array.length t.data))
  in
  let arr' = Array.make size x in
  Array.blit t.data 0 arr' 0 (Array.length t.data);
  t.data <- arr';
  assert (Array.length t.data > t.sz);
  ()

let[@inline] get t i =
  if i < 0 || i >= t.sz then invalid_arg "vec.get";
  Array.unsafe_get t.data i

let[@inline] last t =
  if t.sz = 0 then invalid_arg "vec.last";
  Array.unsafe_get t.data (t.sz - 1)

let[@inline] push t x : unit =
  if is_full t then grow_to_double_size t x;
  Array.unsafe_set t.data t.sz x;
  t.sz <- t.sz + 1

let[@inline] set t i v =
  if i < 0 || i > t.sz then invalid_arg "vec.set";
  if i = t.sz then (
    push t v
  ) else (
    Array.unsafe_set t.data i v
  )


