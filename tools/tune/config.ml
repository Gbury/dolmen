
type t = {
  (* Command *)
  cmd : string;
  (* Gc params to test *)
  minor_heap_size       : Range.Int.t;
  major_heap_increment  : Range.Int.t;
  space_overhead        : Range.Int.t;
  max_overhead          : Range.Int.t;
  allocation_policy     : Range.Int.t;
}

