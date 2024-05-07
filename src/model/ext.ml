(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Extensions builtins *)
(* ************************************************************************ *)

type t = {
  name : string;
  builtins : Env.builtins;
}

let all = ref []
let list () = !all
let name { name; _ } = name
let builtins { builtins; _ } = builtins

let create ~name ~builtins =
  let t = { name; builtins; } in
  all := t :: !all;
  t

let bvconv =
  create ~name:"bvconv" ~builtins:Bitv.bvconv_builtins