(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Extensions builtins *)
(* ************************************************************************ *)

type t = {
  name : string;
  builtins : Env.builtins;
}

let all = Hashtbl.create 17
let list () =
  List.fast_sort (fun e e' -> String.compare e.name e'.name) @@
  Hashtbl.fold (fun _ e acc -> e :: acc) all []
let find_exn = Hashtbl.find all
let name { name; _ } = name
let builtins { builtins; _ } = builtins

let create ~name ~builtins =
  let t = { name; builtins; } in
  Hashtbl.replace all name t;
  t

let bvconv =
  create ~name:"bvconv" ~builtins:Bitv.bvconv_builtins