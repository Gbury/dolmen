(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Extensions builtins *)
(* ************************************************************************ *)

type t = {
  name : string;
  builtins : Env.builtins;
}

let name { name; _ } = name
let builtins { builtins; _ } = builtins

let registry = Hashtbl.create 17

let register ({ name; _ } as ext)  =
  match Hashtbl.find registry name with
  | exception Not_found -> Hashtbl.replace registry name [ ext ]
  | exts -> Hashtbl.replace registry name (ext :: exts)

let find_all name =
  try Hashtbl.find registry name with Not_found -> []

let create ~name ~builtins =
  let t = { name ; builtins } in
  register t;
  t

let bvconv =
  create ~name:"bvconv" ~builtins:Bitv.bvconv_builtins