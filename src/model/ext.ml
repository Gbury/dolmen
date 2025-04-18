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
  Hashtbl.add registry name ext

let find_all name =
  Hashtbl.find_all registry name

let create ~name ~builtins =
  let t = { name ; builtins } in
  register t;
  t

let iter f =
  Hashtbl.iter (fun _ ext -> f ext) registry

