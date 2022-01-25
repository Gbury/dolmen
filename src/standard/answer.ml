
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(* Type definitions *)
type term = Term.t
type location = Loc.t
type defs = Statement.defs

type descr =
  | Unsat
  | Sat of defs list option

type t = {
  id : Id.t option;
  descr : descr;
  attrs : term list;
  loc : location;
}

let mk ?id ?(loc= Loc.no_loc) ?(attrs=[]) descr =
  { id; descr; loc; attrs; }

let fun_def ?loc id vars params ret_ty body : defs =
  Statement.group ~recursive:false [
    Statement.def ?loc id ~vars ~params ret_ty body
  ]

let funs_def_rec ?loc l =
  let contents = List.map (fun (id, vars, params, ret_ty, body) ->
      Statement.def ?loc id ~vars ~params ret_ty body
    ) l in
  Statement.group ~recursive:true contents

let sat ?loc model =
  mk ?loc (Sat model)

let unsat ?loc () =
  mk ?loc Unsat
