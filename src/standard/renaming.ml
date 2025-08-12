
(* This file is free software, part of dolmen. See file "LICENSE" for more details. *)

module type Arg = Dolmen_intf.Id.Scope

module Make(Id : Arg with type name := Name.t) = struct

  exception Already_bound of Id.t

  type rename =
    | Rename : {
        init : 'acc;
        rename : 'acc -> Name.t -> 'acc * Name.t
      } -> rename

  type conf = {
    rename : rename;
    sanitize : Name.t -> Name.t;
  }

  type t = {
    conf : conf;
    names : Name.t Id.Map.t;
  }

  let rename init rename =
    Rename { init; rename; }

  let mk ~rename ~sanitize =
    let conf = { sanitize; rename; } in
    let names = Id.Map.empty in
    { conf; names; }

  let find_opt t id =
    Id.Map.find_opt id t.names

  let rec bind ~scope t id name renamer =
    let name = t.conf.sanitize name in
    match Scope.find_opt name scope with
    | None ->
      (* easy case, no conflict of names *)
      let names = Id.Map.add id name t.names in
      let scope = Scope.bind name id scope in
      { t with names; }, scope
    | Some _other_id ->
      (* another id is already bound to that name, try and find a new name. *)
      let Rename { init; rename; } = renamer in
      let acc, new_name = rename init name in
      let renamer = Rename { init = acc; rename; } in
      bind ~scope t id new_name renamer

  let add ~scope t id name =
    match find_opt t id with
    | None -> bind ~scope t id name t.conf.rename
    | Some _ -> raise (Already_bound id)

end





