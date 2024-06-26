
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Smtlib2 extensions *)
(* ************************************************************************* *)

module Smtlib2 = struct

  type t = {
    name : string;
    stmts : string list;
    mutable active : bool;
  }

  let name { name; _ } = name
  let set t b = t.active <- b
  let enable t = set t true
  let disable t = set t false

  let all_exts = ref []
  let exts () = !all_exts
  let create ~name ~stmts =
    let t = { name; stmts; active = false; } in
    all_exts := t :: !all_exts;
    t

  let maxsmt =
    (* MaxSMT:
       the following statement names are routinely used when trying to
       optimize models on satisfiable problems. Most notably, Z3 accepts
       these, and opam generates problems using them. *)
    create
      ~name:"maxsmt"
      ~stmts:[ "minimize"; "maximize"; "get-objectives"; "assert-soft";]

  let statement s =
    let mk ?loc sexprs =
      Statement.other ?loc Id.(mk Decl s) sexprs
    in
    if List.exists (fun { active; stmts; name = _; } ->
        active && List.mem s stmts) (exts ())
    then Some mk else None

end
