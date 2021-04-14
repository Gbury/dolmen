
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

module Tptp = struct

  let symbol m ~attr ~loc id =
    let t = match id with
      (* Base builtins *)
      | { Id.name =  "$_"; ns = Id.Term }     -> Term.(builtin ~loc Wildcard ())
      | { Id.name = "$tType" ; ns = Id.Term } -> Term.(builtin ~loc Ttype ())
      | { Id.name = "$o"; ns = Id.Term }      -> Term.(builtin ~loc Prop ())
      | { Id.name = "$true"; ns = Id.Term }   -> Term.(builtin ~loc True ())
      | { Id.name = "$false"; ns = Id.Term }  -> Term.(builtin ~loc False ())
      (* Arithmetic builtins *)
      | { Id.name = "$int"; ns = Id.Term }        -> Term.(builtin ~loc Int ())
      | { Id.name = "$less"; ns = Id.Term }       -> Term.(builtin ~loc Lt ())
      | { Id.name = "$lesseq"; ns = Id.Term }     -> Term.(builtin ~loc Leq ())
      | { Id.name = "$greater"; ns = Id.Term }    -> Term.(builtin ~loc Gt ())
      | { Id.name = "$greatereq"; ns = Id.Term }  -> Term.(builtin ~loc Geq ())
      | { Id.name = "$uminus"; ns = Id.Term }     -> Term.(builtin ~loc Minus ())
      | { Id.name = "$sum"; ns = Id.Term }        -> Term.(builtin ~loc Add ())
      | { Id.name = "$difference"; ns = Id.Term } -> Term.(builtin ~loc Sub ())
      | { Id.name = "$product"; ns = Id.Term }    -> Term.(builtin ~loc Mult ())
      | _ -> Term.(const ~loc id)
    in
    let attrs = List.map (Term.map m) attr in
    Term.add_attrs attrs t

  let mapper =
    { Term.id_mapper with symbol }

end

module Smtlib = struct

  let symbol m ~attr ~loc id =
    let t = match id with
      | { Id.name = "Bool"; ns = Id.Sort }  -> Term.(builtin ~loc Prop ())
      | { Id.name = "true"; ns = Id.Term }  -> Term.(builtin ~loc True ())
      | { Id.name = "false"; ns = Id.Term } -> Term.(builtin ~loc False ())
      | { Id.name = "not"; ns = Id.Term }   -> Term.(builtin ~loc Not ())
      | { Id.name = "and"; ns = Id.Term }   -> Term.(builtin ~loc And ())
      | { Id.name = "or"; ns = Id.Term }    -> Term.(builtin ~loc Or ())
      | { Id.name = "xor"; ns = Id.Term }   -> Term.(builtin ~loc Xor ())
      | { Id.name = "=>"; ns = Id.Term }    -> Term.(builtin ~loc Imply ())
      | { Id.name = "="; ns = Id.Term }     -> Term.(builtin ~loc Eq ())
      | { Id.name = "distinct"; ns = Id.Term } -> Term.(builtin ~loc Distinct ())
      | { Id.name = "ite"; ns = Id.Term }   -> Term.(builtin ~loc Ite ())
      | _ -> Term.(const ~loc id)
    in
    let attrs = List.map (Term.map m) attr in
    Term.add_attrs attrs t

  let binder_let_eq m t =
    match t with
    | { Term.term = Term.Colon (u, v) ; _ } ->
      Term.eq ~loc:t.Term.loc (Term.map m u) (Term.map m v)
    | _ -> assert false

  let binder m ~attr ~loc b l body =
    match b with
    | Term.Let_seq ->
      let attrs = List.map (Term.map m) attr in
      let l' = List.map (binder_let_eq m) l in
      Term.add_attrs attrs (Term.letin ~loc l' (Term.map m body))
    | Term.Let_par ->
      let attrs = List.map (Term.map m) attr in
      let l' = List.map (binder_let_eq m) l in
      Term.add_attrs attrs (Term.letand ~loc l' (Term.map m body))
    | _ -> Term.(id_mapper.binder m ~attr ~loc b l body)

  let mapper =
    { Term.id_mapper with symbol; binder }

end
