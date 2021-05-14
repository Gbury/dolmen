
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

module Tptp = struct

  let symbol m ~attr ~loc id =
    let t = match id with
      (* Base builtins *)
      | { Id.name = Simple "$_"; ns = Term }     -> Term.(builtin ~loc Wildcard ())
      | { Id.name = Simple "$tType" ; ns = Term } -> Term.(builtin ~loc Ttype ())
      | { Id.name = Simple "$o"; ns = Term }      -> Term.(builtin ~loc Prop ())
      | { Id.name = Simple "$true"; ns = Term }   -> Term.(builtin ~loc True ())
      | { Id.name = Simple "$false"; ns = Term }  -> Term.(builtin ~loc False ())
      (* Arithmetic builtins *)
      | { Id.name = Simple "$int"; ns = Term }        -> Term.(builtin ~loc Int ())
      | { Id.name = Simple "$less"; ns = Term }       -> Term.(builtin ~loc Lt ())
      | { Id.name = Simple "$lesseq"; ns = Term }     -> Term.(builtin ~loc Leq ())
      | { Id.name = Simple "$greater"; ns = Term }    -> Term.(builtin ~loc Gt ())
      | { Id.name = Simple "$greatereq"; ns = Term }  -> Term.(builtin ~loc Geq ())
      | { Id.name = Simple "$uminus"; ns = Term }     -> Term.(builtin ~loc Minus ())
      | { Id.name = Simple "$sum"; ns = Term }        -> Term.(builtin ~loc Add ())
      | { Id.name = Simple "$difference"; ns = Term } -> Term.(builtin ~loc Sub ())
      | { Id.name = Simple "$product"; ns = Term }    -> Term.(builtin ~loc Mult ())
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
      | { Id.name = Simple "Bool"; ns = Sort }  -> Term.(builtin ~loc Prop ())
      | { Id.name = Simple "true"; ns = Term }  -> Term.(builtin ~loc True ())
      | { Id.name = Simple "false"; ns = Term } -> Term.(builtin ~loc False ())
      | { Id.name = Simple "not"; ns = Term }   -> Term.(builtin ~loc Not ())
      | { Id.name = Simple "and"; ns = Term }   -> Term.(builtin ~loc And ())
      | { Id.name = Simple "or"; ns = Term }    -> Term.(builtin ~loc Or ())
      | { Id.name = Simple "xor"; ns = Term }   -> Term.(builtin ~loc Xor ())
      | { Id.name = Simple "=>"; ns = Term }    -> Term.(builtin ~loc Imply ())
      | { Id.name = Simple "="; ns = Term }     -> Term.(builtin ~loc Eq ())
      | { Id.name = Simple "distinct"; ns = Term } -> Term.(builtin ~loc Distinct ())
      | { Id.name = Simple "ite"; ns = Term }   -> Term.(builtin ~loc Ite ())
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
