
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Extended views *)
(* ************************************************************************* *)

module Extended(V : Dolmen_intf.View.FO.S) = struct

  let rec left_assoc top_head = function
    | [] -> []
    | (t :: r) as args ->
      begin match V.Term.view t with
        | App(h, _, h_args) when top_head h ->
          left_assoc top_head (h_args @ r)
        | _ -> args
      end

  let right_assoc top_head args =
    let rec aux top_head = function
      | [] -> []
      | (t :: r) as rev_args ->
        begin match V.Term.view t with
          | App(h, _, h_args) when top_head h ->
            aux top_head (List.rev_append h_args r)
          | _ -> List.rev rev_args
        end
    in
    aux top_head (List.rev args)

  let chainable top_head args =
    let rec aux top_head acc = function
      | [] -> Some (List.rev acc)
      | t :: r ->
        begin match V.Term.view t with
          | App(h, _, [a; b]) when top_head h ->
            begin match acc with
              | [] -> aux top_head [b; a] r
              | x :: _ ->
                if V.Term.equal x a
                then aux top_head (b :: acc) r
                else None
            end
          | _ -> None
        end
    in
    aux top_head [] args

end
