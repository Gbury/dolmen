
(* Merging builtin parser functions *)
(* ************************************************************************ *)

let noop _ _ = `Not_found

let rec merge l env s =
  match l with
  | [] -> `Not_found
  | f :: r ->
    begin match f env s with
      | `Not_found -> merge r env s
      | ret -> ret
    end

(* Smtlib ID splitting *)
(* ************************************************************************ *)

type 'ret indexed = [
  | `Unary of (string -> 'ret)
  | `Binary of (string -> string -> 'ret)
  | `Ternary of (string -> string -> string -> 'ret)
  | `Nary of int * (string list -> 'ret)
]

let parse_id id l ~err ~k =
  let rec aux h r r_l = function
    | [] -> k (h :: r)
    | (s, `Unary f) :: _ when String.equal s h ->
      begin match r with
        | [x] -> f x
        | _ -> err s 1 r_l
      end
    | (s, `Binary f) :: _ when String.equal s h ->
      begin match r with
        | [x; y] -> f x y
        | _ -> err s 2 r_l
      end
    | (s, `Ternary f) :: _ when String.equal s h ->
      begin match r with
        | [x; y; z] -> f x y z
        | _ -> err s 3 r_l
      end
    | (s, `Nary (n, f)) :: _ when String.equal s h ->
      if r_l = n then f r else err s n r_l
    | _ :: l' -> aux h r r_l l'
  in
  match Dolmen.Std.Id.split id with
  | h :: r -> aux h r (List.length r) l
  | r -> k r

let bad_ty_index_arity env s n r_l =
  `Ty (fun ast _args ->
      Type._error env (Ast ast)
        (Type.Bad_index_arity (s, n, r_l))
    )

let bad_term_index_arity env s n r_l =
  `Term (fun ast _args ->
      Type._error env (Ast ast)
        (Type.Bad_index_arity (s, n, r_l))
    )


(* Low level helpers *)
(* ************************************************************************ *)

type ('args, 'ret) helper =
  Type.env -> string -> (Dolmen.Std.Term.t -> 'args -> 'ret) ->
  (Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> 'ret)

let make_op0 env op ret = fun ast args ->
  match args with
  | [] -> ret ast ()
  | _ ->
    Type._error env (Ast ast)
      (Type.Bad_op_arity (op, [0], List.length args))

let make_op1 env op ret = fun ast args ->
  match args with
  | [t1] -> ret ast t1
  | _ ->
    Type._error env (Ast ast)
      (Type.Bad_op_arity (op, [1], List.length args))

let make_op2 env op ret = fun ast args ->
  match args with
  | [t1; t2] -> ret ast (t1, t2)
  | _ ->
    Type._error env (Ast ast)
      (Type.Bad_op_arity (op, [2], List.length args))

let make_op3 env op ret = fun ast args ->
  match args with
  | [t1; t2; t3] -> ret ast (t1, t2, t3)
  | _ ->
    Type._error env (Ast ast)
      (Type.Bad_op_arity (op, [3], List.length args))

let make_op4 env op ret = fun ast args ->
  match args with
  | [t1; t2; t3; t4] -> ret ast (t1, t2, t3, t4)
  | _ ->
    Type._error env (Ast ast)
      (Type.Bad_op_arity (op, [4], List.length args))

let make_opn n env op ret = fun ast args ->
  let l = List.length args in
  if l = n then
    ret ast args
  else begin
    Type._error env (Ast ast)
      (Type.Bad_op_arity (op, [n], l))
  end

let make_assoc env op ret = fun ast args ->
  match args with
  | [] | [_] ->
    Type._error env (Ast ast)
      (Type.Bad_op_arity (op, [2], List.length args))
  | _ -> ret ast args

let fold_left_assoc mk = function
  | h :: r -> List.fold_left mk h r
  | _ -> raise (Invalid_argument "Base.fold_left_assoc")

let rec fold_right_assoc mk = function
  | [x] -> x
  | h :: r -> mk h (fold_right_assoc mk r)
  | _ -> raise (Invalid_argument "Base.fold_right_assoc")

let make_chain = make_assoc

let map_chain mk args =
  let rec aux mk = function
    | [] -> assert false
    | [_] -> []
    | x :: ((y :: _) as r) -> mk x y :: aux mk r
  in
  match aux mk args with
  | [] -> assert false
  | [x] -> x
  | l -> Dolmen.Std.Expr.Term._and l

(* High level helpers *)
(* ************************************************************************ *)

(* Nullary applications *)

let app0 ?(check=(fun _ -> ())) env name ret =
  make_op0 env name (fun ast () -> check ast; ret)

let app0_ast ?(check=(fun _ -> ())) env name mk =
  make_op0 env name (fun ast () -> check ast; mk ast)


(* Unary applications *)

let ty_app1 ?(check=(fun _ _ -> ())) env name mk =
  make_op1 env name
    (fun ast t -> check ast t; mk (Type.parse_ty env t))

let ty_app1_ast ?(check=(fun _ _ -> ())) env name mk =
  make_op1 env name
    (fun ast t -> check ast t; mk ast (Type.parse_ty env t))

let term_app1 ?(check=(fun _ _ -> ())) env name mk =
  make_op1 env name
    (fun ast t -> check ast t; mk (Type.parse_term env t))

let term_app1_ast ?(check=(fun _ _ -> ())) env name mk =
  make_op1 env name
    (fun ast t -> check ast t; mk ast (Type.parse_term env t))


(* Binary applications *)

let ty_app2 ?(check=(fun _ _ _ -> ())) env name mk =
  make_op2 env name (fun ast (a, b) ->
      check ast a b; mk (Type.parse_ty env a) (Type.parse_ty env b))

let ty_app2_ast ?(check=(fun _ _ _ -> ())) env name mk =
  make_op2 env name (fun ast (a, b) ->
      check ast a b; mk ast (Type.parse_ty env a) (Type.parse_ty env b))

let term_app2 ?(check=(fun _ _ _ -> ())) env name mk =
  make_op2 env name (fun ast (a, b) ->
      check ast a b; mk (Type.parse_term env a) (Type.parse_term env b))

let term_app2_ast ?(check=(fun _ _ _ -> ())) env name mk =
  make_op2 env name (fun ast (a, b) ->
      check ast a b; mk ast (Type.parse_term env a) (Type.parse_term env b))


(* Ternary applications *)

let ty_app3 ?(check=(fun _ _ _ _ -> ())) env name mk =
  make_op3 env name (fun ast (a, b, c) ->
      check ast a b c;
      mk (Type.parse_ty env a) (Type.parse_ty env b) (Type.parse_ty env c))

let ty_app3_ast ?(check=(fun _ _ _ _ -> ())) env name mk =
  make_op3 env name (fun ast (a, b, c) ->
      check ast a b c;
      mk ast (Type.parse_ty env a) (Type.parse_ty env b) (Type.parse_ty env c))

let term_app3 ?(check=(fun _ _ _ _ -> ())) env name mk =
  make_op3 env name (fun ast (a, b, c) ->
      check ast a b c;
      mk (Type.parse_term env a) (Type.parse_term env b) (Type.parse_term env c))

let term_app3_ast ?(check=(fun _ _ _ _ -> ())) env name mk =
  make_op3 env name (fun ast (a, b, c) ->
      check ast a b c;
      mk ast (Type.parse_term env a) (Type.parse_term env b) (Type.parse_term env c))


(* Quaternary applications *)

let ty_app4 ?(check=(fun _ _ _ _ _ -> ())) env name mk =
  make_op4 env name (fun ast (a, b, c, d) ->
      check ast a b c d;
      mk (Type.parse_ty env a) (Type.parse_ty env b)
        (Type.parse_ty env c) (Type.parse_ty env d))

let ty_app4_ast ?(check=(fun _ _ _ _ _ -> ())) env name mk =
  make_op4 env name (fun ast (a, b, c, d) ->
      check ast a b c d;
      mk ast (Type.parse_ty env a) (Type.parse_ty env b)
        (Type.parse_ty env c) (Type.parse_ty env d))

let term_app4 ?(check=(fun _ _ _ _ _ -> ())) env name mk =
  make_op4 env name (fun ast (a, b, c, d) ->
      check ast a b c d;
      mk (Type.parse_term env a) (Type.parse_term env b)
        (Type.parse_term env c) (Type.parse_term env d))

let term_app4_ast ?(check=(fun _ _ _ _ _ -> ())) env name mk =
  make_op4 env name (fun ast (a, b, c, d) ->
      check ast a b c d;
      mk ast (Type.parse_term env a) (Type.parse_term env b)
        (Type.parse_term env c) (Type.parse_term env d))


(* Left associative applications *)

let term_app_left ?(check=(fun _ _ -> ())) env name mk =
  make_assoc env name (fun ast l ->
      check ast l;
      fold_left_assoc mk (List.map (Type.parse_term env) l))

let term_app_left_ast ?(check=(fun _ _ -> ())) env name mk =
  make_assoc env name (fun ast l ->
      check ast l;
      fold_left_assoc (mk ast) (List.map (Type.parse_term env) l))


(* Right associative applications *)

let term_app_right ?(check=(fun _ _ -> ())) env name mk =
  make_assoc env name (fun ast l ->
      check ast l;
      fold_right_assoc mk (List.map (Type.parse_term env) l))

let term_app_right_ast ?(check=(fun _ _ -> ())) env name mk =
  make_assoc env name (fun ast l ->
      check ast l;
      fold_right_assoc (mk ast) (List.map (Type.parse_term env) l))


(* Chained applications *)

let term_app_chain ?(check=(fun _ _ -> ())) env name mk =
  make_chain env name (fun ast l ->
      check ast l;
      let l' = List.map (Type.parse_term env) l in
      map_chain mk l'
    )

let term_app_chain_ast ?(check=(fun _ _ -> ())) env name mk =
  make_chain env name (fun ast l ->
      check ast l;
      let l' = List.map (Type.parse_term env) l in
      map_chain (mk ast) l'
    )


