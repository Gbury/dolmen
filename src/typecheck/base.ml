
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
  | `Not_indexed
  | `Unary of (string -> 'ret)
  | `Binary of (string -> string -> 'ret)
  | `Ternary of (string -> string -> string -> 'ret)
  | `Nary of int * (string list -> 'ret)
]

let parse_indexed h r f ~err ~k =
  let r_l = List.length r in
  match f h with
  | `Not_indexed -> k ()
  | `Unary f ->
    begin match r with
      | [x] -> f x
      | _ -> err h 1 r_l
    end
  | `Binary f ->
    begin match r with
      | [x; y] -> f x y
      | _ -> err h 2 r_l
    end
  | `Ternary f ->
    begin match r with
      | [x; y; z] -> f x y z
      | _ -> err h 3 r_l
    end
  | `Nary (n, f') ->
    if n = r_l then f' r else err h n r_l

let bad_ty_index_arity (type env ty)
    (module Type: Tff_intf.S with type env = env and type Ty.t = ty)
    env s n r_l =
  `Ty ( (), fun ast _args ->
      Type._error env (Ast ast)
        (Type.Bad_index_arity (s, n, r_l))
    )

let bad_term_index_arity (type env term)
    (module Type: Tff_intf.S with type env = env and type T.t = term)
    env s n r_l =
  `Term (`Total, fun ast _args ->
      Type._error env (Ast ast)
        (Type.Bad_index_arity (s, n, r_l))
    )


(* Low level helpers *)
(* ************************************************************************ *)

type ('env, 'args, 'ret) helper =
  (module Tff_intf.S with type env = 'env) ->
  'env -> Intf.symbol -> (Dolmen.Std.Term.t -> 'args -> 'ret) ->
  (Dolmen.Std.Term.t -> Dolmen.Std.Term.t list -> 'ret)

let make_op0
    (type env) (module Type: Tff_intf.S with type env = env)
    env op ret = fun ast args ->
  match args with
  | [] -> ret ast ()
  | _ ->
    Type._error env (Ast ast)
      (Type.Bad_op_arity (op, [0], List.length args))

let make_op1
    (type env) (module Type: Tff_intf.S with type env = env)
    env op ret = fun ast args ->
  match args with
  | [t1] -> ret ast t1
  | _ ->
    Type._error env (Ast ast)
      (Type.Bad_op_arity (op, [1], List.length args))

let make_op2
    (type env) (module Type: Tff_intf.S with type env = env)
    env op ret = fun ast args ->
  match args with
  | [t1; t2] -> ret ast (t1, t2)
  | _ ->
    Type._error env (Ast ast)
      (Type.Bad_op_arity (op, [2], List.length args))

let make_op3
    (type env) (module Type: Tff_intf.S with type env = env)
    env op ret = fun ast args ->
  match args with
  | [t1; t2; t3] -> ret ast (t1, t2, t3)
  | _ ->
    Type._error env (Ast ast)
      (Type.Bad_op_arity (op, [3], List.length args))

let make_op4
    (type env) (module Type: Tff_intf.S with type env = env)
    env op ret = fun ast args ->
  match args with
  | [t1; t2; t3; t4] -> ret ast (t1, t2, t3, t4)
  | _ ->
    Type._error env (Ast ast)
      (Type.Bad_op_arity (op, [4], List.length args))

let make_opn n
    (type env) (module Type: Tff_intf.S with type env = env)
    env op ret = fun ast args ->
  let l = List.length args in
  if l = n then
    ret ast args
  else begin
    Type._error env (Ast ast)
      (Type.Bad_op_arity (op, [n], l))
  end

let make_assoc
    (type env) (module Type: Tff_intf.S with type env = env)
    env op ret = fun ast args ->
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

let map_chain
    (type t) (module Type: Tff_intf.S with type T.t = t) mk args =
  let rec aux mk = function
    | [] -> assert false
    | [_] -> []
    | x :: ((y :: _) as r) -> mk x y :: aux mk r
  in
  match aux mk args with
  | [] -> assert false
  | [x] -> x
  | l -> Type.T._and l

(* High level helpers *)
(* ************************************************************************ *)

(* Nullary applications *)

let app0
    (type env) (module Type : Tff_intf.S with type env = env)
    ?(check=(fun _ -> ())) env symbol ret =
  make_op0 (module Type) env symbol (fun ast () -> check ast; ret)

let app0_ast
    (type env) (module Type : Tff_intf.S with type env = env)
    ?(check=(fun _ -> ())) env symbol mk =
  make_op0 (module Type) env symbol (fun ast () -> check ast; mk ast)


(* Unary applications *)

let ty_app1
    (type env) (type ty)
    (module Type : Tff_intf.S with type env = env and type Ty.t = ty)
    ?(check=(fun _ _ -> ())) env symbol mk =
  make_op1 (module Type) env symbol
    (fun ast t -> check ast t; mk (Type.parse_ty env t))

let ty_app1_ast
    (type env) (type ty)
    (module Type : Tff_intf.S with type env = env and type Ty.t = ty)
    ?(check=(fun _ _ -> ())) env symbol mk =
  make_op1 (module Type) env symbol
    (fun ast t -> check ast t; mk ast (Type.parse_ty env t))

let term_app1
    (type env) (type term)
    (module Type : Tff_intf.S with type env = env and type T.t = term)
    ?(check=(fun _ _ -> ())) env symbol mk =
  make_op1 (module Type) env symbol
    (fun ast t -> check ast t; mk (Type.parse_term env t))

let term_app1_ast
    (type env) (type term)
    (module Type : Tff_intf.S with type env = env and type T.t = term)
    ?(check=(fun _ _ -> ())) env symbol mk =
  make_op1 (module Type) env symbol
    (fun ast t -> check ast t; mk ast (Type.parse_term env t))


(* Binary applications *)

let ty_app2
    (type env) (type ty)
    (module Type : Tff_intf.S with type env = env and type Ty.t = ty)
    ?(check=(fun _ _ _ -> ())) env symbol mk =
  make_op2 (module Type) env symbol (fun ast (a, b) ->
      check ast a b; mk (Type.parse_ty env a) (Type.parse_ty env b))

let ty_app2_ast
    (type env) (type ty)
    (module Type : Tff_intf.S with type env = env and type Ty.t = ty)
    ?(check=(fun _ _ _ -> ())) env symbol mk =
  make_op2 (module Type) env symbol (fun ast (a, b) ->
      check ast a b; mk ast (Type.parse_ty env a) (Type.parse_ty env b))

let term_app2
    (type env) (type term)
    (module Type : Tff_intf.S with type env = env and type T.t = term)
    ?(check=(fun _ _ _ -> ())) env symbol mk =
  make_op2 (module Type) env symbol (fun ast (a, b) ->
      check ast a b; mk (Type.parse_term env a) (Type.parse_term env b))

let term_app2_ast
    (type env) (type term)
    (module Type : Tff_intf.S with type env = env and type T.t = term)
    ?(check=(fun _ _ _ -> ())) env symbol mk =
  make_op2 (module Type) env symbol (fun ast (a, b) ->
      check ast a b; mk ast (Type.parse_term env a) (Type.parse_term env b))


(* Ternary applications *)

let ty_app3
    (type env) (type ty)
    (module Type : Tff_intf.S with type env = env and type Ty.t = ty)
    ?(check=(fun _ _ _ _ -> ())) env symbol mk =
  make_op3 (module Type) env symbol (fun ast (a, b, c) ->
      check ast a b c;
      mk (Type.parse_ty env a) (Type.parse_ty env b) (Type.parse_ty env c))

let ty_app3_ast
    (type env) (type ty)
    (module Type : Tff_intf.S with type env = env and type Ty.t = ty)
    ?(check=(fun _ _ _ _ -> ())) env symbol mk =
  make_op3 (module Type) env symbol (fun ast (a, b, c) ->
      check ast a b c;
      mk ast (Type.parse_ty env a) (Type.parse_ty env b) (Type.parse_ty env c))

let term_app3
    (type env) (type term)
    (module Type : Tff_intf.S with type env = env and type T.t = term)
    ?(check=(fun _ _ _ _ -> ())) env symbol mk =
  make_op3 (module Type) env symbol (fun ast (a, b, c) ->
      check ast a b c;
      mk (Type.parse_term env a) (Type.parse_term env b) (Type.parse_term env c))

let term_app3_ast
    (type env) (type term)
    (module Type : Tff_intf.S with type env = env and type T.t = term)
    ?(check=(fun _ _ _ _ -> ())) env symbol mk =
  make_op3 (module Type) env symbol (fun ast (a, b, c) ->
      check ast a b c;
      mk ast (Type.parse_term env a) (Type.parse_term env b) (Type.parse_term env c))


(* Quaternary applications *)

let ty_app4
    (type env) (type ty)
    (module Type : Tff_intf.S with type env = env and type Ty.t = ty)
    ?(check=(fun _ _ _ _ _ -> ())) env symbol mk =
  make_op4 (module Type) env symbol (fun ast (a, b, c, d) ->
      check ast a b c d;
      mk (Type.parse_ty env a) (Type.parse_ty env b)
        (Type.parse_ty env c) (Type.parse_ty env d))

let ty_app4_ast
    (type env) (type ty)
    (module Type : Tff_intf.S with type env = env and type Ty.t = ty)
    ?(check=(fun _ _ _ _ _ -> ())) env symbol mk =
  make_op4 (module Type) env symbol (fun ast (a, b, c, d) ->
      check ast a b c d;
      mk ast (Type.parse_ty env a) (Type.parse_ty env b)
        (Type.parse_ty env c) (Type.parse_ty env d))

let term_app4
    (type env) (type term)
    (module Type : Tff_intf.S with type env = env and type T.t = term)
    ?(check=(fun _ _ _ _ _ -> ())) env symbol mk =
  make_op4 (module Type) env symbol (fun ast (a, b, c, d) ->
      check ast a b c d;
      mk (Type.parse_term env a) (Type.parse_term env b)
        (Type.parse_term env c) (Type.parse_term env d))

let term_app4_ast
    (type env) (type term)
    (module Type : Tff_intf.S with type env = env and type T.t = term)
    ?(check=(fun _ _ _ _ _ -> ())) env symbol mk =
  make_op4 (module Type) env symbol (fun ast (a, b, c, d) ->
      check ast a b c d;
      mk ast (Type.parse_term env a) (Type.parse_term env b)
        (Type.parse_term env c) (Type.parse_term env d))

(* N-ary application *)

let term_app_list
    (type env) (type term)
    (module Type : Tff_intf.S with type env = env and type T.t = term)
    ?(check=(fun _ -> ())) env _symbol mk = (fun _ast args ->
    List.iter check args;
    mk (List.map (Type.parse_term env) args)
  )

let term_app_list_ast
    (type env) (type term)
    (module Type : Tff_intf.S with type env = env and type T.t = term)
    ?(check=(fun _ -> ())) env _symbol mk = (fun ast args ->
    List.iter check args;
    mk ast (List.map (Type.parse_term env) args)
  )


(* Left associative applications *)

let term_app_left
    (type env) (type term)
    (module Type : Tff_intf.S with type env = env and type T.t = term)
    ?(check=(fun _ _ -> ())) env symbol mk =
  make_assoc (module Type) env symbol (fun ast l ->
      check ast l;
      fold_left_assoc mk (List.map (Type.parse_term env) l))

let term_app_left_ast
    (type env) (type term)
    (module Type : Tff_intf.S with type env = env and type T.t = term)
    ?(check=(fun _ _ -> ())) env symbol mk =
  make_assoc (module Type) env symbol (fun ast l ->
      check ast l;
      fold_left_assoc (mk ast) (List.map (Type.parse_term env) l))


(* Right associative applications *)

let term_app_right
    (type env) (type term)
    (module Type : Tff_intf.S with type env = env and type T.t = term)
    ?(check=(fun _ _ -> ())) env symbol mk =
  make_assoc (module Type) env symbol (fun ast l ->
      check ast l;
      fold_right_assoc mk (List.map (Type.parse_term env) l))

let term_app_right_ast
    (type env) (type term)
    (module Type : Tff_intf.S with type env = env and type T.t = term)
    ?(check=(fun _ _ -> ())) env symbol mk =
  make_assoc (module Type) env symbol (fun ast l ->
      check ast l;
      fold_right_assoc (mk ast) (List.map (Type.parse_term env) l))


(* Chained applications *)

let term_app_chain
    (type env) (type term)
    (module Type : Tff_intf.S with type env = env and type T.t = term)
    ?(check=(fun _ _ -> ())) env symbol mk =
  make_chain (module Type) env symbol (fun ast l ->
      check ast l;
      let l' = List.map (Type.parse_term env) l in
      map_chain (module Type) mk l'
    )

let term_app_chain_ast
    (type env) (type term)
    (module Type : Tff_intf.S with type env = env and type T.t = term)
    ?(check=(fun _ _ -> ())) env symbol mk =
  make_chain (module Type) env symbol (fun ast l ->
      check ast l;
      let l' = List.map (Type.parse_term env) l in
      map_chain (module Type) (mk ast) l'
    )

(* Higher-order application *)

let term_app_cst
    (type env) (type term) (type cst)
    (module Type : Tff_intf.S with type env = env and type T.t = term and type T.Const.t = cst)
    env cst = fun ast args ->
  Type.unwrap_term env ast (Type.parse_app_term_cst env ast cst args)

let term_app_ho
    (type env) (type term)
    (module Type : Thf_intf.S with type env = env and type T.t = term)
    env f = fun ast args ->
  Type.unwrap_term env ast (Type.parse_app_ho_term env ast f args)

let term_app_ho_ast
    (type env) (type term)
    (module Type : Thf_intf.S with type env = env and type T.t = term)
    env f = fun ast args ->
  Type.unwrap_term env ast (Type.parse_app_ho_term env ast (f ast) args)


