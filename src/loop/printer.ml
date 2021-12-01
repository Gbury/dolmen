
(* This file is free software, part of Dolmen. See file "LICENSE" for more details. *)

(* Printing errors *)
(* ************************************************************************ *)



(* Pipe functor *)
(* ************************************************************************ *)

module Pipe
    (Expr : Expr_intf.S)
    (State : State_intf.Parser_pipe
     with type term := Expr.term)
= struct

end
