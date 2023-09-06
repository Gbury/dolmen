
(* MCIL transition systems *)

module MCIL (Type : Tff_intf.S) : sig
  
  val parse_def : 
    Type.env ->
    Dolmen.Std.Statement.sys_def ->
    [> `Sys_def of Dolmen.Std.Id.t * Type.T.Var.t list * Type.T.Var.t list * Type.T.Var.t list]
  (** Parse a transition system definition *)

  val parse_check : Type.env -> Dolmen.Std.Statement.sys_check -> [> `Sys_check ]
  (** Parse a transition system check *)

end