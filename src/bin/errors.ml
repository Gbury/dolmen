
(* Exceptions *)
(* ************************************************************************* *)

let exn st bt = function

  (* Internal exception used for jumping.
     This should only be used ot raise a state that has just been
     'errored' (i.e. where State.error has been called on the state),
     which, for the cli binary, means that we should have already exited. *)
  | Dolmen_loop.State.Error st -> st

  (* Sigint, potentially wrapped by the typechecker *)
  | Dolmen_loop.Pipeline.Sigint ->
    Format.pp_print_flush Format.std_formatter ();
    Format.pp_print_flush Format.err_formatter ();
    Loop.State.error st Dolmen_loop.Report.Error.user_interrupt ()

  (* Timeout, potentially wrapped by the typechecker *)
  | Dolmen_loop.Alarm.Out_of_time ->
    Format.pp_print_flush Format.std_formatter ();
    Loop.State.error st Dolmen_loop.Report.Error.timeout ()

  | Dolmen_loop.Alarm.Out_of_space ->
    Format.pp_print_flush Format.std_formatter ();
    Format.pp_print_flush Format.err_formatter ();
    Loop.State.error st Dolmen_loop.Report.Error.spaceout ()

  (* Internal Dolmen Expr errors *)
  | Dolmen.Std.Expr.Ty.Bad_arity (c, l) ->
    let pp_sep fmt () = Format.fprintf fmt ";@ " in
    Loop.State.error st Dolmen_loop.Report.Error.internal_error
      (Format.dprintf
         "@[<hv>Internal error: Bad arity for type constant '%a',\
          @ which was provided arguments:@ [@[<hv>%a@]]@]"
         Dolmen.Std.Expr.Print.ty_cst c
         (Format.pp_print_list ~pp_sep Dolmen.Std.Expr.Ty.print) l)
  | Dolmen.Std.Expr.Type_already_defined c ->
    Loop.State.error st Dolmen_loop.Report.Error.internal_error
      (Format.dprintf
         "@[<hv>Internal error: Type constant '%a' was already defined earlier,\
          @ cannot re-define it.@]"
         Dolmen.Std.Expr.Print.id c)
  | Dolmen.Std.Expr.Term.Wrong_type (t, ty) ->
    Loop.State.error st Dolmen_loop.Report.Error.internal_error
      (Format.dprintf
         "@[<hv>Internal error: A term of type@ %a@ was expected \
          but instead got a term of type@ %a@]"
         Dolmen.Std.Expr.Ty.print ty
         Dolmen.Std.Expr.Ty.print (Dolmen.Std.Expr.Term.ty t))

  (* Generic catch-all *)
  | exn ->
    Loop.State.error st Dolmen_loop.Report.Error.uncaught_exn (exn, bt)

