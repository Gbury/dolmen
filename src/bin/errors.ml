
(* Exceptions *)
(* ************************************************************************* *)

let exn st = function

  (* Sigint, potentially wrapped by the typechecker *)
  | Loop.Pipeline.Sigint
  | Dolmen_loop.Typer.T.Typing_error (
      Dolmen_loop.Typer.T.Error (_, _, Dolmen_loop.Typer.T.Uncaught_exn (
          Loop.Pipeline.Sigint, _))) ->
    Format.pp_print_flush Format.std_formatter ();
    Loop.State.error st "User Interrupt"

  (* Timeout, potentially wrapped by the typechecker *)
  | Loop.Pipeline.Out_of_time
  | Dolmen_loop.Typer.T.Typing_error (
      Dolmen_loop.Typer.T.Error (_, _, Dolmen_loop.Typer.T.Uncaught_exn (
          Loop.Pipeline.Out_of_time, _))) ->
    Format.pp_print_flush Format.std_formatter ();
    Loop.State.error ~code:Dolmen_loop.Code.limit st "Time limit reached"

  | Loop.Pipeline.Out_of_space
  | Dolmen_loop.Typer.T.Typing_error (
      Dolmen_loop.Typer.T.Error (_, _, Dolmen_loop.Typer.T.Uncaught_exn (
          Loop.Pipeline.Out_of_space, _))) ->
    Format.pp_print_flush Format.std_formatter ();
    Loop.State.error ~code:Dolmen_loop.Code.limit st "Memory limit reached"

  (* Parsing errors *)
  | Dolmen.Std.Loc.Uncaught (loc, exn, bt) ->
    let file = Dolmen_loop.State.input_file_loc st in
    Loop.State.error ~loc:{ file; loc; } st
      ~code:Dolmen_loop.Code.bug
      "%s%a%s"
      (Printexc.to_string exn)
      (if Printexc.backtrace_status ()
       then Format.pp_print_newline
       else (fun _ _ -> ())) ()
      (if Printexc.backtrace_status ()
       then Printexc.raw_backtrace_to_string bt
       else "")

  | Dolmen.Std.Loc.Lexing_error (loc, lex) ->
    let file = Dolmen_loop.State.input_file_loc st in
    Loop.State.error ~loc:{ file; loc; } st
      ~code:Dolmen_loop.Code.parsing
      "Lexing error: invalid character '%s'" lex
  | Dolmen.Std.Loc.Syntax_error (loc, `Regular msg) ->
    let file = Dolmen_loop.State.input_file_loc st in
    Loop.State.error ~loc: { file; loc; } st
      ~code:Dolmen_loop.Code.parsing
      "%t@." msg
  | Dolmen.Std.Loc.Syntax_error (loc, `Advanced (prod, lexed, expected)) ->
    let file = Dolmen_loop.State.input_file_loc st in
    Loop.State.error ~loc: { file; loc; } st
      ~code:Dolmen_loop.Code.parsing
      "@[<v>@[<hv>while parsing %t,@ read %t,@]@ @[<hov>but expected %t.@]@]"
      prod lexed expected

  (* Typing errors *)
  | Dolmen_loop.Typer.T.Typing_error (
      Dolmen_loop.Typer.T.Error (env, fragment, err) as error) ->
    let code =
      match err with
      | Dolmen_loop.Typer.T.Uncaught_exn _
      | Dolmen_loop.Typer.T.Unhandled_ast ->
        Dolmen_loop.Code.bug
      | _ -> Dolmen_loop.Code.typing
    in
    let loc = Dolmen_loop.Typer.T.fragment_loc env fragment in
    if st.context then
      Format.eprintf "@[<hv 2>While typing:@ @[<hov>%a@]@]@."
        Loop.Typer.print_fragment (env, fragment);
    Loop.State.error ~loc st "%a" ~code
      Loop.Typer.report_error error

  (* State errors *)
  | Dolmen_loop.State.File_not_found (loc, dir, f) ->
    if dir = "." then
      Loop.State.error ~loc st
        ~code:Dolmen_loop.Code.generic
        "File not found: '%s'" f
    else
      Loop.State.error ~loc st
        ~code:Dolmen_loop.Code.generic
        "File not found: '%s' in directory '%s'" f dir

  | Dolmen_loop.State.Input_lang_changed (l, l') ->
    Loop.State.error st
      ~code:Dolmen_loop.Code.generic
      "Input language changed from %s to %s (probably because of an include statement)"
      (Dolmen_loop.Logic.string_of_language l)
      (Dolmen_loop.Logic.string_of_language l')

  (* Internal Dolmen Expr errors *)
  | Dolmen.Std.Expr.Bad_ty_arity (c, l) ->
    let pp_sep fmt () = Format.fprintf fmt ";@ " in
    Loop.State.error st ~code:Dolmen_loop.Code.bug
      "@[<hv>Internal error: Bad arity for type constant '%a',\
       @ which was provided arguments:@ [@[<hv>%a@]]@]"
      Dolmen.Std.Expr.Print.ty_const c (Format.pp_print_list ~pp_sep Dolmen.Std.Expr.Ty.print) l
  | Dolmen.Std.Expr.Bad_term_arity (c, tys, ts) ->
    let pp_sep fmt () = Format.fprintf fmt ";@ " in
    Loop.State.error st ~code:Dolmen_loop.Code.bug
      "@[<hv>Internal error: Bad arity for type constant '%a',\
       @ which was provided arguments:@ [@[<hv>%a;@ %a@]]@]"
      Dolmen.Std.Expr.Print.term_const c
      (Format.pp_print_list ~pp_sep Dolmen.Std.Expr.Ty.print) tys
      (Format.pp_print_list ~pp_sep Dolmen.Std.Expr.Term.print) ts
  | Dolmen.Std.Expr.Type_already_defined c ->
    Loop.State.error st ~code:Dolmen_loop.Code.bug
      "@[<hv>Internal error: Type constant '%a' was already defined earlier,\
       @ cannot re-define it.@]"
      Dolmen.Std.Expr.Print.id c

  | Dolmen.Std.Expr.Term.Wrong_type (t, ty) ->
    Loop.State.error st ~code:Dolmen_loop.Code.bug
      "@[<hv>Internal error: A term of type@ %a@ was expected \
       but instead got a term of type@ %a@]"
      Dolmen.Std.Expr.Ty.print ty Dolmen.Std.Expr.Ty.print (Dolmen.Std.Expr.Term.ty t)

  (* File format auto-detect *)
  | Dolmen_loop.Logic.Extension_not_found ext ->
    Loop.State.error st ~code:Dolmen_loop.Code.generic
      "@[<hv>The following extension was not recognized: '%s'.@ %s" ext
      "Please use a recognised extension or specify an input language on the command line"

  (* Generic catch-all *)
  | e ->
    Loop.State.error st ~code:Dolmen_loop.Code.bug
      "@[<hv>Unhandled exception:@ %s@]" (Printexc.to_string e)


