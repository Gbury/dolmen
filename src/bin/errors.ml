
(* Some helper functions *)
(* ************************************************************************ *)

let prelude (st : Loop.State.t) =
  match st.input_lang with
  | None -> "prompt> @?"
  | Some l ->
    Format.asprintf "(%s)# @?" (Dolmen_loop.Logic.string_of_language l)

let prelude_space st =
  String.make (String.length (prelude st) - 8) ' '

(* Location functions *)
(* ************************************************************************ *)

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
    Loop.State.error st "Time limit reached"

  | Loop.Pipeline.Out_of_space
  | Dolmen_loop.Typer.T.Typing_error (
      Dolmen_loop.Typer.T.Error (_, _, Dolmen_loop.Typer.T.Uncaught_exn (
          Loop.Pipeline.Out_of_space, _))) ->
    Format.pp_print_flush Format.std_formatter ();
    Loop.State.error st "Memory limit reached"

  (* Parsing errors *)
  | Dolmen.Std.Loc.Uncaught (loc, exn) ->
    let file = Dolmen_loop.State.input_file_loc st in
    let l = Dolmen.Std.Loc.loc file loc in
    if Dolmen_loop.State.is_interactive st then
      Format.eprintf "%s%a@\n"
        (if l.Dolmen.Std.Loc.start_line = 1 &&
            l.Dolmen.Std.Loc.stop_line = 1 then prelude_space st else "")
        Dolmen.Std.Loc.fmt_hint l;
    Loop.State.error ~loc:{ file; loc; } st "%s" (Printexc.to_string exn)
  | Dolmen.Std.Loc.Lexing_error (loc, msg) ->
    let file = Dolmen_loop.State.input_file_loc st in
    let l = Dolmen.Std.Loc.loc file loc in
    if Dolmen_loop.State.is_interactive st then
      Format.eprintf "%s%a@\n"
        (if l.Dolmen.Std.Loc.start_line = 1 &&
            l.Dolmen.Std.Loc.stop_line = 1 then prelude_space st else "")
        Dolmen.Std.Loc.fmt_hint l;
    Loop.State.error ~loc:{ file; loc; } st "Lexing error: invalid character '%s'" msg
  | Dolmen.Std.Loc.Syntax_error (loc, msg) ->
    let file = Dolmen_loop.State.input_file_loc st in
    let l = Dolmen.Std.Loc.loc file loc in
    if Dolmen_loop.State.is_interactive st then
      Format.eprintf "%s%a@\n"
        (if l.Dolmen.Std.Loc.start_line = 1 &&
            l.Dolmen.Std.Loc.stop_line = 1 then prelude_space st else "")
        Dolmen.Std.Loc.fmt_hint l;
    Loop.State.error ~loc: { file; loc; } st "%s@."
      (match msg with "" -> "Syntax error" | x -> x)


  (* Typing errors *)
  | Dolmen_loop.Typer.T.Typing_error (
      Dolmen_loop.Typer.T.Error (env, fragment, _err) as error) ->
    let loc = Dolmen_loop.Typer.T.fragment_loc env fragment in
    if st.context then
      Format.eprintf "@[<hv 2>While typing:@ @[<hov>%a@]@]@."
        Loop.Typer.print_fragment (env, fragment);
    Loop.State.error ~loc st "%a"
      Loop.Typer.report_error error

  (* State errors *)
  | Dolmen_loop.State.File_not_found (loc, dir, f) ->
    Loop.State.error ~loc st "File not found: '%s' in directory '%s'" f dir
  | Dolmen_loop.State.Input_lang_changed (l, l') ->
    Loop.State.error st "Input language changed from %s to %s (probably because of an include statement)"
      (Dolmen_loop.Logic.string_of_language l)
      (Dolmen_loop.Logic.string_of_language l')

  (* Internal Dolmen Expr errors *)
  | Dolmen.Std.Expr.Bad_ty_arity (c, l) ->
    let pp_sep fmt () = Format.fprintf fmt ";@ " in
    Loop.State.error st "@[<hv>Internal error: Bad arity for type constant '%a',@ which was provided arguments:@ [@[<hv>%a@]]@]"
      Dolmen.Std.Expr.Print.ty_const c (Format.pp_print_list ~pp_sep Dolmen.Std.Expr.Ty.print) l
  | Dolmen.Std.Expr.Bad_term_arity (c, tys, ts) ->
    let pp_sep fmt () = Format.fprintf fmt ";@ " in
    Loop.State.error st "@[<hv>Internal error: Bad arity for type constant '%a',@ which was provided arguments:@ [@[<hv>%a;@ %a@]]@]"
      Dolmen.Std.Expr.Print.term_const c
      (Format.pp_print_list ~pp_sep Dolmen.Std.Expr.Ty.print) tys
      (Format.pp_print_list ~pp_sep Dolmen.Std.Expr.Term.print) ts
  | Dolmen.Std.Expr.Type_already_defined c ->
    Loop.State.error st "@[<hv>Internal error: Type constant '%a' was already defined earlier,@ cannot re-define it.@]"
      Dolmen.Std.Expr.Print.id c

  | Dolmen.Std.Expr.Term.Wrong_type (t, ty) ->
    Loop.State.error st "@[<hv>Internal error: A term of type@ %a@ was expected but instead got a term of type@ %a@]"
      Dolmen.Std.Expr.Ty.print ty Dolmen.Std.Expr.Ty.print (Dolmen.Std.Expr.Term.ty t)

  (* File format auto-detect *)
  | Dolmen_loop.Logic.Extension_not_found ext ->
    Loop.State.error st "@[<hv>The following extension was not recognized: '%s'.@ %s" ext
      "Please use a recognised extension or specify an input language on the command line"

  (* Generic catch-all *)
  | e -> Loop.State.error st "@[<hv>Unhandled exception:@ %s@]" (Printexc.to_string e)


