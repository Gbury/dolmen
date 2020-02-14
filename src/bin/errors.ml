
(* Some helper functions *)
(* ************************************************************************ *)

module State = Bin_state

let prelude (st : State.t) =
  match st.input_lang with
  | None -> "prompt> @?"
  | Some l ->
    Format.asprintf "(%s)# @?" (Dolmen_loop.Parser.string_of_language l)

let prelude_space st =
  String.make (String.length (prelude st) - 8) ' '

(* Location functions *)
(* ************************************************************************ *)

let default_loc (st : State.t) =
  Dolmen.ParseLocation.mk
    (Options.input_to_string st.input_source) 0 0 0 0

let get_loc st = function
  | Some l -> l
  | None -> default_loc st

(* Exceptions *)
(* ************************************************************************* *)

let sigint st = State.error st "User Interrupt"
let out_of_time st = State.error st "Time limit reached"
let out_of_space st = State.error st "Memory limit reached"

let exn st = function
  (* Parsing errors *)
  | Dolmen.ParseLocation.Uncaught (loc, exn) ->
    if Dolmen.State.is_interactive st then
      Format.eprintf "%s%a@\n"
        (if Dolmen.ParseLocation.(loc.start_line = 1) then prelude_space st else "")
        Dolmen.ParseLocation.fmt_hint loc;
    State.error st "%a:@\n%s@."
      Dolmen.ParseLocation.fmt loc (Printexc.to_string exn)
  | Dolmen.ParseLocation.Lexing_error (loc, msg) ->
    if Dolmen.State.is_interactive st then
      Format.eprintf "%s%a@\n"
        (if Dolmen.ParseLocation.(loc.start_line = 1) then prelude_space st else "")
        Dolmen.ParseLocation.fmt_hint loc;
    State.error st "%a:@\nLexing error: invalid character '%s'@."
      Dolmen.ParseLocation.fmt loc msg
  | Dolmen.ParseLocation.Syntax_error (loc, msg) ->
    if Dolmen.State.is_interactive st then
      Format.eprintf "%s%a@\n"
        (if Dolmen.ParseLocation.(loc.start_line = 1) then prelude_space st else "")
        Dolmen.ParseLocation.fmt_hint loc;
    State.error st "%a:@\n%s@."
      Dolmen.ParseLocation.fmt loc
      (match msg with "" -> "Syntax error" | x -> x)

  (* Typing errors *)
  | Typer.T.Typing_error (err, _, t) ->
    let loc = get_loc st t.Dolmen.Term.loc in
    if Dolmen.State.is_interactive st then
      Format.eprintf "%s%a@\n"
        (if Dolmen.ParseLocation.(loc.start_line = 1) then prelude_space st else "")
        Dolmen.ParseLocation.fmt_hint loc;
    State.error st
      "@[<hv>While typing:@ @[<hov>%a@]@]@.%a:@\n@[<hv>%a@]"
      Dolmen.Term.print t
      Dolmen.ParseLocation.fmt loc
      Typer.report_error err

  (* State errors *)
  | Dolmen_loop.State.File_not_found (None, dir, f) ->
    State.error st "File not found: '%s' in directory '%s'" f dir
  | Dolmen_loop.State.File_not_found (Some loc, dir, f) ->
    State.error st "@[<v>%a:@ File not found: '%s' in directory '%s'@]"
      Dolmen.ParseLocation.fmt loc f dir
  | Dolmen_loop.State.Missing_smtlib_logic ->
    State.error st "Missing smtlib set-logic statement"
  | Dolmen_loop.State.Input_lang_changed (l, l') ->
    State.error st "Input language changed from %s to %s (probably because of an include statement)"
      (Dolmen_loop.Parser.string_of_language l)
      (Dolmen_loop.Parser.string_of_language l')

  (* Internal Dolmen Expr errors *)
  | Dolmen.Expr.Bad_ty_arity (c, l) ->
    let pp_sep fmt () = Format.fprintf fmt ";@ " in
    State.error st "@[<hv>Bad arity for type constant '%a', which was provided arguments:@ [@[<hv>%a@]]@]"
      Dolmen.Expr.Print.ty_const c (Format.pp_print_list ~pp_sep Dolmen.Expr.Ty.print) l
  | Dolmen.Expr.Bad_term_arity (c, tys, ts) ->
    let pp_sep fmt () = Format.fprintf fmt ";@ " in
    State.error st "@[<hv>Bad arity for type constant '%a', which was provided arguments:@ [@[<hv>%a;@ %a@]]@]"
      Dolmen.Expr.Print.term_const c
      (Format.pp_print_list ~pp_sep Dolmen.Expr.Ty.print) tys
      (Format.pp_print_list ~pp_sep Dolmen.Expr.Term.print) ts
  | Dolmen.Expr.Type_already_defined c ->
    State.error st "Type constant '%a' was already defined earlier, cannot re-define it."
      Dolmen.Expr.Print.id c
  | Dolmen.Expr.Term.Wrong_type (t, ty) ->
    State.error st "@[<hv>A term of type@ %a@ was expected but instead got a term of type@ %a@]"
      Dolmen.Expr.Ty.print ty Dolmen.Expr.Ty.print (Dolmen.Expr.Term.ty t)

  (* File format auto-detect *)
  | Dolmen_loop.Parser.Extension_not_found ext ->
    State.error st "@[<hv>The following extension was not recognized: '%s'.@ %s" ext
      "Please use a recognised extension or specify an input language on the command line"

  (* Generic catch-all *)
  | e -> State.error st "@[<hv>Unhandled exception:@ %s@]" (Printexc.to_string e)


