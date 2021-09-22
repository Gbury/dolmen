
(* This file is free software, part of Dolmen. See file "LICENSE" for more details. *)

(* Parsing errors *)
(* ************************************************************************ *)

let extension_not_found =
  Report.Error.mk ~code:Code.generic ~mnemonic:"ext-unknown"
    ~message:(fun fmt ext ->
        Format.fprintf fmt
          "@[<hv>The following extension was not recognized: '%s'.@ %s" ext
          "Please use a recognised extension or specify an input language on the command line")
    ~name:"File extension unknown" ()

let file_not_found =
  Report.Error.mk ~code:Code.generic ~mnemonic:"file-not-found"
    ~message:(fun fmt (dir, f) ->
        if dir = "." then
          Format.fprintf fmt "File not found: '%s'" f
        else
          Format.fprintf fmt
            "File not found: '%s' in directory '%s'" f dir)
    ~name:"File not Found" ()

let input_lang_changed =
  Report.Error.mk ~code:Code.generic ~mnemonic:"input-lang-changed"
    ~message:(fun fmt (old_lang, new_lang) ->
        Format.fprintf fmt
          "Input language changed from %s to %s (probably because of an include statement)"
          (Logic.string_of_language old_lang)
          (Logic.string_of_language new_lang))
    ~name:"Input language change" ()

let lexing_error =
  Report.Error.mk ~code:Code.parsing ~mnemonic:"lexing-error"
    ~message:(fun fmt lex ->
        Format.fprintf fmt
          "Lexing error: invalid character '%s'" lex)
    ~name:"Lexing error" ()

let parsing_error =
  Report.Error.mk ~code:Code.parsing ~mnemonic:"parsing-error"
    ~message:(fun fmt perr ->
        match perr with
        | `Regular msg ->
          Format.fprintf fmt "%t" msg
        | `Advanced (prod, lexed, expected) ->
          Format.fprintf fmt
            "@[<v>@[<hv>while parsing %t,@ read %t,@]@ @[<hov>but expected %t.@]@]"
            prod lexed expected)
    ~name:"Parsing error" ()


(* Pipe functor *)
(* ************************************************************************ *)

module Pipe
    (Expr : Expr_intf.S)
    (State : State_intf.Parser_pipe
     with type term := Expr.term)
= struct

  (* Module alias & Helper functions *)
  (* ************************************************************************ *)

  module S = Dolmen.Std.Statement

  let set_lang ?loc st l =
    match State.input_lang st with
    | None -> State.set_lang st l
    | Some l' ->
      if l = l'
      then State.set_lang st l
      else State.error ?loc st input_lang_changed (l', l)

  (* Parsing *)
  (* ************************************************************************ *)

  let gen_finally (gen : 'a Gen.t) cl : 'a Gen.t =
    (* Register a finaliser for the original generator in case an exception is
       raised at some point in one of the pipes, which would prevent gen from
       reaching its end and thus prevent closing of the underlying file. *)
    let () = Gc.finalise_last cl gen in
    (* Return a new generator which wraps gen and calls the closing function
       once gen is finished. *)
    let aux () =
      match gen () with
      | Some _ as res -> res
      | None -> cl (); None
      | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        cl ();
        Printexc.raise_with_backtrace exn bt
    in
    aux

  let wrap_parser g = fun st ->
    if State.is_interactive st then
      Format.printf "%s @?" (State.prelude st);
    match g () with
    | ret -> st, ret
    | exception Dolmen.Std.Loc.Uncaught (loc, exn, bt) ->
      let file = State.input_file_loc st in
      let st =
        State.error st ~loc:{ file; loc; } Report.Error.uncaught_exn (exn, bt)
      in
      st, None
    | exception Dolmen.Std.Loc.Lexing_error (loc, lex) ->
      let file = State.input_file_loc st in
      let st = State.error st ~loc:{ file; loc; } lexing_error lex in
      st, None
    | exception Dolmen.Std.Loc.Syntax_error (loc, perr) ->
      let file = State.input_file_loc st in
      let st = State.error st ~loc:{ file; loc; } parsing_error perr in
      st, None

  let parse prelude st =
    (* Parse the input *)
    let st', g =
      try
        match State.input_source st with
        | `Stdin ->
          let lang, file_loc, gen, _ = Logic.parse_input
              ?language:(State.input_lang st)
              (`Stdin (Logic.Smtlib2 `Latest))
          in
          let st = State.set_input_file_loc st file_loc in
          let st = set_lang st lang in
          st, gen
        | `Raw (filename, contents) ->
          let lang =
            match State.input_lang st with
            | Some l -> l
            | None ->
              let res, _, _ = Logic.of_filename filename in
              res
          in
          let lang, file_loc, gen, cl = Logic.parse_input
              ~language:lang (`Raw (filename, lang, contents)) in
          let st = State.set_input_file_loc st file_loc in
          let st = set_lang st lang in
          st, gen_finally gen cl
        | `File f ->
          let s = Dolmen.Std.Statement.include_ f [] in
          (* Auto-detect input format *)
          let lang =
            match State.input_lang st with
            | Some l -> l
            | None ->
              let res, _, _ = Logic.of_filename f in
              res
          in
          (* Formats Dimacs and Tptp are descriptive and lack the emission
              of formal solve/prove instructions, so we need to add them. *)
          let s' =
            match lang with
            | Logic.Zf
            | Logic.ICNF
            | Logic.Smtlib2 _
            | Logic.Alt_ergo -> s
            | Logic.Dimacs
            | Logic.Tptp _ ->
              Dolmen.Std.Statement.pack [s; Dolmen.Std.Statement.prove ()]
          in
          set_lang st lang,
          (Gen.singleton s')
      with
      | Logic.Extension_not_found ext ->
        State.error st extension_not_found ext, Gen.empty
    in
    (* Wrap the resulting parser *)
    st', wrap_parser (Gen.append (Gen.of_list prelude) g)

  (* Expand dolmen statements *)
  (* ************************************************************************ *)

  let merge _ st = st

  let gen_of_llist l =
    let l = ref l in
    (fun () -> match Lazy.force !l with
       | [] -> None
       | x :: r ->
         l := (lazy r); Some x
    )

  let expand st c =
    let ret = match c with
      | { S.descr = S.Pack l; _ } ->
        st, `Gen (merge, wrap_parser (Gen.of_list l))
      (* TODO: filter the statements by passing some stions *)
      | { S.descr = S.Include file; _ } ->
        let loc = { Dolmen.Std.Loc.file = State.input_file_loc st; loc = c.loc; } in
        let language = State.input_lang st in
        let dir = State.input_dir st in
        begin
          match Logic.find ?language ~dir file with
          | None ->
            State.error ~loc st file_not_found (dir, file), `Ok
          | Some file ->
            begin match State.input_mode st with
              | None
              | Some `Incremental ->
                let lang, file_loc, gen, cl = Logic.parse_input ?language (`File file) in
                let st = State.set_input_file_loc st file_loc in
                let st = set_lang ~loc st lang in
                st, `Gen (merge, wrap_parser (gen_finally gen cl))
              | Some `Full ->
                let lang, file_loc, l = Logic.parse_file_lazy ?language file in
                let st = State.set_input_file_loc st file_loc in
                let st = set_lang ~loc st lang in
                st, `Gen (merge, wrap_parser (gen_of_llist l))
            end
        end
      | _ -> (st, `Ok)
    in
    ret

end

