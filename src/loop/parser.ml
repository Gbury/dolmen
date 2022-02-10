
(* This file is free software, part of Dolmen. See file "LICENSE" for more details. *)

(* Parsing errors *)
(* ************************************************************************ *)

let full_mode_switch =
  Report.Warning.mk ~code:Code.generic ~mnemonic:"full-mode-switch"
    ~message:(fun fmt lang ->
        Format.fprintf fmt
          "The@ %s@ format@ does@ not@ support@ \
           incremental@ mode,@ switching@ to@ full@ mode"
          lang)
    ~name:"Forced switch to full mode" ()

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

module type Pipe_res = Parser_intf.Pipe_res

module Pipe
    (Expr : Expr_intf.S)
    (State : State_intf.Parser_pipe
     with type term := Expr.term)
= struct

  (* Module & type aliases *)
  (* ************************************************************************ *)

  type 'a file = 'a State_intf.file

  module S = Dolmen.Std.Statement


  (* Helper functions *)
  (* ************************************************************************ *)

  let gen_of_llist l =
    let l = ref l in
    (fun () -> match Lazy.force !l with
       | [] -> None
       | x :: r ->
         l := (lazy r); Some x
    )

  let switch_to_full_mode ?loc lang st (old : _ file) (file: _ file) =
    let st =
      match old.mode with
      | Some `Incremental -> State.warn ?loc st full_mode_switch lang
      | _ -> st
    in
    State.set_logic_file st { file with mode = Some `Full; }

  let set_logic_file ?loc st old (file : _ file) =
    match file.lang with
    | Some Logic.Alt_ergo -> switch_to_full_mode ?loc "Alt-Ergo" st old file
    | _ -> State.set_logic_file st file

  let set_logic_file ?loc st (new_file : _ file) =
    let old_file = State.logic_file st in
    match old_file.lang with
    | None -> State.set_logic_file st new_file
    | Some l ->
      begin match new_file.lang with
        | None -> State.set_logic_file st new_file
        | Some l' ->
          if l = l'
          then set_logic_file ?loc st old_file new_file
          else State.error ~file:new_file ?loc st input_lang_changed (l', l)
      end

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

  let wrap_parser ~file g = fun st ->
    if State.is_interactive st then
      Format.printf "%s @?" (State.prelude st);
    match g () with
    | ret -> st, ret
    | exception Dolmen.Std.Loc.Uncaught (loc, exn, bt) ->
      let st =
        State.error st ~file ~loc:{ file = file.loc; loc; }
          Report.Error.uncaught_exn (exn, bt)
      in
      st, None
    | exception Dolmen.Std.Loc.Lexing_error (loc, lex) ->
      let st = State.error st ~file ~loc:{ file = file.loc; loc; } lexing_error lex in
      st, None
    | exception Dolmen.Std.Loc.Syntax_error (loc, perr) ->
      let st = State.error st ~file ~loc:{ file = file.loc; loc; } parsing_error perr in
      st, None

  let parse_logic prelude st (file : Logic.language file) =
    (* Parse the input *)
    let st, file, g =
      try
        match file.source with
        | `Stdin ->
          let lang, file_loc, gen, _ = Logic.parse_input
              ?language:file.lang (`Stdin (Logic.Smtlib2 `Latest))
          in
          let file = { file with loc = file_loc; lang = Some lang; } in
          st, file, gen
        | `Raw (filename, contents) ->
          let lang =
            match file.lang with
            | Some l -> l
            | None ->
              let res, _, _ = Logic.of_filename filename in
              res
          in
          let lang, file_loc, gen, cl = Logic.parse_input
              ~language:lang (`Raw (filename, lang, contents)) in
          let file = { file with loc = file_loc; lang = Some lang; } in
          st, file, gen_finally gen cl
        | `File f ->
          let s = Dolmen.Std.Statement.include_ f [] in
          (* Auto-detect input format *)
          let lang =
            match file.lang with
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
          let file = { file with lang = Some lang; } in
          st, file, (Gen.singleton s')
      with
      | Logic.Extension_not_found ext ->
        State.error st extension_not_found ext, file, Gen.empty
    in
    let st = set_logic_file st file in
    (* Wrap the resulting parser *)
    st, wrap_parser ~file (Gen.append (Gen.of_list prelude) g)

  let parse_response prelude st (file : Response.language file) =
    (* Parse the input *)
    let st, file, g =
      try
        match file.source with
        | `Stdin ->
          let lang, file_loc, gen, _ = Response.parse_input
              ?language:file.lang (`Stdin (Response.Smtlib2 `Latest))
          in
          let file = { file with loc = file_loc; lang = Some lang; } in
          st, file, gen
        | `Raw (filename, contents) ->
          let lang =
            match file.lang with
            | Some l -> l
            | None ->
              let res, _, _ = Response.of_filename filename in
              res
          in
          let lang, file_loc, gen, cl = Response.parse_input
              ~language:lang (`Raw (filename, lang, contents)) in
          let file = { file with loc = file_loc; lang = Some lang; } in
          st, file, gen_finally gen cl
        | `File f ->
          begin match Response.find ?language:file.lang ~dir:file.dir f with
          | None ->
            let st = State.error st file_not_found (file.dir, f) in
            st, file, Gen.empty
            | Some filename ->
              begin match file.mode with
                | None
                | Some `Incremental ->
                  let lang, file_loc, gen, cl =
                    Response.parse_input ?language:file.lang (`File filename)
                  in
                  let file = { file with loc = file_loc; lang = Some lang; } in
                  st, file, gen_finally gen cl
                | Some `Full ->
                  let lang, file_loc, l =
                    Response.parse_file_lazy ?language:file.lang filename
                  in
                  let file = { file with loc = file_loc; lang = Some lang; } in
                  st, file, gen_of_llist l
              end
          end
      with
      | Logic.Extension_not_found ext ->
        State.error st extension_not_found ext, file, Gen.empty
    in
    let st = State.set_response_file st file in
    (* Wrap the resulting parser *)
    st, wrap_parser ~file (Gen.append (Gen.of_list prelude) g)

  (* Expand dolmen statements *)
  (* ************************************************************************ *)

  let merge _ st = st

  let expand st c =
    let ret = match c with
      | { S.descr = S.Pack l; _ } ->
        let file = State.logic_file st in
        st, `Gen (merge, wrap_parser ~file (Gen.of_list l))
      (* TODO: filter the statements by passing some options *)
      | { S.descr = S.Include file; _ } ->
        let logic_file = State.logic_file st in
        let loc = { Dolmen.Std.Loc.file = logic_file.loc; loc = c.loc; } in
        let language = logic_file.lang in
        let dir = logic_file.dir in
        begin
          match Logic.find ?language ~dir file with
          | None ->
            State.error ~loc st file_not_found (dir, file), `Ok
          | Some file ->
            begin match logic_file.mode with
              | None
              | Some `Incremental ->
                let lang, file_loc, gen, cl = Logic.parse_input ?language (`File file) in
                let new_logic_file = { logic_file with loc = file_loc; lang = Some lang; } in
                let st = set_logic_file st new_logic_file in
                st, `Gen (merge, wrap_parser ~file:new_logic_file (gen_finally gen cl))
              | Some `Full ->
                let lang, file_loc, l = Logic.parse_file_lazy ?language file in
                let new_logic_file = { logic_file with loc = file_loc; lang = Some lang; } in
                let st = set_logic_file st new_logic_file in
                st, `Gen (merge, wrap_parser ~file:new_logic_file (gen_of_llist l))
            end
        end
      | _ -> (st, `Ok)
    in
    ret

end

