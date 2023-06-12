
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
    ~message:(fun fmt (p_error_ref, perr) ->
        match perr with
        | `Regular msg ->
          Format.fprintf fmt "%t" msg
        | `Advanced (error_ref, prod, lexed, expected) ->
          let p_ref fmt = if p_error_ref then Format.fprintf fmt "(%s)@ " error_ref in
          Format.fprintf fmt
            "@[<v>@[<hv>%twhile parsing %t,@ read %t,@]@ @[<hov>but expected %t.@]@]"
            p_ref prod lexed expected)
    ~name:"Parsing error" ()

let stdin_prelude =
  Report.Error.mk ~code:Code.parsing ~mnemonic:"stdin-prelude"
    ~message:(fun fmt () ->
      Format.fprintf fmt
        "Standard input can't be used as a prelude.")
    ~name:"Prelude error" ()

(* Pipe functor *)
(* ************************************************************************ *)

type 'a file = 'a State.file

module type S = Parser_intf.S

module Make(State : State.S) = struct

  (* Prologue *)
  (* ************************************************************************ *)

  type nonrec 'a file = 'a file

  module S = Dolmen.Std.Statement

  let pipe = "Parser"
  let syntax_error_ref = State.create_key ~pipe "syntax_error_ref"
  let interactive_prompt = State.create_key ~pipe "interactive_prompt"

  let interactive_prompt_default _ = None

  let interactive_prompt_lang st =
    match State.get State.logic_file st with
    | { source = `Stdin; _ } ->
      begin match (State.get State.logic_file st).lang with
        | None -> Some "prompt> @?"
        | Some l ->
          Some (Format.asprintf "(%s)# @?" (Logic.string_of_language l))
      end
    | _ -> None

  let init
      ?syntax_error_ref:(syntax_error_ref_value=false)
      ?interactive_prompt:(interactive_prompt_value=interactive_prompt_default)
    = fun st ->
      st
      |> State.set syntax_error_ref syntax_error_ref_value
      |> State.set interactive_prompt interactive_prompt_value

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
    st, { file with mode = Some `Full }

  let switch_to_full_mode_if_needed ?loc st old (file : _ file) =
    match file.lang with
    | Some Logic.Alt_ergo -> switch_to_full_mode ?loc "Alt-Ergo" st old file
    | _ -> st, file

  let set_logic_file ?loc st (file : _ file) =
    let old = State.get State.logic_file st in
    match old.lang, file.lang with
    | Some l, Some l' when l <> l' ->
      State.error ~file ?loc st input_lang_changed (l', l)
    | _ ->
      let st, new_file = switch_to_full_mode_if_needed ?loc st old file in
      State.set State.logic_file new_file st

  (* Parsing *)
  (* ************************************************************************ *)

  let gen_finally (gen : 'a Gen.t) cl : 'a Gen.t =
    (* Register a finaliser for the original generator in case an exception is
       raised at some point in one of the pipes, which would prevent gen from
       reaching its end and thus prevent closing of the underlying file. *)
    (* For now, we use `Gc.finalise, to avoid a bug in the implementation of
       `Gc.finalise_last` in the ocaml 5.0 runtime (see
       https://github.com/ocaml/ocaml/pull/12001). There should not be any
       difference between `finalise` and `finalise_last` as long as:
       1- the finaliser funciton does not keep the finalised value alive
       2- we do not use ephemerons

       Unfortunately, that means that finalisers will be called later than
       with `finalise_last`, but that should not be too much of a problem. *)
    let () = Gc.finalise (fun _ -> cl ()) gen in
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
    begin match (State.get interactive_prompt st) st with
      | None -> ()
      | Some prelude -> Format.printf "%s @?" prelude
    end;
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
      let syntax_error_ref = State.get_or ~default:false syntax_error_ref st in
      let st = State.error st ~file ~loc:{ file = file.loc; loc; } parsing_error (syntax_error_ref, perr) in
      st, None

  let parse_stdin st (file : Logic.language file) =
    let lang, file_loc, gen, _ = Logic.parse_input
        ?language:file.lang (`Stdin (Logic.Smtlib2 `Latest))
    in
    let file = { file with loc = file_loc; lang = Some lang; } in
    st, file, gen

  let parse_file st source (file : Logic.language file) lang =
    (* Parse the input *)
    match source with
    | `Raw (filename, contents) ->
      begin match file.mode with
      | None
      | Some `Incremental ->
        let lang, file_loc, gen, cl = Logic.parse_input
            ~language:lang (`Raw (filename, lang, contents)) in
        let file = { file with loc = file_loc; lang = Some lang; } in
        st, file, gen_finally gen cl
      | Some `Full ->
        let lang, file_loc, l = Logic.parse_raw_lazy ~language:lang
          ~filename contents
        in
        let file = { file with loc = file_loc; lang = Some lang; } in
        st, file, gen_of_llist l
      end
    | `File f ->
      let s = Dolmen.Std.Statement.include_ f [] in
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

  (* This is a ['a Gen.t] with a threaded state. *)
  type 'a gen' = {g: State.t -> State.t * 'a option} [@@unboxed]

  let flat_map (f : 'a -> 'b gen') ({ g } : 'a gen') : 'b gen' =
    (* [cur_f] holds the last generator returned by a call to [f] *)
    let cur_f = ref None in
    let rec aux st =
      match !cur_f with
      | None ->
        (* [cur_f] is empty, get a new one and try again *)
        let st, x = g st in
        begin match x with
        | None -> st, None
        | Some x ->
          cur_f := Some (f x);
          aux st
        end
      | Some { g = c } ->
        (* We got a generator, use it or refill it *)
        let st, c' = c st in
        match c' with
        | Some _ -> st, c'
        | None ->
          cur_f := None;
          aux st
    in
    { g = aux }

  (* [map_st_gen f g] calls [f] on all the values produced by [g] in order,
     threading the state.

     [f] is *not* a generator but a function in the state monad. *)
  let map_st_gen f (g : _ Gen.t) : _ gen' =
    { g = fun st ->
      begin match g () with
      | None -> st, None
      | Some x -> f st x
      end }

  let parse_logic ?(preludes = []) file =
    Gen.(
      append
        (of_list preludes |> map (fun p -> (p, true)))
        (singleton (file, false)))
    |> map_st_gen (fun st ((file : _ file), is_prelude) ->
      match file.source with
      | `Stdin when is_prelude ->
        State.error ~file st stdin_prelude (), None
      | `Stdin ->
        let st, file, g = parse_stdin st file in
        let st = State.set State.logic_file file st in
        st, Some ({ g = wrap_parser ~file g })
      | `Raw (filename, _) | `File filename as source ->
        (* NB: We need to make sure the lang is set before calling
            [switch_to_full_mode_if_needed] *)
        try
          let file, lang =
            match file.lang with
            | Some l -> file, l
            | None ->
              (* Auto-detect input format *)
              let l, _, _ = Logic.of_filename filename in
              { file with lang = Some l }, l
          in
          let st, file = switch_to_full_mode_if_needed st file file in
          let st, file, g = parse_file st source file lang in
          let st = State.set State.logic_file file st in
          st, Some ({ g = wrap_parser ~file g })
        with Logic.Extension_not_found ext ->
          State.error st extension_not_found ext, None
    ) |> flat_map Fun.id |> fun { g } -> g

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
          begin match file.mode with
          | None
          | Some `Incremental ->
            let lang, file_loc, gen, cl = Response.parse_input
              ~language:lang (`Raw (filename, lang, contents)) in
            let file = { file with loc = file_loc; lang = Some lang; } in
            st, file, gen_finally gen cl
          | Some `Full ->
            let lang, file_loc, l = Response.parse_raw_lazy ?language:file.lang
              ~filename contents
            in
            let file = { file with loc = file_loc; lang = Some lang; } in
            st, file, gen_of_llist l
          end
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
    let st = State.set State.response_file file st in
    (* Wrap the resulting parser *)
    st, wrap_parser ~file (Gen.append (Gen.of_list prelude) g)

  (* Expand dolmen statements *)
  (* ************************************************************************ *)

  let merge _ st = st

  let expand st c =
    let ret = match c with
      | { S.descr = S.Pack l; _ } ->
        let file = State.get State.logic_file st in
        st, `Gen (merge, wrap_parser ~file (Gen.of_list l))
      (* TODO: filter the statements by passing some options *)
      | { S.descr = S.Include file; _ } ->
        let logic_file = State.get State.logic_file st in
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
