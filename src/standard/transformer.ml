(* This file is free software, part of dolmen. See file "LICENSE" formore information *)

module Make
    (Loc    : Loc.S)
    (Ty     : sig
       type token
       type statement
       val env : string list
       val incremental : bool
       val error : int -> string
     end)
    (Lex    : Dolmen_intf.Lex.S with type token := Ty.token)
    (Parse  : Dolmen_intf.Parse.S with type token := Ty.token
                                   and type statement := Ty.statement) = struct


  (* Module Includes
     --------------- *)

  include Ty

  module Lexer = Lex
  module Parser = Parse


  (* Finding include files
     --------------------- *)

  let rec find_env file = function
    | [] -> None
    | var :: r ->
      begin match Sys.getenv var with
        | dir ->
          let f = Filename.concat dir file in
          if Sys.file_exists f then Some f
          else find_env file r
        | exception Not_found ->
          find_env file r
      end

  let find ?(dir="") file =
    if Filename.is_relative file then begin
      let f = Filename.concat dir file in
      if Sys.file_exists f then
        Some f
      else
          find_env file Ty.env
    end else if Sys.file_exists file then
      Some file
    else
      None


  (* Menhir state & checkpoint manipulations
     --------------------------------------- *)

  let error_message token s =
    match token with
    | None ->
      `Regular (Format.dprintf "Syntax error@ with@ missing@ token@ read,@ \
                                please@ report upstream,@ ^^")
    | Some tok ->
      let tok_descr = Lexer.descr tok in
      begin match String.trim (Ty.error s) with
        | exception Not_found ->
          `Regular (Format.dprintf "Missing@ syntax@ error@ message@ \
                                    (state %d),@ please@ report@ \
                                    upstream,@ ^^" s)
        | "<YOUR SYNTAX ERROR MESSAGE HERE>" ->
          `Regular (Format.dprintf "Syntax error (state %d)@ \
                                    while reading %a." s Tok.print tok_descr)
        | msg ->
          begin match Misc.split_on_char '\n' msg with
            | error_ref :: production :: l ->
              let prod = Format.dprintf "%s" production in
              let lexed = Format.dprintf "%a" Tok.print tok_descr in
              let expected =
                Format.dprintf "%a" Format.pp_print_text (String.concat " " l)
              in
              `Advanced (error_ref, prod, lexed, expected)
            | _ ->
              `Regular (Format.dprintf "Syntax error (state %d)." s)
          end
      end


  (* Parsing loop
     ------------ *)

  let parse_aux ~k_exn newline sync lexbuf parser_fun =
    (* Token supplier *)
    let last_token = ref None in
    let lexer lexbuf =
      let token = Lexer.token newline lexbuf in
      last_token := Some token;
      token
    in
    (* Run the loop *)
    let aux () =
      begin match parser_fun lexer lexbuf with
        | res ->
          let () = sync lexbuf in
          res
        | exception (((Loc.Syntax_error _) | (Loc.Lexing_error _)) as e) ->
          let () = sync lexbuf in
          let () = k_exn () in
          raise e
        | exception Lexer.Error ->
          let pos = Loc.of_lexbuf lexbuf in
          let err = Lexing.lexeme lexbuf in
          let () = sync lexbuf in
          let () = k_exn () in
          raise (Loc.Lexing_error (pos, err))
        | exception Parser.Error state ->
          let pos = Loc.of_lexbuf lexbuf in
          let msg = error_message !last_token state in
          let () = sync lexbuf in
          let () = k_exn () in
          raise (Loc.Syntax_error (pos, msg))
        | exception e ->
          let bt = Printexc.get_raw_backtrace () in
          let pos = Loc.of_lexbuf lexbuf in
          let () = sync lexbuf in
          let () = k_exn () in
          raise (Loc.Uncaught (pos, e, bt))
      end
    in
    aux

  (* Instantiations of the parsing loop
     ---------------------------------- *)

  let parse_all i =
    let lexbuf, cleanup = Misc.mk_lexbuf i in
    let locfile = Loc.mk_file (Misc.filename_of_input i) in
    let newline = Loc.newline locfile in
    let sync = Loc.update_size locfile in
    let k_exn () = cleanup () in
    let res =
      lazy (
        let res =
          parse_aux ~k_exn newline sync lexbuf Parser.file ()
        in
        let () = cleanup () in
        res
      )
    in
    locfile, res

  let parse_input i =
    let lexbuf, cleanup = Misc.mk_lexbuf i in
    let locfile = Loc.mk_file (Misc.filename_of_input i) in
    let newline = Loc.newline locfile in
    let sync = Loc.update_size locfile in
    if not Ty.incremental then begin
      (* If incremental mode is not supported, raise an error rather than
         do weird things. *)
      let msg = Format.dprintf ": @[<hov>%a@]"
          Format.pp_print_text "Input format does not support incremental parsing"
      in
      raise (Loc.Syntax_error (Loc.of_lexbuf lexbuf, `Regular msg))
    end;
    let k_exn () = Dolmen_line.consume ~newline ~sync lexbuf in
    let aux = parse_aux ~k_exn newline sync lexbuf Parser.input in
    locfile, aux, cleanup

end
