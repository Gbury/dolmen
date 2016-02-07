
module type Ast = Ast_dimacs.S

module Make
    (L : ParseLocation.S)
    (T : Ast_dimacs.S) = struct

  module P = ParseDimacs.Make(L)(T)

  let parse_file file =
    let ch = open_in file in
    let lexbuf = Lexing.from_channel ch in
    ParseLocation.set_file lexbuf file;
    try
      P.file LexDimacs.token lexbuf
    with
    | P.Error ->
      let pos = L.of_lexbuf lexbuf in
      raise (L.Syntax_error (pos, ""))
    | LexDimacs.Error ->
      let pos = L.of_lexbuf lexbuf in
      raise (L.Lexing_error (pos, Lexing.lexeme lexbuf))
    | _ as e ->
      let pos = L.of_lexbuf lexbuf in
      raise (L.Uncaught (pos, e))

end

