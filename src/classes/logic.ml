
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

module Make
    (L : ParseLocation.S)
    (T : Term_intf.Logic with type location := L.t)
    (S : Stmt_intf.Logic with type location := L.t and type term := T.t)
= struct

  exception Extension_not_found of string

  module type S = Language_intf.S with type statement := S.t

  type language =
    | Dimacs
    | Smtlib
    | Tptp
    | Zf

  let string_of_language = function
    | Dimacs -> "dimacs"
    | Smtlib -> "smt2"
    | Tptp -> "tptp"
    | Zf -> "zf"

  let assoc = [
    Dimacs, ".cnf",  (module Dimacs.Make(L)(T)(S) : S);
    Smtlib, ".smt2", (module Smtlib.Make(L)(T)(S) : S);
    Tptp,   ".p",    (module Tptp.Make(L)(T)(S) : S);
    Zf,     ".zf",   (module Zf.Make(L)(T)(S) : S);
  ]

  let of_language l =
    List.find (fun (l', _, _) -> l = l') assoc

  let of_extension ext =
    try
      List.find (fun (_, ext', _) -> ext = ext') assoc
    with Not_found ->
      raise (Extension_not_found ext)

  let parse_file file =
    let l, _, (module P : S) = of_extension (Misc.get_extension file) in
    l, P.parse_file file

  let parse_input ?language = function
    | `File file ->
      let l, _, (module P : S) =
        match language with
        | Some l -> of_language l
        | None -> of_extension (Misc.get_extension file)
      in
      l, P.parse_input (`File file)
    | `Stdin l ->
      let _, _, (module P : S) = of_language
          (match language with | Some l' -> l' | None -> l) in
      l, P.parse_input `Stdin

end

