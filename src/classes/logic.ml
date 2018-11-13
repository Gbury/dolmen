
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

module Make
    (L : Dolmen_intf.Location.S)
    (I : Dolmen_intf.Id.Logic)
    (T : Dolmen_intf.Term.Logic with type location := L.t
                                 and type id := I.t)
    (S : Dolmen_intf.Stmt.Logic with type location := L.t
                                 and type id := I.t
                                 and type term := T.t) = struct

  exception Extension_not_found of string

  module type S = Dolmen_intf.Language.S with type statement := S.t

  type language =
    | Dimacs
    | ICNF
    | Smtlib
    | Tptp
    | Zf

  let enum = [
    "dimacs", Dimacs;
    "iCNF",   ICNF;
    "smt2",   Smtlib;
    "tptp",   Tptp;
    "zf",     Zf;
  ]

  let string_of_language l =
    fst (List.find (fun (_, l') -> l = l') enum)

  let assoc = [
    Dimacs, ".cnf",  (module Dolmen_dimacs.Make(L)(T)(S)     : S);
    ICNF,   ".icnf", (module Dolmen_icnf.Make(L)(T)(S)       : S);
    Smtlib, ".smt2", (module Dolmen_smtlib.Make(L)(I)(T)(S)  : S);
    Tptp,   ".p",    (module Dolmen_tptp.Make(L)(I)(T)(S)    : S);
    Zf,     ".zf",   (module Dolmen_zf.Make(L)(I)(T)(S)      : S);
  ]

  let of_language l =
    List.find (fun (l', _, _) -> l = l') assoc

  let of_extension ext =
    try
      List.find (fun (_, ext', _) -> ext = ext') assoc
    with Not_found ->
      raise (Extension_not_found ext)

  let of_filename s = of_extension (Dolmen_std.Misc.get_extension s)

  let find ?language ?(dir="") file =
    match language with
    | None ->
      let f =
        if Filename.is_relative file then
          Filename.concat dir file
        else file
      in
      if Sys.file_exists f then Some f else None
    | Some l ->
      let _, _, (module P : S) = of_language l in
      P.find ~dir file

  let parse_file ?language file =
    let l, _, (module P : S) = match language with
      | None -> of_filename file
      | Some l -> of_language l
    in
    l, P.parse_file file

  let parse_input ?language = function
    | `File file ->
      let l, _, (module P : S) =
        match language with
        | Some l -> of_language l
        | None -> of_extension (Dolmen_std.Misc.get_extension file)
      in
      let gen, cl = P.parse_input (`File file) in
      l, gen, cl
    | `Stdin l ->
      let _, _, (module P : S) = of_language
          (match language with | Some l' -> l' | None -> l) in
      let gen, cl = P.parse_input `Stdin in
      l, gen, cl

end

