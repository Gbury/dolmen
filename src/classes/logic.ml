
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

module type S = sig

  type file
  type statement
  exception Extension_not_found of string

  type language =
    | Alt_ergo
    | Dimacs
    | ICNF
    | Smtlib2 of Dolmen_smtlib2.version
    | Tptp of Dolmen_tptp.version
    | Zf

  val enum : (string * language) list
  val string_of_language : language -> string

  val find :
    ?language:language ->
    ?dir:string -> string -> string option
  val parse_file :
    ?language:language ->
    string -> language * file * statement list
  val parse_file_lazy :
    ?language:language ->
    string -> language * file * statement list Lazy.t
  val parse_input :
    ?language:language ->
    [< `File of string | `Stdin of language
    | `Raw of string * language * string ] ->
    language * file * (unit -> statement option) * (unit -> unit)

  module type S = Dolmen_intf.Language.S
    with type statement := statement
     and type file := file

  val of_language   : language  -> language * string * (module S)
  val of_extension  : string    -> language * string * (module S)
  val of_filename   : string    -> language * string * (module S)

end

module Make
    (L : Dolmen_intf.Location.S)
    (I : Dolmen_intf.Id.Logic)
    (T : Dolmen_intf.Term.Logic with type location := L.t
                                 and type id := I.t)
    (S : Dolmen_intf.Stmt.Logic with type location := L.t
                                 and type id := I.t
                                 and type term := T.t) = struct

  exception Extension_not_found of string

  module type S = Dolmen_intf.Language.S
    with type statement := S.t
     and type file := L.file

  type language =
    | Alt_ergo
    | Dimacs
    | ICNF
    | Smtlib2 of Dolmen_smtlib2.version
    | Tptp of Dolmen_tptp.version
    | Zf

  let enum = [
    "ae",         Alt_ergo;
    "dimacs",     Dimacs;
    "iCNF",       ICNF;
    "smt2",       Smtlib2 `Latest;
    "smt2.6",     Smtlib2 `V2_6;
    "tptp",       Tptp `Latest;
    "tptp-6.3.0", Tptp `V6_3_0;
    "zf",         Zf;
  ]

  let string_of_language l =
    fst (List.find (fun (_, l') -> l = l') enum)

  let assoc = [

    (* Alt-ergo format *)
    Alt_ergo, ".ae",
    (module Dolmen_ae.Make(L)(I)(T)(S) : S);

    (* Dimacs *)
    Dimacs, ".cnf",
    (module Dolmen_dimacs.Make(L)(T)(S) : S);

    (* iCNF *)
    ICNF, ".icnf",
    (module Dolmen_icnf.Make(L)(T)(S) : S);

    (* Smtlib2 *)
    Smtlib2 `Latest, ".smt2",
    (module Dolmen_smtlib2.Latest.Make(L)(I)(T)(S) : S);
    Smtlib2 `V2_6, ".smt2",
    (module Dolmen_smtlib2.V2_6.Make(L)(I)(T)(S) : S);
    Smtlib2 `Poly, ".psmt2",
    (module Dolmen_smtlib2.Poly.Make(L)(I)(T)(S) : S);

    (* TPTP *)
    Tptp `Latest, ".p",
    (module Dolmen_tptp.Latest.Make(L)(I)(T)(S) : S);
    Tptp `V6_3_0, ".p",
    (module Dolmen_tptp.V6_3_0.Make(L)(I)(T)(S) : S);

    (* Zipperposition format *)
    Zf, ".zf",
    (module Dolmen_zf.Make(L)(I)(T)(S) : S);
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
    let l, _, (module P : S) =
      match language with
      | None -> of_filename file
      | Some l -> of_language l
    in
    let locfile, res = P.parse_file file in
    l, locfile, res

  let parse_file_lazy ?language file =
    let l, _, (module P : S) =
      match language with
      | None -> of_filename file
      | Some l -> of_language l
    in
    let locfile, res = P.parse_file_lazy file in
    l, locfile, res

  let parse_input ?language = function
    | `File file ->
      let l, _, (module P : S) =
        match language with
        | Some l -> of_language l
        | None -> of_extension (Dolmen_std.Misc.get_extension file)
      in
      let locfile, gen, cl = P.parse_input (`File file) in
      l, locfile, gen, cl
    | `Stdin l ->
      let _, _, (module P : S) = of_language
          (match language with | Some l' -> l' | None -> l) in
      let locfile, gen, cl = P.parse_input `Stdin in
      l, locfile, gen, cl
    | `Raw (filename, l, s) ->
      let _, _, (module P : S) = of_language
          (match language with | Some l' -> l' | None -> l) in
      let locfile, gen, cl = P.parse_input (`Contents (filename, s)) in
      l, locfile, gen, cl

end

