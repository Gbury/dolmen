
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

module type S = sig

  type file
  type statement
  exception Extension_not_found of string

  type language =
    | Smtlib2 of Dolmen_smtlib2.Response.version

  val enum : (string * language) list
  val string_of_language : language -> string

  val find :
    ?language:language ->
    ?dir:string -> string -> string option
  val parse_all :
    ?language:language ->
    [< `File of string | `Stdin of language
    | `Raw of string * language * string ] ->
    language * file * statement list Lazy.t
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
    (I : Dolmen_intf.Id.Response)
    (T : Dolmen_intf.Term.Response with type location := L.t
                                 and type id := I.t)
    (S : Dolmen_intf.Stmt.Response with type location := L.t
                                 and type id := I.t
                                 and type term := T.t)
  : S with type file := L.file
       and type statement := S.t
= struct

  exception Extension_not_found of string

  module type S = Dolmen_intf.Language.S
    with type statement := S.t
     and type file := L.file

  type language =
    | Smtlib2 of Dolmen_smtlib2.Response.version

  let enum = [
    "smt2",       Smtlib2 `Latest;
    "smt2.6",     Smtlib2 `V2_6;
  ]

  let string_of_language l =
    fst (List.find (fun (_, l') -> l = l') enum)

  let assoc = [

    (* Smtlib2 *)
    Smtlib2 `Latest, ".rsmt2",
    (module Dolmen_smtlib2.Response.Latest.Make(L)(I)(T)(S) : S);
    Smtlib2 `V2_6, ".rsmt2",
    (module Dolmen_smtlib2.Response.V2_6.Make(L)(I)(T)(S) : S);

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

  let parse_all ?language = function
    | `File file ->
      let l, _, (module P : S) =
        match language with
        | None -> of_filename file
        | Some l -> of_language l
      in
      let locfile, res = P.parse_all (`File file) in
      l, locfile, res
    | `Raw (filename, l, s) ->
      let l, _, (module P : S) =
        match language with
        | None -> of_language l
        | Some lang -> of_language lang
      in
      let locfile, res = P.parse_all (`Contents (filename, s)) in
      l, locfile, res
    | `Stdin l ->
      let l, _, (module P : S) =
        match language with
        | None -> of_language l
        | Some lang -> of_language lang
      in
      let locfile, res = P.parse_all `Stdin in
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
      let l, _, (module P : S) = of_language
          (match language with | Some l' -> l' | None -> l) in
      let locfile, gen, cl = P.parse_input `Stdin in
      l, locfile, gen, cl
    | `Raw (filename, l, s) ->
      let _, _, (module P : S) = of_language
          (match language with | Some l' -> l' | None -> l) in
      let locfile, gen, cl = P.parse_input (`Contents (filename, s)) in
      l, locfile, gen, cl

end

