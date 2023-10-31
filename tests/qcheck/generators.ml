
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

let name ~printable ~simple ~indexed ~qualified =
  let string =
    if printable
    then QCheck2.Gen.string_printable
    else QCheck2.Gen.string
  in
  let simple_gen =
    QCheck2.Gen.map (fun s -> Dolmen_std.Name.simple s) string
  in
  let indexed_gen =
    QCheck2.Gen.map
      (fun (basename, indexes) -> Dolmen_std.Name.indexed basename indexes)
      (QCheck2.Gen.pair string (QCheck2.Gen.list string))
  in
  let qualified_gen =
    QCheck2.Gen.map
      (fun (path, basename) -> Dolmen_std.Name.qualified path basename)
      (QCheck2.Gen.pair (QCheck2.Gen.list string) string)
  in
  QCheck2.Gen.oneof (
    (if simple then [simple_gen] else []) @
    (if indexed then [indexed_gen] else []) @
    (if qualified then [qualified_gen] else [])
  )

