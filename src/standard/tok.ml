
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

type descr = Dolmen_intf.Tok.descr = {
  article : string;
  kind : string;
  lexeme : string;
  hint : string option;
}

let descr ?hint ?(article="the") ~kind lexeme = { article; kind; lexeme; hint; }

let print fmt { article; kind; lexeme; _ } =
  let lxm =
    if String.length lexeme < 15
    then lexeme
    else String.sub lexeme 0 14 ^ "…"
  in
  match lxm with
  | "" -> Format.fprintf fmt "%s %s" article kind
  | s -> Format.fprintf fmt "%s %s '%s'" article kind s

