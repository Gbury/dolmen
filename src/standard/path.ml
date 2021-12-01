
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(** Names

*)

(** {2 Type definition} *)

type path = string list

type t =
  | Local of {
      name : string;
    }
  | Absolute of {
      path : path;
      name : string;
    }

let print fmt = function
  | Local { name; } ->
    Format.fprintf fmt "%s" name
  | Absolute { path = []; name; } ->
    Format.fprintf fmt "%s" name
  | Absolute { path; name; } ->
    let pp_sep fmt () = Format.fprintf fmt "." in
    Format.fprintf fmt "%a.%a"
      (Format.pp_print_list ~pp_sep Format.pp_print_string) path
      Format.pp_print_string name


let local name = Local { name; }
let global name = Absolute { path = []; name; }
let absolute path name = Absolute { path; name; }

let rename f = function
  | Local { name; } -> local (f name)
  | Absolute { path; name; } -> absolute path (f name)
