
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(* Type definition *)
(* ************************************************************************* *)

type t =
  | Simple of string
  | Indexed of {
      basename : string;
      indexes : string list;
    }
  | Qualified of {
      path : string list;
      basename : string;
    }


(* Creation functions *)
(* ************************************************************************* *)

let simple basename =
  Simple basename

let indexed basename indexes =
  Indexed { basename; indexes; }

let qualified path basename =
  Qualified { path; basename; }


(* Std functions *)
(* ************************************************************************* *)

let print fmt = function
  | Simple basename ->
    Format.fprintf fmt "%s" basename
  | Indexed { basename; indexes; } ->
    Format.fprintf fmt "(_ %s %a)" basename
      (Format.pp_print_list ~pp_sep:Format.pp_print_space
         Format.pp_print_string) indexes
  | Qualified { path = []; basename; } ->
    Format.fprintf fmt "%s" basename
  | Qualified { path; basename; } ->
    Format.fprintf fmt "%a.%s"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space
         Format.pp_print_string) path basename

let hash = function
  | Simple basename ->
    Misc.hash2 3
      (Misc.hash_string basename)
  | Indexed { basename; indexes; } ->
    Misc.hash3 5
      (Misc.hash_string basename)
      (Misc.hash_list Misc.hash_string indexes)
  | Qualified { path; basename; } ->
    Misc.hash3 7
      (Misc.hash_list Misc.hash_string path)
      (Misc.hash_string basename)

let discr = function
  | Simple _ -> 0
  | Indexed _ -> 1
  | Qualified _ -> 2

let compare n n' =
  let (<?>) = Misc.(<?>) in
  match n, n' with
  | Simple name, Simple name' ->
    String.compare name name'
  | Indexed { basename = s; indexes = l; },
    Indexed { basename = s'; indexes = l'; } ->
    String.compare s s'
    <?> (Misc.lexicographic String.compare, l, l')
  | Qualified { path = p; basename = s; },
    Qualified { path = p'; basename = s'; } ->
    Misc.lexicographic String.compare p p'
    <?> (String.compare, s, s')

  | _, _ -> compare (discr n) (discr n')

let equal n n' = n == n' || compare n n' = 0


