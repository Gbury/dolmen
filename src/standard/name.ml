
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


(* Maps *)
(* ************************************************************************* *)

module Map = struct

  module M = Maps.String

  (* Types *)
  type 'a t = {
    simple : 'a M.t;
    qualified : 'a qualified M.t;
    indexed : 'a indexed M.t;
  }

  and 'a qualified = {
    base : 'a M.t;
    path : 'a qualified M.t;
  }

  and 'a indexed = {
    value : 'a option;
    index : 'a indexed M.t;
  }


  (* Empty *)
  let empty = {
    simple = M.empty;
    qualified = M.empty;
    indexed = M.empty;
  }

  let empty_q = {
    base = M.empty;
    path = M.empty;
  }

  let empty_i = {
    value = None;
    index = M.empty;
  }


  (* find *)
  let rec find_opt k t =
    match k with
    | Simple basename ->
      M.find_opt basename t.simple
    | Qualified { path = []; _ } -> assert false
    | Qualified { path = hd :: tl; basename; } ->
      begin match M.find_opt hd t.qualified with
        | None -> None
        | Some q -> find_qualified basename q tl
      end
    | Indexed { basename; indexes; } ->
      begin match M.find_opt basename t.indexed with
        | None -> None
        | Some i -> find_indexed i indexes
      end

  and find_qualified basename q = function
    | [] -> M.find_opt basename q.base
    | s :: r ->
      match M.find_opt s q.path with
      | None -> None
      | Some q' -> find_qualified basename q' r

  and find_indexed i = function
    | [] -> i.value
    | s :: r ->
      match M.find_opt s i.index with
      | None -> None
      | Some i' -> find_indexed i' r

  let find_exn k t =
    match find_opt k t with
    | Some res -> res
    | None -> raise Not_found


  (* Add *)
  let rec add k v t =
    match k with
    | Simple basename ->
      { t with simple = M.add basename v t.simple; }
    | Qualified { path = []; _ } -> assert false
    | Qualified { path = hd :: tl; basename; } ->
      { t with qualified = M.find_add hd (function
            | None -> add_qualified basename v empty_q tl
            | Some q -> add_qualified basename v q tl
          ) t.qualified; }
    | Indexed { basename; indexes; } ->
      { t with indexed = M.find_add basename (function
            | None -> add_indexed v empty_i indexes
            | Some i -> add_indexed v i indexes
          ) t.indexed; }

  and add_qualified basename v q = function
    | [] -> { q with base = M.add basename v q.base; }
    | s :: r ->
      { q with path = M.find_add s (function
            | None -> add_qualified basename v empty_q r
            | Some q' -> add_qualified basename v q' r
          ) q.path; }

  and add_indexed v i = function
    | [] -> { i with value = Some v; }
    | s :: r ->
      { i with index = M.find_add s (function
            | None -> add_indexed v empty_i r
            | Some i' -> add_indexed v i' r
          ) i.index; }

  (* Find-Add *)
  let rec find_add k f t =
    match k with
    | Simple basename ->
      { t with simple = M.find_add basename f t.simple; }
    | Qualified { path = []; _ } -> assert false
    | Qualified { path = hd :: tl; basename; } ->
      { t with qualified = M.find_add hd (function
            | None -> find_add_qualified basename f empty_q tl
            | Some q -> find_add_qualified basename f q tl
          ) t.qualified; }
    | Indexed { basename; indexes; } ->
      { t with indexed = M.find_add basename (function
            | None -> find_add_indexed f empty_i indexes
            | Some i -> find_add_indexed f i indexes
          ) t.indexed; }

  and find_add_qualified basename f q = function
    | [] -> { q with base = M.find_add basename f q.base; }
    | s :: r ->
      { q with path = M.find_add s (function
            | None -> find_add_qualified basename f empty_q r
            | Some q' -> find_add_qualified basename f q' r
          ) q.path; }

  and find_add_indexed f i = function
    | [] -> { i with value = Some (f i.value); }
    | s :: r ->
      { i with index = M.find_add s (function
            | None -> find_add_indexed f empty_i r
            | Some i' -> find_add_indexed f i' r
          ) i.index; }

  (* Iter *)
  let rec iter f t =
    M.iter (fun s v -> f (simple s) v) t.simple;
    M.iter (fun s q -> iter_qualified f [s] q) t.qualified;
    M.iter (fun basename i -> iter_indexed f basename [] i) t.indexed;
    ()

  and iter_qualified f rev_path q =
    let path = List.rev rev_path in
    M.iter (fun s v -> f (qualified path s) v) q.base;
    M.iter (fun s q' -> iter_qualified f (s :: rev_path) q') q.path;
    ()

  and iter_indexed f basename indexes i =
    begin match i.value with
      | None -> ()
      | Some v -> f (indexed basename (List.rev indexes)) v
    end;
    M.iter (fun s i' -> iter_indexed f basename (s :: indexes) i') i.index;
    ()

  let fold f t acc =
    let r = ref acc in
    iter (fun name v -> r := f name v !r) t;
    !r

end
