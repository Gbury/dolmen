
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

  type key = t

  (* Types *)
  type 'a t = {
    simple : 'a M.t;
    qualified : 'a qualified M.t;
    indexed : 'a indexed M.t;
  }

  and 'a qualified = {
    base : 'a M.t;
    mod_value : 'a option;
    path : 'a qualified M.t;
  }

  and 'a indexed = {
    value : 'a option;
    prefix_value : 'a option;
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
    mod_value = None;
  }

  let empty_i = {
    value = None;
    prefix_value = None;
    index = M.empty;
  }

  (* helper functions *)
  let (>=) o o' =
    match o with
    | Some _ as res -> res
    | None -> o'

  (* find *)
  let rec find_opt k t =
    match k with
    | Simple basename ->
      M.find_opt basename t.simple
    | Qualified { path = []; _ } -> assert false
    | Qualified { path = hd :: tl; basename; } ->
      begin match M.find_opt hd t.qualified with
        | None -> None
        | Some q -> find_qualified basename None q tl
      end
    | Indexed { basename; indexes; } ->
      begin match M.find_opt basename t.indexed with
        | None -> None
        | Some i -> find_indexed None i indexes
      end

  and find_qualified basename prec_mod_value { base; path; mod_value; } = function
    | [] -> M.find_opt basename base >= mod_value >= prec_mod_value
    | s :: r ->
      let prec_mod_value = mod_value >= prec_mod_value in
      begin match M.find_opt s path with
        | Some q' -> find_qualified basename prec_mod_value q' r
        | None -> prec_mod_value
      end

  and find_indexed prec_index_value { value; index; prefix_value; } indexes =
    match indexes with
    | [] -> value >= prefix_value >= prec_index_value
    | s :: r ->
      let prec_index_value = prefix_value >= prec_index_value in
      begin match M.find_opt s index with
        | Some i' -> find_indexed prec_index_value i' r
        | None -> prec_index_value
      end

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

  (* Add-set *)
  let rec add_set ~prefix v t =
    match prefix with
    | Simple basename ->
      { t with simple = M.add basename v t.simple; }
    | Qualified { path = []; _ } -> assert false
    | Qualified { path = hd :: tl; basename; } ->
      { t with qualified = M.find_add hd (function
            | None -> add_set_qualified basename v empty_q tl
            | Some q -> add_set_qualified basename v q tl
          ) t.qualified; }
    | Indexed { basename; indexes; } ->
      { t with indexed = M.find_add basename (function
            | None -> add_set_indexed v empty_i indexes
            | Some i -> add_set_indexed v i indexes
          ) t.indexed; }

  and add_set_qualified basename v q = function
    | [] ->
      assert (basename = "");
      { q with mod_value = Some v; }
    | s :: r ->
      { q with path = M.find_add s (function
            | None -> add_set_qualified basename v empty_q r
            | Some q' -> add_set_qualified basename v q' r
          ) q.path; }

  and add_set_indexed v i = function
    | [] -> { i with prefix_value = Some v; }
    | s :: r ->
      { i with index = M.find_add s (function
            | None -> add_set_indexed v empty_i r
            | Some i' -> add_set_indexed v i' r
          ) i.index; }

  (* Find-Add *)
  let rec find_add k f t =
    match k with
    | Simple basename ->
      { t with simple = M.find_add basename f t.simple; }
    | Qualified { path = []; _ } -> assert false
    | Qualified { path = hd :: tl; basename; } ->
      { t with qualified = M.find_add hd (function
            | None -> find_add_qualified None basename f empty_q tl
            | Some q -> find_add_qualified None basename f q tl
          ) t.qualified; }
    | Indexed { basename; indexes; } ->
      { t with indexed = M.find_add basename (function
            | None -> find_add_indexed None f empty_i indexes
            | Some i -> find_add_indexed None f i indexes
          ) t.indexed; }

  and find_add_qualified prec_mod_value basename f q = function
    | [] -> { q with base = M.find_add basename (fun old ->
        f (old >= q.mod_value >= prec_mod_value)
      ) q.base; }
    | s :: r ->
      let prec_mod_value = q.mod_value >= prec_mod_value in
      { q with path = M.find_add s (function
            | Some q' -> find_add_qualified prec_mod_value basename f q' r
            | None -> find_add_qualified prec_mod_value basename f empty_q r
          ) q.path; }

  and find_add_indexed prec_index_value f i = function
    | [] -> { i with value = Some (
        f (i.value >= i.prefix_value >= prec_index_value)
      ); }
    | s :: r ->
      let prec_index_value = i.prefix_value >= prec_index_value in
      { i with index = M.find_add s (function
            | None -> find_add_indexed prec_index_value f empty_i r
            | Some i' -> find_add_indexed prec_index_value f i' r
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
