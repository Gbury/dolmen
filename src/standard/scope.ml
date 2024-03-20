
(* This file is free software, part of Archsat. See file "LICENSE" for more details. *)

(* Wrapper around polymorphic identifiers *)
(* ************************************************************************ *)

module type S = Dolmen_intf.Scope.S with type name := Name.t
module type Arg = Dolmen_intf.Id.Scope with type name := Name.t

(* Wrapping type/terms vars/consts into ids *)
(* ************************************************************************ *)

type 'a id = 'a Dolmen_intf.Scope.id =
  | Ty_var : 'ty_var -> < ty_var : 'ty_var; .. > id
  | Ty_cst : 'ty_cst -> < ty_cst : 'ty_cst; .. > id
  | Term_var : 'term_var -> < term_var : 'term_var; .. > id
  | Term_cst : 'term_cst -> < term_cst : 'term_cst; .. > id

module Wrap
    (Ty_var : Arg)(Ty_cst : Arg)
    (Term_var : Arg)(Term_cst : Arg)
  : Arg with type t = <
      ty_var : Ty_var.t;
      ty_cst : Ty_cst.t;
      term_var :  Term_var.t;
      term_cst : Term_cst.t
    > id
= struct

  type t = <
    ty_var : Ty_var.t;
    ty_cst : Ty_cst.t;
    term_var : Term_var.t;
    term_cst : Term_cst.t
  > id


  let equal i j =
    match i, j with
    | Ty_var v, Ty_var v' -> Ty_var.equal v v'
    | Ty_cst c, Ty_cst c' -> Ty_cst.equal c c'
    | Term_var v, Term_var v' -> Term_var.equal v v'
    | Term_cst c, Term_cst c' -> Term_cst.equal c c'
    | _ -> false

  let name = function
    | Ty_var v -> Ty_var.name v
    | Ty_cst c -> Ty_cst.name c
    | Term_var v -> Term_var.name v
    | Term_cst c -> Term_cst.name c

  module Map = struct

    type key = t
    type 'a t = {
      ty_vars : 'a Ty_var.Map.t;
      ty_csts : 'a Ty_cst.Map.t;
      term_vars : 'a Term_var.Map.t;
      term_csts : 'a Term_cst.Map.t;
    }

    let empty = {
      ty_vars = Ty_var.Map.empty;
      ty_csts = Ty_cst.Map.empty;
      term_vars = Term_var.Map.empty;
      term_csts = Term_cst.Map.empty;
    }

    let find_exn k t =
      match k with
      | Ty_var v -> Ty_var.Map.find_exn v t.ty_vars
      | Ty_cst c -> Ty_cst.Map.find_exn c t.ty_csts
      | Term_var v -> Term_var.Map.find_exn v t.term_vars
      | Term_cst c -> Term_cst.Map.find_exn c t.term_csts

    let find_opt k t =
      match k with
      | Ty_var v -> Ty_var.Map.find_opt v t.ty_vars
      | Ty_cst c -> Ty_cst.Map.find_opt c t.ty_csts
      | Term_var v -> Term_var.Map.find_opt v t.term_vars
      | Term_cst c -> Term_cst.Map.find_opt c t.term_csts

    let add k x t =
      match k with
      | Ty_var v -> { t with ty_vars = Ty_var.Map.add v x t.ty_vars; }
      | Ty_cst c -> { t with ty_csts = Ty_cst.Map.add c x t.ty_csts; }
      | Term_var v -> { t with term_vars = Term_var.Map.add v x t.term_vars; }
      | Term_cst c -> { t with term_csts = Term_cst.Map.add c x t.term_csts; }

    let find_add k f t =
      match k with
      | Ty_var v -> { t with ty_vars = Ty_var.Map.find_add v f t.ty_vars; }
      | Ty_cst c -> { t with ty_csts = Ty_cst.Map.find_add c f t.ty_csts; }
      | Term_var v -> { t with term_vars = Term_var.Map.find_add v f t.term_vars; }
      | Term_cst c -> { t with term_csts = Term_cst.Map.find_add c f t.term_csts; }

    let iter f t =
      Ty_var.Map.iter (fun v -> f (Ty_var v)) t.ty_vars;
      Ty_cst.Map.iter (fun c -> f (Ty_cst c)) t.ty_csts;
      Term_var.Map.iter (fun v -> f (Term_var v)) t.term_vars;
      Term_cst.Map.iter (fun c -> f (Term_cst c)) t.term_csts;
      ()

    let fold f t acc =
      let acc = Ty_var.Map.fold (fun v x acc -> f (Ty_var v) x acc) t.ty_vars acc in
      let acc = Ty_cst.Map.fold (fun c x acc -> f (Ty_cst c) x acc) t.ty_csts acc in
      let acc = Term_var.Map.fold (fun v x acc -> f (Term_var v) x acc) t.term_vars acc in
      let acc = Term_cst.Map.fold (fun c x acc -> f (Term_cst c) x acc) t.term_csts acc in
      acc
  end

end

(* Printing wrappers for escapped sequences *)
(* ************************************************************************ *)

module Make(Id : Arg) : S with type id := Id.t = struct

  type binding =
    | Same of Name.t
    | Renamed of { original : Name.t; renamed : Name.t; }

  type on_conflict =
    | Error
    | Shadow
    | Rename

  type rename =
    | Rename : {
        acc : 'acc;
        rename : 'acc -> Name.t -> 'acc * Name.t
      } -> rename

  type conf = {
    rename : rename;
    sanitize : Name.t -> Name.t;
    on_conflict :
      prev_id:Id.t ->
      new_id:Id.t ->
      name:Name.t ->
      on_conflict;
  }

  type t = {
    conf : conf;
    in_scope : Id.t Name.Map.t;
    bindings : binding Id.Map.t;
  }

  exception Conflict of {
      prev_id : Id.t;
      prev_binding : binding;
      new_id : Id.t;
      new_binding : binding;
    }

  let name = function
    | Same name | Renamed { renamed = name; original = _; } -> name

  let mk_rename init rename = Rename { acc = init; rename; }

  let empty ~rename ~sanitize ~on_conflict =
    let conf = { sanitize; rename; on_conflict; } in
    { conf;
      bindings = Id.Map.empty;
      in_scope = Name.Map.empty; }


  (* Adding escapped sequences *)
  (* ************************************************************************ *)

  let binding original name =
    if Name.equal name original
    then Same name
    else Renamed { original; renamed = name; }

  (* do NOT export: this functions unconditionally overwrites any
     pre-existing binding *)
  let unsafe_add t id name binding =
    let t =
      { t with
      in_scope = Name.Map.add name id t.in_scope;
      bindings = Id.Map.add id binding t.bindings; }
    in
    t, binding

  let rec find_name t name acc rename =
    let new_acc, new_name = rename acc name in
    match Name.Map.find_opt new_name t.in_scope with
    | None -> new_name
    | Some _other_name -> find_name t new_name new_acc rename

  let bind t id =
    let original = Id.name id in
    let name = t.conf.sanitize original in
    match Name.Map.find_opt name t.in_scope with
    | None ->
      (* no name conflict *)
      unsafe_add t id name (binding original name)
    | Some prev_id ->
      begin match Id.Map.find_opt prev_id t.bindings with
        | None ->
          (* internal error: the [bindings] and [in_scope] fields
             should form a bijection. *)
          assert false
        | Some prev_binding ->
          if Id.equal id prev_id then
            (* we may want to emit a warning, but this is harmless *)
            t, prev_binding
          else begin
            match t.conf.on_conflict ~prev_id ~new_id:id ~name with
            | Error ->
              raise (Conflict { prev_id; prev_binding; new_id = id;
                                new_binding = binding original name; })
            | Shadow ->
              unsafe_add t id name (binding original name)
            | Rename ->
              let Rename { acc; rename; } = t.conf.rename in
              let name = find_name t name acc rename in
              unsafe_add t id name (binding original name)
          end
      end

  (* Printing *)
  (* ************************************************************************ *)

  let print t fmt id =
    match Id.Map.find_opt id t.bindings with
    | None -> assert false
    | Some binding ->
      let name = name binding in
      begin match Name.Map.find_opt name t.in_scope with
        | Some id' ->
          if Id.equal id id'
          then Name.print fmt name
          else assert false (* TODO: proper error; id had been shadowed by id' *)
        | None ->
          (* internal invariant error: [bindings] and [in_scope]
             do not represent a bijection. *)
          assert false
      end

end

