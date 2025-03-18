
(* This file is free software, part of Archsat. See file "LICENSE" for more details. *)

(* Wrapper around polymorphic identifiers *)
(* ************************************************************************ *)

module type S = Dolmen_intf.Scope.S
  with type name := Name.t
module type Arg = Dolmen_intf.Id.Scope_Full
  with type path := Path.t
   and type namespace := Namespace.t

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

  let path = function
    | Ty_var v -> Ty_var.path v
    | Ty_cst c -> Ty_cst.path c
    | Term_var v -> Term_var.path v
    | Term_cst c -> Term_cst.path c

  let namespace = function
    | Ty_var v -> Ty_var.namespace v
    | Ty_cst c -> Ty_cst.namespace c
    | Term_var v -> Term_var.namespace v
    | Term_cst c -> Term_cst.namespace c

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


module Make(Tid : Arg) : S with type id := Tid.t = struct

  (* Note:
     Tid : module for Typied identifiers
     Pid : module for Parsed/Printed identifiers *)
  module Pid = Id

  type binding =
    | Same of Pid.t
    | Renamed of { original : Name.t; renamed : Pid.t; }

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
    sanitize : Tid.t -> Name.t -> Name.t;
    on_conflict :
      prev_id:Tid.t ->
      new_id:Tid.t ->
      name:Name.t ->
      on_conflict;
  }

  type t = {
    conf : conf;
    in_scope : Tid.t Pid.Map.t;
    bindings : binding Tid.Map.t;
  }

  exception Conflict of {
      prev_id : Tid.t;
      prev_binding : binding;
      new_id : Tid.t;
      new_binding : binding;
    }

  let pid = function
    | Same id | Renamed { renamed = id; original = _; } -> id

  let mk_rename init rename = Rename { acc = init; rename; }

  let empty ~rename ~sanitize ~on_conflict =
    let conf = { sanitize; rename; on_conflict; } in
    { conf;
      bindings = Tid.Map.empty;
      in_scope = Pid.Map.empty; }


  (* Adding escapped sequences *)
  (* ************************************************************************ *)

  let binding original pid =
    if Name.equal (Pid.name pid) original
    then Same pid
    else Renamed { original; renamed = pid; }

  (* do NOT export: this functions unconditionally overwrites any
     pre-existing binding *)
  let unsafe_add t tid original pid =
    let binding = binding original pid in
    let t =
      { t with
      in_scope = Pid.Map.add pid tid t.in_scope;
      bindings = Tid.Map.add tid binding t.bindings; }
    in
    t

  let rec find_name t ns name acc rename =
    let new_acc, new_name = rename acc name in
    let pid = Pid.create ns new_name in
    match Pid.Map.find_opt pid t.in_scope with
    | None -> pid
    | Some _other_name -> find_name t ns name new_acc rename

  let bind t id =
    match Tid.Map.find_opt id t.bindings with
    | Some _ ->
      assert false (* binding already bound id, TODO: proper error *)
    | None ->
      let original =
        match Tid.path id with
        | Local { name; } | Absolute { name; path = []; } ->
          Name.simple name
        | Absolute { name; path; } ->
          Name.qualified path name
      in
      let ns = Tid.namespace id in
      let name = t.conf.sanitize id original in
      let pid = Pid.create ns name in
      begin match Pid.Map.find_opt pid t.in_scope with
        | None ->
          (* no name conflict *)
          unsafe_add t id original pid
        | Some prev_id ->
          begin match Tid.Map.find_opt prev_id t.bindings with
            | None ->
              (* internal error: the [bindings] and [in_scope] fields
                 should form a bijection. *)
              assert false
            | Some prev_binding ->
              if Tid.equal id prev_id then
                (* we may want to emit a warning, but this is harmless *)
                t
              else begin
                match t.conf.on_conflict ~prev_id ~new_id:id ~name with
                | Error ->
                  raise (Conflict { prev_id; prev_binding; new_id = id;
                                    new_binding = binding original pid; })
                | Shadow ->
                  unsafe_add t id original pid
                | Rename ->
                  let Rename { acc; rename; } = t.conf.rename in
                  let pid = find_name t ns name acc rename in
                  (* sanity check *)
                  assert (Name.equal (Pid.name pid) (t.conf.sanitize id (Pid.name pid)));
                  unsafe_add t id original pid
              end
          end
      end

  (* Printing *)
  (* ************************************************************************ *)

  let name t id =
    (* TODO: allow for globals from other modules
       to be printed without being bound *)
    match Tid.Map.find_opt id t.bindings with
    | None ->
      (* TODO: proper error, missing id. *)
      failwith "cannot find binding for id"
    | Some binding ->
      let pid = pid binding in
      begin match Pid.Map.find_opt pid t.in_scope with
        | Some id' ->
          if Tid.equal id id'
          then Pid.name pid
          else assert false (* TODO: proper error; id had been shadowed by id' *)
        | None ->
          (* internal invariant error: [bindings] and [in_scope]
             do not represent a bijection. *)
          assert false
      end

end

