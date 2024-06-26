(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

type 'a location =
  | Builtin of 'a
    (** A built-in extension. *)
  | Unavailable
    (** An unavailable extension. Cannot be loaded. *)
  | Dune_plugin of string
    (** An external extension to be loaded dune's plugin mechanism. Loading the
        plugin should register an extension in the appropriate registry. *)
(** The ['a location] type represents the location of an extension (either
    built-in, or to be loaded from a specific dune plugin). *)

let is_available = function
  | Unavailable -> false
  | Builtin _ | Dune_plugin _ -> true

(* Merge two possible locations for the same plugin. Prefer external plugins
   over built-in plugins to allow overrides. *)
let merge_location p1 p2 =
  match p1, p2 with
  | Dune_plugin _ as p, _ | _, (Dune_plugin _ as p) -> p
  | Builtin _ as p, _ | _, (Builtin _ as p) -> p
  | Unavailable, Unavailable -> Unavailable

type t = {
  extension_name : string ;
  (** Name of the extension (without dot). *)
  typing_plugin : Dolmen_loop.Typer.Ext.t location ;
  (** Name of the Dune plugin that provide typing extensions. *)
  model_plugin : Dolmen_model.Ext.t location ;
  (** Name of the Dune plugin that provide model extensions. *)
}

let has_typing_extension { typing_plugin; _ } = is_available typing_plugin

let has_model_extension { model_plugin; _ } = is_available model_plugin

let pp ppf e =
  let variants =
    (if has_typing_extension e then ["typing"] else []) @
    (if has_model_extension e then ["model"] else [])
  in
  Fmt.pf ppf "%s@ %a" e.extension_name
    Fmt.(parens @@ list ~sep:comma string) variants

let name { extension_name ; _ } = extension_name

type kind = Typing | Model

let parse_ext_opt s : (string * kind option) option =
  match String.rindex s '.' with
  | exception Not_found -> Some (s, None)
  | pos ->
    let extension_name = String.sub s 0 pos in
    let plugin_kind = String.sub s (pos + 1) (String.length s - pos - 1) in
    match plugin_kind with
    | "typing" -> Some (extension_name, Some Typing)
    | "model" -> Some (extension_name, Some Model)
    | _ -> None

let merge_ext e1 e2 =
  assert (e1.extension_name = e2.extension_name);
  let typing_plugin = merge_location e1.typing_plugin e2.typing_plugin
  and model_plugin = merge_location e1.model_plugin e2.model_plugin in
  { extension_name = e1.extension_name
  ; typing_plugin
  ; model_plugin }

let add_ext tbl ext =
  let name = ext.extension_name in
  match Hashtbl.find tbl name with
  | ext' -> Hashtbl.replace tbl name (merge_ext ext ext')
  | exception Not_found -> Hashtbl.replace tbl name ext

let infos =
  lazy (
    let extensions = Hashtbl.create 17 in
    (* Add builtin typing extensions. *)
    List.iter (fun ext ->
      add_ext extensions
        { extension_name = Dolmen_loop.Typer.Ext.name ext
        ; typing_plugin = Builtin ext
        ; model_plugin = Unavailable }
    ) (Dolmen_loop.Typer.Ext.list ());

    (* Add builtin model extensions. *)
    List.iter (fun ext ->
      add_ext extensions
        { extension_name = Dolmen_model.Ext.name ext
        ; typing_plugin = Unavailable
        ; model_plugin = Builtin ext }
    ) (Dolmen_model.Ext.list ());

    (* Add extensions from plugins. *)
    let add_plugin invalid plugin = function
      | None -> plugin :: invalid
      | Some (extension_name, k) ->
        let typing_plugin =
          match k with
          | None | Some Typing -> Dune_plugin plugin
          | Some _ -> Unavailable
        and model_plugin =
          match k with
          | None | Some Model -> Dune_plugin plugin
          | Some _ -> Unavailable
        in
        add_ext extensions
          { extension_name ; typing_plugin ; model_plugin };
        invalid
    in
    let invalid =
      List.fold_left (fun invalid e ->
        add_plugin invalid e @@ parse_ext_opt e
      ) [] (Dolmen.Sites.Plugins.Plugins.list ())
    in
    extensions, List.fast_sort String.compare invalid
  )

let invalid () = snd @@ Lazy.force infos

let list () =
  List.fast_sort
    (fun e1 e2 -> String.compare (name e1) (name e2))
    (Hashtbl.fold (fun _ e exts -> e :: exts) (fst @@ Lazy.force infos) [])

let find_ext name =
  let exts, _ = Lazy.force infos in
  try Ok (Hashtbl.find exts name)
  with Not_found ->
    Fmt.error_msg
      "@[<v>Could not find extension '%s'@ \
      Available extensions:@;<1 2>@[<v>%a@]@]"
      name
      Fmt.(list (box pp)) (list ())

let load_typing_extension ext =
    match ext.typing_plugin with
    | Unavailable ->
      Fmt.error_msg "No typing extension '%s'" ext.extension_name
    | Builtin e ->  Ok e
    | Dune_plugin plugin ->
      Dolmen.Sites.Plugins.Plugins.load plugin;
      try
        Ok (List.find
          (fun e -> Dolmen_loop.Typer.Ext.name e = ext.extension_name)
          (Dolmen_loop.Typer.Ext.list ())
        )
      with Not_found ->
        Fmt.error_msg
          "Plugin '%s' did not register a typing extension for '%s'"
          plugin ext.extension_name

let load_model_extension ext =
    match ext.model_plugin with
    | Unavailable ->
      Fmt.error_msg "No model extension '%s'" ext.extension_name
    | Builtin e ->  Ok e
    | Dune_plugin plugin ->
      Dolmen.Sites.Plugins.Plugins.load plugin;
      try
        Ok (List.find
          (fun e -> Dolmen_model.Ext.name e = ext.extension_name)
          (Dolmen_model.Ext.list ())
        )
      with Not_found ->
        Fmt.error_msg
          "Plugin '%s' did not register a model extension for '%s'"
          plugin ext.extension_name

let parse s =
  match parse_ext_opt s with
  | None -> Fmt.error_msg "Invalid extension name '%s'" s
  | Some (name, kind) ->
    Result.map (fun ext -> (ext, kind)) (find_ext name)