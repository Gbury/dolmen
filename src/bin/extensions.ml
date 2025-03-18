(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

type location =
  | Builtin
    (** A built-in extension. *)
  | Dune_plugin of string
    (** An external extension to be loaded using dune's plugin mechanism.
        Loading the plugin should register an extension in the appropriate
        registry. *)
(** The ['a location] type represents the location of an extension (either
    built-in, or to be loaded from a specific dune plugin). *)

type t = {
  extension_name : string ;
  (** Name of the extension. *)
  extension_location : location ;
}

let pp ppf { extension_name; extension_location } =
  let pp_location ppf = function
    | Builtin -> Fmt.pf ppf "@ (builtin)"
    | Dune_plugin _ -> ()
  in
  Fmt.pf ppf "@[%s%a@]" extension_name pp_location extension_location

let name { extension_name ; _ } = extension_name

let builtin_extensions =
  (* Note: this must be computed at toplevel in order to make sure we don't
     pick up extensions loaded from plugins as builtin. *)

  let builtin_extensions = Hashtbl.create 17 in

  (* Add builtin typing extensions. *)
  Dolmen_loop.Typer.Ext.iter (fun ext ->
    Hashtbl.replace builtin_extensions (Dolmen_loop.Typer.Ext.name ext) ()
  );

  (* Add builtin model extensions (note: these may share name with typing
     extensions). *)
  Dolmen_model.Ext.iter (fun ext ->
    Hashtbl.replace builtin_extensions (Dolmen_model.Ext.name ext) ()
  );

  builtin_extensions

let dune_extensions =
  lazy (
    let extensions = Hashtbl.create 17 in

    List.iter (fun plugin ->
      if Hashtbl.mem builtin_extensions plugin then
        Format.eprintf
          "@[<v>Warning: @[<hov>%a@ '%s'@ %a@]@]@."
          Fmt.text "Ignoring external extension"
          plugin
          Fmt.text "as it would override a builtin extension of the same name."
      else
        Hashtbl.replace extensions plugin (Dune_plugin plugin)
    ) (Dolmen.Sites.Plugins.Plugins.list ());

    extensions
  )

let list () =
  let all_extensions =
    Hashtbl.fold
      (fun extension_name () exts ->
        { extension_name ; extension_location = Builtin } :: exts)
      builtin_extensions
      []
  in
  let all_extensions =
    Hashtbl.fold
      (fun extension_name extension_location exts ->
        { extension_name ; extension_location } :: exts)
      (Lazy.force dune_extensions)
      all_extensions
  in
  List.fast_sort
    (fun e1 e2 -> String.compare (name e1) (name e2))
    all_extensions

let loaded_plugins =
  Hashtbl.create 17

let load_plugin_or_fail location =
  match location with
  | Builtin -> Ok ()
  | Dune_plugin plugin ->
    if Hashtbl.mem loaded_plugins plugin then Ok ()
    else
      match Dolmen.Sites.Plugins.Plugins.load plugin with
      | () ->
        Hashtbl.replace loaded_plugins plugin ();
        Ok ()
      | exception Dynlink.Error err ->
        Fmt.error_msg "while loading plugin %s: %s"
          plugin (Dynlink.error_message err)

let add_typing_extensions exts st =
  Loop.State.update Loop.Typer.extension_builtins (List.append exts) st

let load_typing_extension { extension_name ; extension_location } st =
  Result.bind (load_plugin_or_fail extension_location) @@ fun ()  ->
  match Dolmen_loop.Typer.Ext.find_all extension_name with
  | [] -> Ok st
  | exts  -> Ok (add_typing_extensions exts st)

let add_model_extensions exts st =
  Loop.State.update Loop.Check.builtins
   (fun bs ->
     Dolmen_model.Eval.builtins
       (List.append (List.map Dolmen_model.Ext.builtins exts) [ bs ]))
   st

let load_model_extension { extension_name; extension_location } st =
  Result.bind (load_plugin_or_fail extension_location) @@ fun ()  ->
  match Dolmen_model.Ext.find_all extension_name with
  | [] -> Ok st
  | exts  -> Ok (add_model_extensions exts st)

let parse extension_name =
  let extension_location =
    if Hashtbl.mem builtin_extensions extension_name
    then Ok Builtin
    else
      (* Dune doesn't provide a way to check if a plugin with a given name is
         available, so list all plugins to check. *)
      try Ok (Hashtbl.find (Lazy.force dune_extensions) extension_name)
      with Not_found ->
        Fmt.error_msg
          "@[<v>Could not find extension '%s'@ \
          Available extensions:@;<1 2>@[<v>%a@]@]"
          extension_name
          Fmt.(list (box pp)) (list ())
  in
  Result.bind extension_location (fun extension_location ->
    Ok { extension_name ; extension_location })
