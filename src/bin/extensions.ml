(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

type t = string
(** We only store the name of the extension. *)

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

let pp ppf plugin =
  let pp_location ppf plugin =
    if Hashtbl.mem builtin_extensions plugin
    then
      Fmt.pf ppf "@ (builtin)"
  in
  Fmt.pf ppf "@[%s%a@]" plugin pp_location plugin

let name plugin = plugin

let list () =
  let all_extensions =
    Hashtbl.fold
      (fun extension_name () exts ->
        extension_name :: exts)
      builtin_extensions
      []
  in
  let all_extensions =
    List.fold_left (fun all_extensions plugin ->
      if Hashtbl.mem builtin_extensions plugin
      then
        all_extensions
      else
        plugin :: all_extensions
    ) all_extensions (Dolmen.Sites.Plugins.Plugins.list ())
  in
  List.fast_sort
    (fun e1 e2 -> String.compare (name e1) (name e2))
    all_extensions

let list_conflicts () =
  List.filter
    (Hashtbl.mem builtin_extensions)
    (Dolmen.Sites.Plugins.Plugins.list ())

let loaded_plugins =
  Hashtbl.create 17

let dynlink_error =
  let open Dolmen_loop in
  Report.Error.mk ~mnemonic:"dynlink-plugin-error"
    ~message:(fun ppf (plugin, err) ->
      Fmt.pf ppf "Unable to load plugin library '%s':@ %s"
        plugin (Dynlink.error_message err))
    ~name:"Dynlink error" ()

let list_available_extensions (_plugin, _exn) =
  Some (
    Format.dprintf
      "Available extensions are:@ %a"
      Fmt.(list ~sep:Fmt.comma (box pp)) (list ())
  )

let dune_plugin_error =
  Dolmen_loop.Report.Error.mk ~mnemonic:"dune-plugin-error"
    ~hints:[list_available_extensions]
    ~message:(fun ppf (plugin, exn) ->
      Fmt.pf ppf "Unknown error while loading plugin '%s':@ %s"
        plugin (Printexc.to_string exn))
    ~name:"Dune plugin error" ()

let load_plugin_or_fail plugin st =
  if Hashtbl.mem loaded_plugins plugin then st
  else
    (* Ideally, we'd like to check for a conflict with a dune plugin here,
       but there is no nice way to check if a plugin of a given name exists
       without actually trying to load the plugin. *)
    if Hashtbl.mem builtin_extensions plugin
    then (
      Hashtbl.replace loaded_plugins plugin ();
      st
    )
    else
      match Dolmen.Sites.Plugins.Plugins.load plugin with
      | () ->
        Hashtbl.replace loaded_plugins plugin ();
        st
      | exception Dynlink.Error err ->
        Loop.State.error st dynlink_error (plugin, err)
      | exception exn ->
        (* Use an error rather than a warning here because, while it is
            likely that the plugin simply doesn't exist, but this could also be
            caused by a buggy plugin that might leave things in a partially
            initialized state. *)
        Loop.State.error st dune_plugin_error (plugin, exn)

let add_typing_extensions exts st =
  Loop.State.update Loop.Typer.extension_builtins (List.append exts) st

let load_typing_extension plugin st =
  let st = load_plugin_or_fail plugin st in
  match Dolmen_loop.Typer.Ext.find_all plugin with
  | [] -> st
  | exts  -> add_typing_extensions exts st

let add_model_extensions exts st =
  Loop.State.update Loop.Check.builtins
   (fun bs ->
     Dolmen_model.Eval.builtins
       (List.append (List.map Dolmen_model.Ext.builtins exts) [ bs ]))
   st

let load_model_extension plugin st =
  let st = load_plugin_or_fail plugin st in
  match Dolmen_model.Ext.find_all plugin with
  | [] -> st
  | exts  -> add_model_extensions exts st

let parse extension_name = Ok extension_name
