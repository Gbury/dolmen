# User-defined builtins

Dolmen supports user-defined builtins through extensions.

When using Dolmen as a library, extensions can be registered programmatically
using `Dolmen_loop.Typer.Ext.create` and `Dolmen_model.Ext.create`. They can
then be activated using the `extension_builtins` in `Dolmen_loop.Typer.init`
and `Dolmen_model.Loop.init` , or by adding to the
`Dolmen_loop.Typer.extension_builtins` and `Dolmen_model.Loop.builtins` state
keys.

When using Dolmen as a binary, extensions are loaded through Dune's plugin
mechanism; see the `abs_real` example plugin.  Plugins only need to register
the extension; the Dolmen binary will take care of activating them.

**Plugins must use their name (defined in the `plugin` stanza) to register
extensions, otherwise Dolmen will not activate the extensions.**
