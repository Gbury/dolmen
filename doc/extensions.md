# User-defined builtins

Dolmen supports extensions through the form of user-defined builtins; see the
`abs_real` and `abs_real_split` plugins.

User-defined builtins can be added to the extensible `Builtin` type in
`Dolmen.Std.Builtin.t`.

Builtins need to be registered with the typer using `Dolmen_loop.Typer.Ext`,
and they optionally need to be registered with the model evaluation mechanism
(if they are to be used with model verification) using `Dolmen_model.Env.Ext`.

The `dolmen` command-line tool looks up user-defined extensions using the Dune
plugin mechanism. A plugin named `plugin.typing` will be picked up when
`--ext plugin` or `--ext plugin.typing` is provided on the command-line, and
the plugin must register a typing extension named `"plugin"` using
`Dolmen_loop.Typer.Ext.create`. A plugin named `plugin.model` will be picked up
when `--ext plugin` or `--ext plugin.model` is provided on the command-line and
the plugin must register a model extension named `"plugin"` using
`Dolmen_model.Ext.create`. A plugin named `plugin` (without dots) will be
picked up when either of the above command line flags is provided, and must
provide both a typing and model extension.

*Note*: Model extensions are only used (and their existence checked) if the
user provides the `--check-model` flag.
