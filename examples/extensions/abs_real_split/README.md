# `abs_real_split` plugin

This is an example plugin adding support for an `abs_real` function. This
plugin implements the typing and model extensions for the `abs_real` function
in separate Dune plugins.

This is more complex than implementing `abs_real` as a single plugin, but
allows using the typing extension without a dependency on `dolmen_model`.
