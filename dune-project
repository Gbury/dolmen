(lang dune 3.0)
(name dolmen)
(using mdx 0.2)
(using menhir 2.0)
(using dune_site 0.1)
(implicit_transitive_deps true)

(package
  (name dolmen)
  (sites (lib plugins)))

(package
  (name dolmen_type))

(package
  (name dolmen_loop))

(package
  (name dolmen_model))

(package
  (name dolmen_bin))

(package
  (name dolmen_lsp))
