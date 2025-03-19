
(* Re-export manually the sub libs/modules *)
module Std = Dolmen_std
module Intf = Dolmen_intf

(* Languages *)
module Line = Dolmen_line
module Ae = Dolmen_ae
module Dimacs = Dolmen_dimacs
module ICNF = Dolmen_icnf
module Tptp = Dolmen_tptp
module Smtlib2 = Dolmen_smtlib2
module Zf = Dolmen_zf

(* Classes *)
module Class = Dolmen_class

(* Extensions *)
module Sites = Sites
(** Dune-site module to access the sites of the [dolmen] package.

    {b Note}: This is an auto-generated module using the dune-site's
    [(generate_sites_module)]. Use with caution.
*)
