
module Intf = struct
  module Lex = Dolmen_intf.Lex
  module Parse = Dolmen_intf.Parse
  module Location = Dolmen_intf.Location
  module Id = Dolmen_intf.Id
  module Ty = Dolmen_intf.Type
  module Tag = Dolmen_intf.Tag
  module Term = Dolmen_intf.Term
  module Stmt = Dolmen_intf.Stmt
  module Language = Dolmen_intf.Language
end

module State = Dolmen_std.State
module Misc = Dolmen_std.Misc
module Id = Dolmen_std.Id
module Tag = Dolmen_std.Tag
module Term = Dolmen_std.Term
module Expr = Dolmen_std.Expr
module Statement = Dolmen_std.Statement
module Normalize = Dolmen_std.Normalize
module Transformer = Dolmen_std.Transformer
module ParseLocation = Dolmen_std.ParseLocation

module Line = Dolmen_line

module Ae = Dolmen_ae
module Dimacs = Dolmen_dimacs
module ICNF = Dolmen_icnf
module Smtlib2 = Dolmen_smtlib2
module Tptp = Dolmen_tptp
module Zf = Dolmen_zf

module Logic = Dolmen_class.Logic

