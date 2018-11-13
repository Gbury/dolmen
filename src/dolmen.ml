
module Intf = struct
  module Lex = Dolmen_intf.Lex
  module Parse = Dolmen_intf.Parse
  module Location = Dolmen_intf.Location
  module Id = Dolmen_intf.Id
  module Term = Dolmen_intf.Term
  module Stmt = Dolmen_intf.Stmt
  module Language = Dolmen_intf.Language
end

module Misc = Dolmen_std.Misc
module Id = Dolmen_std.Id
module Term = Dolmen_std.Term
module Statement = Dolmen_std.Statement
module Normalize = Dolmen_std.Normalize
module Transformer = Dolmen_std.Transformer
module ParseLocation = Dolmen_std.ParseLocation

module Line = Dolmen_line
module Dimacs = Dolmen_dimacs
module ICNF = Dolmen_icnf
module Smtlib = Dolmen_smtlib
module Tptp = Dolmen_tptp
module Zf = Dolmen_zf

module Logic = Dolmen_class.Logic
