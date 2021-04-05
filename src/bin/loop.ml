
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

module State = Dolmen_loop.State
module Pipeline = Dolmen_loop.Pipeline.Make(State)

module Parser = Dolmen_loop.Parser.Pipe(Dolmen.Std.Expr)(State)
module Header = Dolmen_loop.Headers.Pipe(State)
module Typer = struct
  module T = Dolmen_loop.Typer.Make(State)
  include T
  include Dolmen_loop.Typer.Pipe(Dolmen.Std.Expr)(Dolmen.Std.Expr.Print)(State)(T)
end

