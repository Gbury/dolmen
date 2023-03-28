
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

module State = Dolmen_loop.State
module Pipeline = Dolmen_loop.Pipeline.Make(State)

module Parser = Dolmen_loop.Parser.Make(State)
module Header = Dolmen_loop.Headers.Make(State)
module Flow = Dolmen_loop.Flow.Make(State)
module Typer = Dolmen_loop.Typer.Typer(State)
module Typer_Pipe = Dolmen_loop.Typer.Make(Dolmen.Std.Expr)(Dolmen.Std.Expr.Print)(State)(Typer)
module Check = Dolmen_model.Loop.Make(State)(Parser)(Typer)(Typer_Pipe)
