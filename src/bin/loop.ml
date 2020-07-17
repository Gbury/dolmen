
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

module State = Dolmen_loop.State
module Typer = Dolmen_loop.Typer.Make(State)
module Pipeline = Dolmen_loop.Pipeline.Make(State)
module Pipe = Dolmen_loop.Pipes.Make(Dolmen.Expr)(State)(Typer)

