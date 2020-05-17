
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

module Typer = Dolmen_loop.Typer.Make(Dolmen_loop.State.Aux)

module State = struct
  include Dolmen.State
  include Dolmen_loop.State.Make(Typer)
end

module Pipeline = Dolmen_loop.Pipeline.Make(State)
module Pipe = Dolmen_loop.Pipes.Make(Dolmen.Expr)(State)(Typer)

