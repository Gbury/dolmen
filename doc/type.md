
# The Dolmen typing library

## Quick example

```ocaml
(* instantiate the modules for typechecking *)
module State = Dolmen_loop.State
module Typer_aux = Dolmen_loop.Typer.Typer(State)
module Typer = Dolmen_loop.Typer.Make(Dolmen.Std.Expr)(Dolmen.Std.Expr.Print)(State)(Typer_aux)

(* let's suppose we have a list of parsed statements, and a location table;
   both of which would typically be given by the parsing functions *)
let parsed, loc = [], Dolmen.Std.Loc.mk_file "file.smt2"
let logic_file = State.mk_file ~loc "some/dir/to" (`File "file.smt2")
let response_file = State.mk_file "" (`File "this is unused")

(* let's create the initial state *)
let state =
  State.empty
  |> State.init
     ~debug:false ~report_style:Regular ~max_warn:max_int
     ~reports:(Dolmen_loop.Report.Conf.mk ~default:Enabled)
     ~response_file
     (* these limits are ignored in this example; to actually enforce
        the limits, one has to use the `run` function from `Dolmen_loop.Pipeline` *)
     ~time_limit:0. ~size_limit:0.
  |> State.set State.logic_file logic_file
  |> Typer_aux.init
  |> Typer.init ~type_check:true

(* We can add some custom builtin theories to type extensions if we want *)
let state =
  (* We define a dummy builtin theory that does nothing *)
  let new_builtins _state _lang _env _symbol = `Not_found in
  (* We add it to the builtins used during typechecking (in addition to
     all the theories that will naturally be used depending on the `set-logic`
     statement) *)
  State.set Typer_aux.additional_builtins new_builtins state

(* Now we can iter on all the parsed statements *)
let () =
  let final_state, rev_typed_stmts =
    List.fold_left (fun (state, acc) parsed_stmt ->
      let state, typed_stmt = Typer.check state parsed_stmt in
      (state, typed_stmt :: acc)
    ) (state, []) parsed
  in
  let typed_stmts = List.rev rev_typed_stmts in
  List.iter (fun typed_stmt ->
    Format.printf "%a@." Typer.print typed_stmt
  ) typed_stmts
```

## Global architecture

Contrary to aprsing, where `dolmen` provides one parser for each language,
the typechecking part of `dolmen` provides one typechecker, which can be
parameterized depending on which language one wants to type-check.

TODO: write some more doc

