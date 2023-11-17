
(* This file is free software, part of dolmen. See file "LICENSE" for more information. *)

(* Functor instanciation *)
(* ************************************************************************* *)

module L = Dolmen.Class.Logic.Make
    (Dolmen.Std.Loc)
    (Dolmen.Std.Id)
    (Dolmen.Std.Term)
    (Dolmen.Std.Statement)
    (Dolmen.Std.Extensions)

(* Symbol/var printing *)
(* ************************************************************************* *)

let identifier
    ~print ~template ~name ~is_print_exn ~language ~gen =
  QCheck2.Test.make
    ~count:50 ~max_gen:500 ~long_factor:100
    ~print:(fun name -> Format.asprintf template print name)
    ~name gen
    (fun name ->
       let test =
         try Format.asprintf template print name
         with exn when is_print_exn exn ->
           QCheck2.assume_fail ()
       in
       let _, _, l = L.parse_all ~language (`Raw ("test", language, test)) in
       try
         let _ = Lazy.force l in
         true
       with
       | Dolmen.Std.Loc.Lexing_error (_loc, lex) ->
         QCheck2.Test.fail_reportf "lexing: invalid char: '%s'" lex
       | Dolmen.Std.Loc.Syntax_error (_loc, perr) ->
         begin match perr with
           | `Regular msg ->
             QCheck2.Test.fail_reportf "syntax: %t" msg
           | `Advanced (_error_ref, prod, lexed, expected) ->
             QCheck2.Test.fail_reportf
               "syntax: while parsing %t,@ read %t,@]@ @[<hov>but expected %t.@]@]"
                prod lexed expected
         end
    )

(* this test is mainly there to check that non-printable ids are correctly
   rejected by the printer, so we allow everything in the generator. *)
let smtlib2_id =
  identifier
    ~language:(Smtlib2 `V2_6)
    ~print:Dolmen.Smtlib2.Script.V2_6.Print.id
    ~gen:(Generators.name ~printable:false
         ~simple:true ~indexed:true ~qualified:true)
    ~template:{|(assert %a)|}
    ~name:"Print.smtlib2_6.id"
    ~is_print_exn:(function
        | Dolmen.Smtlib2.Script.V2_6.Print.Cannot_print _ -> true
        | _ -> false)

(* this test is mainly there to check that printing of printable ids is correct,
   and is accepted by the parser, so we restrict the generated names to one that
   have reasonablke chances of being printed. *)
let smtlib2_id_printable =
  identifier
    ~language:(Smtlib2 `V2_6)
    ~print:Dolmen.Smtlib2.Script.V2_6.Print.id
    ~gen:(Generators.name ~printable:true
            ~simple:true ~indexed:true ~qualified:false)
    ~template:{|(assert %a)|}
    ~name:"Print.smtlib2_6.id_printable"
    ~is_print_exn:(function
        | Dolmen.Smtlib2.Script.V2_6.Print.Cannot_print _ -> true
        | _ -> false)

(* this test is mainly there to check that non-printable ids are correctly
   rejected by the printer, so we allow everything in the generator. *)
let poly_smtlib2_id =
  identifier
    ~language:(Smtlib2 `Poly)
    ~print:Dolmen.Smtlib2.Script.Poly.Print.id
    ~gen:(Generators.name ~printable:false
         ~simple:true ~indexed:true ~qualified:true)
    ~template:{|(assert %a)|}
    ~name:"Print.smtlib2_poly.id"
    ~is_print_exn:(function
        | Dolmen.Smtlib2.Script.Poly.Print.Cannot_print _ -> true
        | _ -> false)

(* this test is mainly there to check that printing of printable ids is correct,
   and is accepted by the parser, so we restrict the generated names to one that
   have reasonablke chances of being printed. *)
let poly_smtlib2_id_printable =
  identifier
    ~language:(Smtlib2 `V2_6)
    ~print:Dolmen.Smtlib2.Script.Poly.Print.id
    ~gen:(Generators.name ~printable:true
            ~simple:true ~indexed:true ~qualified:false)
    ~template:{|(assert %a)|}
    ~name:"Print.smtlib2_poly.id_printable"
    ~is_print_exn:(function
        | Dolmen.Smtlib2.Script.Poly.Print.Cannot_print _ -> true
        | _ -> false)

