
(* This file is free software, part of dolmem. See file "LICENSE" for more information *)

%parameter <L : ParseLocation.S>
%parameter <I : Ast_smtlib.Id>
%parameter <T : Ast_smtlib.Term with type location := L.t and type id := I.t>
%parameter <S : Ast_smtlib.Statement with type location := L.t and type id := I.t and type term := T.t>

%start <T.t> term
%start <S.t list> file
%start <S.t option> input

%%

numeral_plus:
  | s=NUMERAL
    { s }
  | s=NUMERAL n=numeral_plus
    { s ^ "_" ^ n }
;

spec_constant:
  | s=NUMERAL
    { fun _ -> let loc = L.mk_pos $startpos $endpos in T.int ~loc s }
  | s=DECIMAL
    { fun _ -> let loc = L.mk_pos $startpos $endpos in T.real ~loc s }
  | s=HEXADECIMAL
    { fun _ -> let loc = L.mk_pos $startpos $endpos in T.hexa ~loc s }
  | s=BINARY
    { fun _ -> let loc = L.mk_pos $startpos $endpos in T.binary ~loc s }
  | s=STRING
    { fun ns -> let loc = L.mk_pos $startpos $endpos in T.const ~loc I.(mk ns s) }
;

s_expr:
  | c=spec_constant
    { c I.attr }
  | s=SYMBOL
    { let loc = L.mk_pos $startpos $endpos in T.const ~loc I.(mk term s) }
  | s=KEYWORD
    { let loc = L.mk_pos $startpos $endpos in T.const ~loc I.(mk term s) }
  | OPEN l=s_expr* CLOSE
    { let loc = L.mk_pos $startpos $endpos in T.sexpr ~loc l }
;

identifier:
  | s=SYMBOL
    { s }
  | OPEN UNDERSCORE s=SYMBOL n=numeral_plus CLOSE
    { s ^ "_" ^ n }
;

sort:
  | s=identifier
    { let loc = L.mk_pos $startpos $endpos in T.const ~loc I.(mk sort s) }
  | OPEN f=identifier args=sort+ CLOSE
    { let c =
        let loc = L.mk_pos $startpos(f) $endpos(f) in
        T.const ~loc I.(mk sort f)
      in
      let loc = L.mk_pos $startpos $endpos in T.apply ~loc c args }
;

attribute_value:
  | v=spec_constant
    { v I.attr }
  | v=SYMBOL
    { let loc = L.mk_pos $startpos $endpos in T.const I.(mk attr v) }
  | OPEN l=s_expr* CLOSE
    { let loc = L.mk_pos $startpos $endpos in T.sexpr ~loc l }
;

attribute:
  | s=KEYWORD a=attribute_value?
    {
      let t =
        let loc = L.mk_pos $startpos(s) $endpos(s) in
        T.const ~loc I.(mk attr s)
      in
      match a with
      | None -> t
      | Some t' ->
        let loc = L.mk_pos $startpos $endpos in
        T.colon ~loc t t'
    }
;

qual_identifier:
  | s=identifier
    { let loc = L.mk_pos $startpos $endpos in T.const I.(mk term s) }
  | OPEN AS s=identifier ty=sort CLOSE
    { let c =
        let loc = L.mk_pos $startpos(s) $endpos(s) in
        T.const ~loc I.(mk term s)
      in
      let loc = L.mk_pos $startpos $endpos in T.colon ~loc c ty }
;

var_binding:
  | OPEN s=SYMBOL t=term CLOSE
    { let c =
        let loc = L.mk_pos $startpos(s) $endpos(s) in
        T.const ~loc I.(mk term s)
      in
      let loc = L.mk_pos $startpos $endpos in T.colon ~loc c t }
;

sorted_var:
  | OPEN s=SYMBOL ty=sort CLOSE
    { let c =
        let loc = L.mk_pos $startpos(s) $endpos(s) in
        T.const ~loc I.(mk term s)
      in
      let loc = L.mk_pos $startpos $endpos in T.colon ~loc c ty }
;

term:
  | c=spec_constant
    { c I.term }
  | s=qual_identifier
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc s [] }
  | OPEN f=qual_identifier args=term+ CLOSE
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc f args }
  | OPEN LET OPEN l=var_binding+ CLOSE t=term CLOSE
    { let loc = L.mk_pos $startpos $endpos in T.letin ~loc l t }
  | OPEN FORALL OPEN l=sorted_var+ CLOSE t=term CLOSE
    { let loc = L.mk_pos $startpos $endpos in T.forall ~loc l t }
  | OPEN EXISTS OPEN l=sorted_var+ CLOSE t=term CLOSE
    { let loc = L.mk_pos $startpos $endpos in T.exists ~loc l t }
  | OPEN ATTRIBUTE f=term args=attribute+ CLOSE
    { let loc = L.mk_pos $startpos $endpos in T.annot ~loc f args }
;

command_option:
  | s=KEYWORD t=attribute_value?
    { (s, t) }
;

command:
  | OPEN POP n=NUMERAL CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.pop ~loc (int_of_string n) }
  | OPEN PUSH n=NUMERAL CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.push ~loc (int_of_string n) }

  | OPEN ASSERT t=term CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.assert_ ~loc t }
  | OPEN CHECK_SAT CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.check_sat ~loc () }

  | OPEN SET_LOGIC s=SYMBOL CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.set_logic ~loc s }

  | OPEN GET_INFO i=KEYWORD CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.get_info ~loc i }
  | OPEN SET_INFO c=command_option CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.set_info ~loc c }

  | OPEN GET_OPTION k=KEYWORD CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.get_option ~loc k }
  | OPEN SET_OPTION c=command_option CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.set_option ~loc c }

  | OPEN DECLARE_SORT s=SYMBOL n=NUMERAL CLOSE
    { let id = I.(mk sort s) in
      let loc = L.mk_pos $startpos $endpos in
      S.type_decl ~loc id (int_of_string n) }
  | OPEN DEFINE_SORT s=SYMBOL OPEN args=SYMBOL* CLOSE ty=sort CLOSE
    { let id = I.(mk sort s) in
      let l = List.map I.(mk sort) args in
      let loc = L.mk_pos $startpos $endpos in
      S.type_def ~loc id l ty }
  | OPEN DECLARE_FUN s=SYMBOL OPEN args=sort* CLOSE ty=sort CLOSE
    { let id = I.(mk term s) in
      let loc = L.mk_pos $startpos $endpos in
      S.fun_decl ~loc id args ty }
  | OPEN DEFINE_FUN s=SYMBOL OPEN args=sorted_var* CLOSE ret=sort body=term CLOSE
    { let id = I.(mk term s) in
      let loc = L.mk_pos $startpos $endpos in
      S.fun_def ~loc id args ret body }

  | OPEN GET_PROOF CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.get_proof ~loc () }
  | OPEN GET_VALUE OPEN l=term+ CLOSE CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.get_value ~loc l }
  | OPEN GET_ASSERTIONS CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.get_assertions ~loc () }
  | OPEN GET_UNSAT_CORE CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.get_unsat_core ~loc () }
  | OPEN GET_ASSIGNMENT CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.get_assignment ~loc () }

  | OPEN EXIT CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.exit ~loc () }
;

file:
  | EOF
    { [] }
  | c=command l=file
    { c :: l }
;

input:
  | EOF
    { None }
  | c=command
    { Some c }

%%
