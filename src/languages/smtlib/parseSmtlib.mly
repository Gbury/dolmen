
(* This file is free software, part of dolmem. See file "LICENSE" for more information *)

%parameter <L : ParseLocation.S>
%parameter <T : Ast_smtlib.Term with type location := L.t>
%parameter <S : Ast_smtlib.Statement with type location := L.t and type term := T.t>

%type <T.namespace -> T.t> spec_constant symbol identifier

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

symbol:
  | s=SYMBOL
    { fun ns -> let loc = L.mk_pos $startpos $endpos in T.const ~loc ~ns s }
;

spec_constant:
  | s=NUMERAL
    { fun ns -> let loc = L.mk_pos $startpos $endpos in T.int ~loc ~ns s }
  | s=DECIMAL
    { fun ns -> let loc = L.mk_pos $startpos $endpos in T.real ~loc ~ns s }
  | s=HEXADECIMAL
    { fun ns -> let loc = L.mk_pos $startpos $endpos in T.hexa ~loc ~ns s }
  | s=BINARY
    { fun ns -> let loc = L.mk_pos $startpos $endpos in T.binary ~loc ~ns s }
  | s=STRING
    { fun ns -> let loc = L.mk_pos $startpos $endpos in T.const ~loc ~ns s }
;

s_expr:
  | c=spec_constant
    { c T.term }
  | s=symbol
    { s T.term }
  | s=KEYWORD
    { let loc = L.mk_pos $startpos $endpos in T.const ~loc ~ns:T.term s }
  | OPEN l=s_expr* CLOSE
    { let loc = L.mk_pos $startpos $endpos in T.sexpr ~loc l }
;

identifier:
  | c=symbol
    { c }
  | OPEN UNDERSCORE s=SYMBOL n=numeral_plus CLOSE
    { fun ns -> let loc = L.mk_pos $startpos $endpos in T.const ~loc ~ns (s ^ "_" ^ n) }
;

sort:
  | s=identifier
    { s T.sort }
  | OPEN f=identifier args=sort+ CLOSE
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc (f T.sort) args }
;

attribute_value:
  | v=symbol
  | v=spec_constant
    { v T.attr }
  | OPEN l=s_expr* CLOSE
    { let loc = L.mk_pos $startpos $endpos in T.sexpr ~loc l }
;

attribute:
  | s=KEYWORD a=attribute_value?
    {
      let loc = L.mk_pos $startpos(s) $endpos(s) in
      let t = T.const ~loc ~ns:T.attr s in
      match a with
      | None -> t
      | Some t' ->
        let loc = L.mk_pos $startpos $endpos in
        T.colon ~loc t t'
    }
;

qual_identifier:
  | s=identifier
    { s T.term }
  | OPEN AS s=identifier ty=sort CLOSE
    { let loc = L.mk_pos $startpos $endpos in T.colon ~loc (s T.term) ty }
;

var_binding:
  | OPEN s=symbol t=term CLOSE
    { let loc = L.mk_pos $startpos $endpos in T.colon ~loc (s T.term) t }
;

sorted_var:
  | OPEN s=symbol ty=sort CLOSE
    { let loc = L.mk_pos $startpos $endpos in T.colon ~loc (s T.sort) ty }
;

term:
  | c=spec_constant
    { c T.term }
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

info_flag:
  | KEYWORD    { $1 }
;

command:
  | OPEN POP NUMERAL CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.pop ~loc (int_of_string $3) }
  | OPEN PUSH NUMERAL CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.push ~loc (int_of_string $3) }

  | OPEN ASSERT term CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.assert_ ~loc $3 }
  | OPEN CHECK_SAT CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.check_sat ~loc () }

  | OPEN SET_LOGIC SYMBOL CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.set_logic ~loc $3 }

  | OPEN GET_INFO info_flag CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.get_info ~loc $3 }
  | OPEN SET_INFO command_option CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.set_info ~loc $3 }

  | OPEN GET_OPTION KEYWORD CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.get_option ~loc $3 }
  | OPEN SET_OPTION command_option CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.set_option ~loc $3 }

  | OPEN DECLARE_SORT SYMBOL NUMERAL CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.type_decl ~loc $3 (int_of_string $4) }
  | OPEN DEFINE_SORT SYMBOL OPEN symbol* CLOSE sort CLOSE
    { let l = List.map ((|>) T.term) $5 in
      let loc = L.mk_pos $startpos $endpos in S.type_def ~loc $3 l $7 }
  | OPEN DECLARE_FUN SYMBOL OPEN sort* CLOSE sort CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.fun_decl ~loc $3 $5 $7 }
  | OPEN DEFINE_FUN SYMBOL OPEN sorted_var* CLOSE sort term CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.fun_def ~loc $3 $5 $7 $8 }

  | OPEN GET_PROOF CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.get_proof ~loc () }
  | OPEN GET_VALUE OPEN term+ CLOSE CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.get_value ~loc $4 }
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
