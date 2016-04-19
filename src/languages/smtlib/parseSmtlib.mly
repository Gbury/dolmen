
(* This file is free software, part of dolmem. See file "LICENSE" for more information *)

%parameter <L : ParseLocation.S>
%parameter <T : Ast_smtlib.Term with type location := L.t>
%parameter <S : Ast_smtlib.Statement with type location := L.t and type term := T.t>

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
  | s=SYMBOL { let loc = L.mk_pos $startpos $endpos in T.const ~loc s }
;

spec_constant:
  | s=NUMERAL        { let loc = L.mk_pos $startpos $endpos in T.int ~loc s }
  | s=DECIMAL        { let loc = L.mk_pos $startpos $endpos in T.real ~loc s }
  | s=HEXADECIMAL    { let loc = L.mk_pos $startpos $endpos in T.hexa ~loc s }
  | s=BINARY         { let loc = L.mk_pos $startpos $endpos in T.binary ~loc s }
  | s=STRING         { let loc = L.mk_pos $startpos $endpos in T.const ~loc s }
;

s_expr:
  | c=spec_constant
    { c }
  | s=symbol
    { s }
  | s=KEYWORD
    { let loc = L.mk_pos $startpos $endpos in T.const ~loc s }
  | OPEN l=s_expr* CLOSE
    { let loc = L.mk_pos $startpos $endpos in T.sexpr ~loc l }
;

identifier:
  | symbol { $1 }
  | OPEN UNDERSCORE SYMBOL numeral_plus CLOSE
    { let loc = L.mk_pos $startpos $endpos in T.const ~loc ($3 ^ "_" ^ $4) }
;

sort:
  | identifier
    { $1 }
  | OPEN identifier sort+ CLOSE
    { let loc = L.mk_pos $startpos $endpos in T.app ~loc $2 $3 }
;

attribute_value:
  | spec_constant             { $1 }
  | symbol                    { $1 }
  | OPEN s_expr* CLOSE        { let loc = L.mk_pos $startpos $endpos in T.sexpr ~loc $2 }
;

attribute:
  | KEYWORD                    { ($1,None) }
  | KEYWORD attribute_value    { ($1,Some $2) }
;

qual_identifier:
  | identifier                       { $1 }
  | OPEN AS identifier sort CLOSE    { let loc = L.mk_pos $startpos $endpos in T.typed ~loc $3 $4 }
;

var_binding:
  | OPEN symbol term CLOSE    { let loc = L.mk_pos $startpos $endpos in T.typed ~loc $2 $3 }
;

sorted_var:
  | OPEN symbol sort CLOSE    { let loc = L.mk_pos $startpos $endpos in T.typed ~loc $2 $3 }
;

term:
  | spec_constant                                     { $1 }
  | qual_identifier                                   { let loc = L.mk_pos $startpos $endpos in T.app ~loc $1 [] }
  | OPEN qual_identifier term+ CLOSE                  { let loc = L.mk_pos $startpos $endpos in T.app ~loc $2 $3 }
  | OPEN LET OPEN var_binding+ CLOSE term CLOSE       { let loc = L.mk_pos $startpos $endpos in T.letin ~loc $4 $6 }
  | OPEN FORALL OPEN sorted_var+ CLOSE term CLOSE     { let loc = L.mk_pos $startpos $endpos in T.forall ~loc $4 $6 }
  | OPEN EXISTS OPEN sorted_var+ CLOSE term CLOSE     { let loc = L.mk_pos $startpos $endpos in T.exists ~loc $4 $6 }
  | OPEN ATTRIBUTE term attribute+ CLOSE              { let loc = L.mk_pos $startpos $endpos in T.attr ~loc $3 $4 }
;

command_option:
  | attribute    { $1 }
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
    { let loc = L.mk_pos $startpos $endpos in S.assume ~loc $3 }
  | OPEN CHECK_SAT CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.check_sat ~loc () }

  | OPEN SET_LOGIC SYMBOL CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.set_logic ~loc $3 }

  | OPEN GET_INFO info_flag CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.get_info ~loc $3 }
  | OPEN SET_INFO attribute CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.set_info ~loc $3 }

  | OPEN GET_OPTION KEYWORD CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.get_option ~loc $3 }
  | OPEN SET_OPTION command_option CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.set_option ~loc $3 }

  | OPEN DECLARE_SORT SYMBOL NUMERAL CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.new_type ~loc $3 (int_of_string $4) }
  | OPEN DEFINE_SORT SYMBOL OPEN symbol* CLOSE sort CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.type_alias ~loc $3 $5 $7 }
  | OPEN DECLARE_FUN SYMBOL OPEN sort* CLOSE sort CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.type_def ~loc $3 $5 $7 }
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
  | EOF { [] }
  | command file { $1 :: $2 }
;

input:
  | EOF { None }
  | command { Some $1 }

%%
