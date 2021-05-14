
(* This file is free software, part of dolmem. See file "LICENSE" for more information *)

%parameter <L : ParseLocation.S>
%parameter <T : Ast_dedukti.Term with type location := L.t>
%parameter <S : Ast_dedukti.Statement with type location := L.t and type term := T.t>

%right ARROW FATARROW

%%

prelude:
  | s=NAME DOT
    { let loc = L.mk_pos $startpos $endpos in S.module_start ~loc s }

line:
  | id=ID COLON t=term DOT
    { let loc = L.mk_pos $startpos $endpos in
      decl ~loc id t }
  | ID COLON term DEF term DOT
    { mk_definition (fst $1) (snd $1) (Some (scope_term [] $3)) (scope_term [] $5) }
  | ID DEF term DOT
    { mk_definition (fst $1) (snd $1)  None (scope_term [] $3) }
  | ID param+ COLON term DEF term DOT
    { mk_definition (fst $1) (snd $1) (Some (scope_term [] (mk_pi $4 $2)))
                        (scope_term [] (mk_lam $6 $2)) }
                | ID param+ DEF term DOT
                { mk_definition (fst $1) (snd $1) None (scope_term [] (mk_lam $4 $2)) }
                | LEFTBRA ID RIGHTBRA COLON term DEF term DOT
                { mk_opaque (fst $2) (snd $2) (Some (scope_term [] $5)) (scope_term [] $7) }
                | LEFTBRA ID RIGHTBRA DEF term DOT
                { mk_opaque (fst $2) (snd $2)  None (scope_term [] $5) }
                | LEFTBRA ID param+ RIGHTBRA COLON term DEF term DOT
                { mk_opaque (fst $2) (snd $2) (Some (scope_term [] (mk_pi $6 $3)))
                        (scope_term [] (mk_lam $8 $3)) }
                | LEFTBRA ID param+ RIGHTBRA DEF term DOT
                { mk_opaque (fst $2) (snd $2)  None (scope_term [] (mk_lam $6 $3)) }
                | rule+ DOT
                { mk_rules (List.map scope_rule $1) }
                | command DOT { $1 }
                | EOF
                { mk_ending () ; raise Tokens.EndOfFile }


command         : WHNF  term    { mk_command $1 (Whnf (scope_term [] $2)) }
                | HNF   term    { mk_command $1 (Hnf (scope_term [] $2)) }
                | SNF   term    { mk_command $1 (Snf (scope_term [] $2)) }
                | STEP  term    { mk_command $1 (OneStep (scope_term [] $2)) }
                | INFER term    { mk_command $1 (Infer (scope_term [] $2)) }
                | CONV  term  COMMA term { mk_command $1 (Conv (scope_term [] $2,scope_term [] $4)) }
                | CHECK term  COMMA term { mk_command $1 (Check (scope_term [] $2,scope_term [] $4)) }
                | PRINT STRING  { mk_command $1 (Print $2) }
                | GDT   ID      { mk_command $1 (Gdt (None,snd $2)) }
                | GDT   QID     { let (_,m,v) = $2 in mk_command $1 (Gdt (Some m,v)) }
                | OTHER term_lst { mk_command (fst $1) (Other (snd $1,List.map (scope_term []) $2)) }


term_lst        : term                                  { [$1] }
                | term COMMA term_lst                   { $1::$3 }

param           : LEFTPAR decl RIGHTPAR                 { $2 }

rule            : LEFTSQU context RIGHTSQU top_pattern LONGARROW term
                { let (l,id,args) = $4 in ( l , $2 , id , args , $6) }

decl           : ID COLON term         { (fst $1,snd $1,$3) }

context         : /* empty */          { [] }
                | separated_nonempty_list(COMMA, decl) { $1 }

top_pattern     : ID pattern_wp*        { (fst $1,snd $1,$2) }

pattern_wp      : ID
                        { PPattern (fst $1,None,snd $1,[]) }
                | QID
                        { let (l,md,id)=$1 in PPattern (l,Some md,id,[]) }
                | UNDERSCORE
                        { PJoker $1 }
                | LEFTBRA term RIGHTBRA
                        { PCondition $2 }
                | LEFTPAR pattern RIGHTPAR
                        { $2 }

pattern         : ID  pattern_wp+
                        { PPattern (fst $1,None,snd $1,$2) }
                | QID pattern_wp+
                        { let (l,md,id)=$1 in PPattern (l,Some md,id,$2) }
                | ID FATARROW pattern
                        { PLambda (fst $1,snd $1,$3) }
                | pattern_wp
                        { $1 }

sterm           : QID
                { let (l,md,id)=$1 in PreQId(l,md,id) }
                | ID
                { PreId (fst $1,snd $1) }
                | LEFTPAR term RIGHTPAR
                { $2 }
                | TYPE
                { PreType $1 }

term            : sterm+
                { mk_pre_from_list $1 }
                | ID COLON sterm+ ARROW term
                { PrePi (fst $1,Some (snd $1),mk_pre_from_list $3,$5) }
                | term ARROW term
                { PrePi (preterm_loc $1,None,$1,$3) }
                | ID FATARROW term
                { PreLam (fst $1,snd $1,None,$3) }
                | ID COLON sterm+ FATARROW term
                { PreLam (fst $1,snd $1,Some(mk_pre_from_list $3),$5) }

id:
  | id=ID
  { let loc = L.mk_pos $startpos $endpos in
    let md, name = id in
    T.mk ~loc (I.qualified I.term [md] name) }
%%
