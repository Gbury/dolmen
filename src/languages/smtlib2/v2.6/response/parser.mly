
(* This file is free software, part of dolmem. See file "LICENSE" for more information *)

%parameter <L : Dolmen_intf.Location.S>
%parameter <I : Ast.Id>
%parameter <T : Ast.Term with type location := L.t and type id := I.t>
%parameter <S : Ast.Statement with type location := L.t and type id := I.t and type term := T.t>

%start <T.t> term
%start <S.t list> file
%start <S.t option> input

%{

  let pp_num_list fmt (l, singular, plural) =
    let n = List.length l in
    Format.fprintf fmt "%d %s" n (if n = 1 then singular else plural)

  let mismatched_lists ~loc l1 l2 =
    let msg = Format.dprintf
      "@[<v>@[<hov>Expected@ two@ lists@ of@ the@ same@ size,@ but@ got:@]@ \
        - @[<hov>%a@]@ - @[<hov>%a@]@]"
      pp_num_list l1 pp_num_list l2
    in
    raise (L.Syntax_error (loc, `Regular msg))

%}

%%

spec_constant:
  | s=NUM
    { let loc = L.mk_pos $startpos $endpos in T.int ~loc s }
  | s=DEC
    { let loc = L.mk_pos $startpos $endpos in T.real ~loc s }
  | s=HEX
    { let loc = L.mk_pos $startpos $endpos in T.hexa ~loc s }
  | s=BIN
    { let loc = L.mk_pos $startpos $endpos in T.binary ~loc s }
  | s=STR
    { let loc = L.mk_pos $startpos $endpos in T.str ~loc s }
;

reserved:
  /* these are currently unused, see lexer.
   * | BINARY { "BINARY" }
   * | DECIMAL { "DECIMAL" }
   * | HEXADECIMAL { "HEXADECIMAL" }
   * | NUMERAL { "NUMERAL" }
   * | STRING { "STRING" }
   */
  | UNDERSCORE { "_" }
  | ATTRIBUTE { "!" }
  | AS { "as" }
  | LET { "let" }
  | EXISTS { "exists" }
  | FORALL { "forall" }
  | MATCH { "match" }
  | SAT { "sat" }
  | UNSAT { "unsat" }
  | MODEL { "model" }
  | DEFINE_FUN { "define-fun" }
  | DEFINE_FUN_REC { "define-fun-rec" }
  | DEFINE_FUNS_REC { "define-funs-rec" }
;

s_expr:
  | c=spec_constant
    { c }
  | s=SYMBOL
    { let loc = L.mk_pos $startpos $endpos in T.const ~loc I.(mk term s) }
  | s=reserved
    { let loc = L.mk_pos $startpos $endpos in T.const ~loc I.(mk term s) }
  | s=KEYWORD
    { let loc = L.mk_pos $startpos $endpos in T.const ~loc I.(mk term s) }
  | OPEN l=s_expr* CLOSE
    { let loc = L.mk_pos $startpos $endpos in T.sexpr ~loc l }
;

index:
  | s=NUM
  | s=SYMBOL
    { s }
  /* Small language extension to support string char literals */
  | s=HEX
    { s }
;

identifier:
  | s=SYMBOL
    { fun ns -> I.mk ns s }
  | OPEN UNDERSCORE s=SYMBOL l=index+ CLOSE
    { fun ns -> I.indexed ns s l }
;

sort:
  | s=identifier
    { let loc = L.mk_pos $startpos $endpos in T.const ~loc (s I.sort) }
  | OPEN f=identifier args=sort+ CLOSE
    { let c =
        let loc = L.mk_pos $startpos(f) $endpos(f) in
        T.const ~loc (f I.sort)
      in
      let loc = L.mk_pos $startpos $endpos in T.apply ~loc c args }
;

attribute_value:
  | v=spec_constant
    { v }
  | v=SYMBOL
    { let loc = L.mk_pos $startpos $endpos in T.const ~loc I.(mk attr v) }
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
        T.apply ~loc t [t']
    }
;

/*
The [(as id ty)] doesn't specify the type of the function [id]
but only its result type
*/
qual_identifier:
  | s=identifier
    { let loc = L.mk_pos $startpos $endpos in `NoAs (T.const ~loc (s I.term)) }
  | OPEN AS s=identifier ty=sort CLOSE
    { let loc = L.mk_pos $startpos(s) $endpos(s) in
      let as_loc = L.mk_pos $startpos $endpos in
      `As (T.const ~loc (s I.term), ty, as_loc) }
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

/* Additional rule for pattern symbols, useful for:
   1- locations in symbol lists in patterns,
   2- menhir '+' syntax doesn't support raw tokens afaik */
pattern_symbol:
  | s=SYMBOL
    { let loc = L.mk_pos $startpos $endpos in T.const ~loc I.(mk term s) }
;

pattern:
  | c=pattern_symbol
    { c }
  | OPEN f=pattern_symbol args=pattern_symbol+ CLOSE
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc f args }
;

match_case:
  | OPEN p=pattern t=term CLOSE
    { p, t }
;

term:
  | c=spec_constant
    { c }
  | s=qual_identifier
    { match s with
      | `NoAs f -> f
      | `As (f, ty, loc) -> T.colon ~loc f ty }
  | OPEN s=qual_identifier args=term+ CLOSE
    { let loc = L.mk_pos $startpos $endpos in
      match s with
      | `NoAs f -> T.apply ~loc f args
      | `As (f, ty, as_loc) -> T.colon ~loc:as_loc (T.apply ~loc f args) ty }
  | OPEN LET OPEN l=var_binding+ CLOSE t=term CLOSE
    { let loc = L.mk_pos $startpos $endpos in T.letand ~loc l t }
  | OPEN FORALL OPEN l=sorted_var+ CLOSE t=term CLOSE
    { let loc = L.mk_pos $startpos $endpos in T.forall ~loc l t }
  | OPEN EXISTS OPEN l=sorted_var+ CLOSE t=term CLOSE
    { let loc = L.mk_pos $startpos $endpos in T.exists ~loc l t }
  | OPEN MATCH t=term OPEN l=match_case+ CLOSE CLOSE
    { let loc = L.mk_pos $startpos $endpos in T.match_ ~loc t l }
  | OPEN ATTRIBUTE f=term args=attribute+ CLOSE
    { let loc = L.mk_pos $startpos $endpos in T.annot ~loc f args }
;

function_dec:
  | OPEN s=SYMBOL OPEN args=sorted_var* CLOSE ret=sort CLOSE
    { I.(mk term s), [], args, ret }

function_def:
  | s=SYMBOL OPEN args=sorted_var* CLOSE ret=sort body=term
    { I.(mk term s), [], args, ret, body }

definition:
  | OPEN DEFINE_FUN f=function_def CLOSE
    { let id, vars, args, ret, body = f in
      let loc = L.mk_pos $startpos $endpos in
      S.fun_def ~loc id vars args ret body }
  | OPEN DEFINE_FUN_REC f=function_def CLOSE
    { let id, vars, args, ret, body = f in
      let loc = L.mk_pos $startpos $endpos in
      S.funs_def_rec ~loc [id, vars, args, ret, body] }
  /* The syntax technically defines this reduction as having l and l' be the same length,
      but that isn't easily expressible in menhir, so the check is delayed */
  | OPEN DEFINE_FUNS_REC OPEN l1=function_dec+ CLOSE OPEN l2=term+ CLOSE CLOSE
    { let res =
        try List.map2 (fun (id, vars, args, ret) body -> id, vars, args, ret, body) l1 l2
        with Invalid_argument _ ->
          let loc = L.mk_pos $startpos($3) $endpos($8) in
          mismatched_lists ~loc
            (l1, "function declaration", "function declarations")
            (l2, "function body", "function bodies")
      in
      let loc = L.mk_pos $startpos $endpos in
      S.funs_def_rec ~loc res }
;

answer:
  | UNSAT
    { let loc = L.mk_pos $startpos $endpos in
      S.unsat ~loc () }
  | SAT OPEN MODEL? model=definition* CLOSE
    { let loc = L.mk_pos $startpos $endpos in
      S.sat ~loc (Some model) }

file:
  | l=answer* EOF
    { l }
;

input:
  | EOF
    { None }
  | c=answer
    { Some c }

%%
