
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
  | PAR { "par" }
  | ASSERT { "assert" }
  | CHECK_SAT { "check-sat" }
  | CHECK_SAT_ASSUMING { "check-sat-assuming" }
  | DECLARE_CONST { "declare-const" }
  | DECLARE_DATATYPE { "declare-datatype" }
  | DECLARE_DATATYPES { "declare-datatypes" }
  | DECLARE_FUN { "declare-fun" }
  | DECLARE_SORT { "declare-sort" }
  | DEFINE_FUN { "define-fun" }
  | DEFINE_FUN_REC { "define-fun-rec" }
  | DEFINE_FUNS_REC { "define-funs-rec" }
  | DEFINE_SYS { "define-system" }
  | CHECK_SYS { "check-system" }
  | DEFINE_SORT { "define-sort" }
  | ECHO { "echo" }
  | EXIT { "exit" }
  | GET_ASSERTIONS { "get-assertions" }
  | GET_ASSIGNMENT { "get-assignment" }
  | GET_INFO { "get-info" }
  | GET_MODEL { "get-model" }
  | GET_OPTION { "get-option" }
  | GET_PROOF { "get-proof" }
  | GET_UNSAT_ASSUMPTIONS { "get-unsat-assumptions" }
  | GET_UNSAT_CORE { "get-unsat-core" }
  | GET_VALUE { "get-value" }
  | POP { "pop" }
  | PUSH { "push" }
  | RESET { "reset" }
  | RESET_ASSERTIONS { "reset-assertions" }
  | SET_INFO { "set-info" }
  | SET_LOGIC { "set-logic" }
  | SET_OPTION { "set-option" }
  | SYS_INPUT { ":input" }
  | SYS_OUTPUT { ":output" }
  | SYS_LOCAL { ":local" }
  | SYS_SUBSYS { ":subsys" }
  | SYS_INIT { ":init" }
  | SYS_TRANS { ":trans" }
  | SYS_INV { ":inv" }
  | CHECK_REACH { ":reachable" }
  | CHECK_ASSUMPTION { ":assumption" }
  | CHECK_QUERY { ":query" }
  | CHECK_QUERIES { ":queries" }
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

info_flag:
  /* The following cases are subsumed by the last case, and thus ignored,
     most notably because they would force to introduce tokens for specific
     keywords even though these rules are syntaxically useless.
  | :all-statistics
  | :assertion-stack-levels
  | :authors
  | :error-behavior
  | :name
  | :reason-unknown
  | :version
  */
  | s=KEYWORD
    { s }
;

/* This definition is useless (not used in the syntax),
   and it would force to match on non-reserved symbols,
   which is very, very, very ugly...
b_value:
  | true
  | false
;
*/

/* renamed from option to avoid a name_clash */
command_option:
  /* These cases are subsumed by the last case, and thus ignored,
    most notably because they would force to introduce tokens for specific
    keywords even though these rules are syntaxically useless.
    Also, this allows to ignore the definition of <b_value>, which is problematic.
  | :diagnostic-output-channel <string>
  | :global-declarations <b_value>
  | :interactive-mode <b_value>
  | :print-success <b_value>
  | :produce-assertions <b_value>
  | :produce-assignments <b_value>
  | :produce-models <b_value>
  | :produce-proofs <b_value>
  | :produce-unsat-assumptions <b_value>
  | :produce-unsat-cores <b_value>
  | :random-seed <numeral>
  | :regular-output-channel <string>
  | :reproducible-resource-limit <numeral>
  | :verbosity <numeral>
  */
  | a=attribute
    { a }
;

sort_dec:
  | OPEN s=SYMBOL n=NUM CLOSE
    { I.(mk sort s), int_of_string n
      (* shouldn't raise because of the definition of numeral in lexer *) }
;

selector_dec:
  | OPEN s=SYMBOL ty=sort CLOSE
    { let f =
      let loc = L.mk_pos $startpos $endpos in
      T.const ~loc (I.mk I.term s)
    in
    let loc = L.mk_pos $startpos $endpos in
    T.colon ~loc f ty }
;

enum_constructor_dec:
  | s=SYMBOL 
    { (I.mk I.term s), [] }

constructor_dec:
  | OPEN s=SYMBOL l=selector_dec* CLOSE
    { (I.mk I.term s), l }
;

/* Additional rule for datatype symbols, useful because
   menhir '+' syntax does'nt support raw tokens afaik */
datatype_symbol:
  | s=SYMBOL
    { let loc = L.mk_pos $startpos $endpos in T.const ~loc I.(mk sort s) }

datatype_dec:
  | OPEN l=constructor_dec+ CLOSE
    { [], l }
  | OPEN PAR OPEN vars=datatype_symbol+ CLOSE OPEN l=constructor_dec+ CLOSE CLOSE
    { vars, l }
;

function_dec:
  | OPEN s=SYMBOL OPEN args=sorted_var* CLOSE ret=sort CLOSE
    { I.(mk term s), [], args, ret }

function_def:
  | s=SYMBOL OPEN args=sorted_var* CLOSE ret=sort body=term
    { I.(mk term s), [], args, ret, body }

input_var_decl:
  | SYS_INPUT OPEN vars=sorted_var* CLOSE 
  { L.mk_pos $startpos $endpos, vars }

output_var_decl:
  | SYS_OUTPUT OPEN vars=sorted_var* CLOSE
  { L.mk_pos $startpos $endpos, vars }

local_var_decl:
  | SYS_LOCAL OPEN vars=sorted_var* CLOSE
  { L.mk_pos $startpos $endpos, vars }

system_var_decls_lst:
  | i=input_var_decl others=system_var_decls_lst
  { let input, output, local = others in
    i :: input, output, local
  }
  | o=output_var_decl others=system_var_decls_lst
  { let input, output, local = others in
    input, o :: output, local 
  }
  | l=local_var_decl others=system_var_decls_lst
  { let input, output, local = others in
    input, output, l :: local 
  }
  | i=input_var_decl
  { [i], [], [] }
  | o=output_var_decl
  { [], [o], [] }
  | l=local_var_decl
  { [], [], [l] }

system_var_decls:
  | decls=system_var_decls_lst
  {
    let get_system_var_decl var_decl attr_name =
      match var_decl with
      | [] -> []
      | [(_, d)] -> d
      | _ :: (loc, _) :: _ ->
        let msg = Format.dprintf "%a attribute is not repeatable"
          Format.pp_print_text attr_name
        in
        raise (L.Syntax_error (loc, `Regular msg))
    in 
    let input, output, local = decls in
    get_system_var_decl input ":input",
    get_system_var_decl output ":output",
    get_system_var_decl local ":local"
  }

opt_system_var_decls:
  | { [], [], [] }
  | decls=system_var_decls { decls }

system_instantiation:
  | c=pattern_symbol
    { c }
  | OPEN f=pattern_symbol args=pattern_symbol+ CLOSE
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc f args }

system_subsys_dec:
  | SYS_SUBSYS OPEN local_name=SYMBOL sys_inst=system_instantiation CLOSE
  { I.(mk term local_name), sys_inst }

init_cond:
  | SYS_INIT cond=term
  { L.mk_pos $startpos $endpos, cond }

trans_cond:
  | SYS_TRANS cond=term
  { L.mk_pos $startpos $endpos, cond }

inv_cond:
  | SYS_INV cond=term
  { L.mk_pos $startpos $endpos, cond }

subs_and_conds_lst:
  | sub=system_subsys_dec others=subs_and_conds_lst
  { let subs, init, trans, inv = others in
    sub::subs, init, trans, inv
  }
  | i=init_cond others=subs_and_conds_lst
  { let subs, init, trans, inv = others in
    subs, i :: init, trans, inv
  }
  | t=trans_cond others=subs_and_conds_lst
  { let subs, init, trans, inv = others in
    subs, init, t :: trans, inv
  }
  | i=inv_cond others=subs_and_conds_lst
  { let subs, init, trans, inv = others in
    subs, init, trans, i :: inv
  }
  | sub=system_subsys_dec
  { [sub], [], [], [] }
  | init=init_cond
  { [], [init], [], [] }
  | trans=trans_cond
  { [], [], [trans], [] }
  | inv=inv_cond
  { [], [], [], [inv] }

subs_and_conds:
  | sc=subs_and_conds_lst
  {
    let get_cond cond_lst attr_name =
      match cond_lst with
      | [] -> T.const I.(mk term "true")
      | [(_, c)] -> c
      | _ :: (loc, _) :: _ ->
        let msg = Format.dprintf "%a attribute is not repeatable"
          Format.pp_print_text attr_name
        in
        raise (L.Syntax_error (loc, `Regular msg))
    in
    let subs, init, trans, inv = sc in
    subs,
    get_cond init ":init",
    get_cond trans ":trans",
    get_cond inv ":inv"
  }

opt_subs_and_conds :
  | 
  { let true_ = T.const I.(mk term "true") in 
    [], true_, true_, true_ 
  }
  | sc=subs_and_conds { sc }

system_def:
  | s=SYMBOL vars=opt_system_var_decls sc=opt_subs_and_conds
  { let subs, init, trans, inv = sc in
    I.(mk term s), vars, subs, init, trans, inv
  }

reach_cond:
  | CHECK_REACH OPEN s=SYMBOL cond=term CLOSE
  { I.(mk term s), cond }

assump_cond:
  | CHECK_ASSUMPTION OPEN s=SYMBOL cond=term CLOSE
  { I.(mk term s), cond }

sys_check_query_base:
  | OPEN s=SYMBOL OPEN args=pattern_symbol* CLOSE CLOSE
  {I.(mk term s), args}

sys_check_query:
  | CHECK_QUERY query=sys_check_query_base
  { [query] }
  | CHECK_QUERIES OPEN queries=sys_check_query_base* CLOSE
  { queries }

sys_check_attrs_and_queries:
  | a=assump_cond others=sys_check_attrs_and_queries
  { let assumption, reachable, queries = others in
    a :: assumption, reachable, queries
  }
  | r=reach_cond others=sys_check_attrs_and_queries
  { let assumption, reachable, queries = others in
    assumption, r :: reachable, queries
  }
  | q=sys_check_query others=sys_check_attrs_and_queries
  { let assumption, reachable, queries = others in
    assumption, reachable, q :: queries
  }
  | a=assump_cond
  { [a], [], [] }
  | r=reach_cond
  { [], [r], [] }
  | q=sys_check_query
  { [], [], [q] }

opt_sys_check_attrs_and_queries:
  | { [], [], [] }
  | aq = sys_check_attrs_and_queries { aq }

sys_symbol:
  | s=pattern_symbol { s }

system_check:
  | sid=sys_symbol vars=opt_system_var_decls attrs_queries=opt_sys_check_attrs_and_queries
  { let assumption, reachable, queries = attrs_queries in
    sid, vars, assumption, reachable, List.flatten queries
  }

/* Additional rule for prop_literals symbols, to have lighter
   semantic actions in prop_literal reductions. */
prop_symbol:
  | s=pattern_symbol { s }
;

/* This is a ugly hack, but necessary because the syntax defines
   this reduction using a `not` token which doesn't really exists,
   since it is not a reserved word, thus forcing us to pattern
   match on the string... */
not_symbol:
  | s=SYMBOL
    { let loc = L.mk_pos $startpos $endpos in
      match s with
      | "not" ->
        T.const ~loc I.(mk term s)
      | _ ->
        let msg = Format.dprintf "@[<v>@[<hov>%a@]@ Hint: @[<hov>%a@]@]"
          Format.pp_print_text "expected the 'not' symbol at that point."
          Format.pp_print_text
           "check-sat-assuming only accepts a list of terms \
            of the form 'p' or '(not p)', where p is a boolean literal."
        in
        raise (L.Syntax_error (loc, `Regular msg)) }
;

prop_literal:
  | s=prop_symbol
    { s }
  | OPEN f=not_symbol s=prop_symbol CLOSE
    { let loc = L.mk_pos $startpos $endpos in T.apply ~loc f [s] }
;

command:
  | OPEN ASSERT t=term CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.assert_ ~loc t }
  | OPEN CHECK_SAT CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.check_sat ~loc [] }
  | OPEN CHECK_SAT_ASSUMING OPEN l=prop_literal* CLOSE CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.check_sat ~loc l }
  | OPEN DECLARE_CONST s=SYMBOL ty=sort CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.fun_decl ~loc I.(mk term s) [] [] ty }
  | OPEN DECLARE_DATATYPE s=SYMBOL d=datatype_dec CLOSE
    { let vars, constructors = d in
      let loc = L.mk_pos $startpos $endpos in
      S.datatypes ~loc [I.(mk sort s), vars, constructors] }
  | OPEN DECLARE_DATATYPES OPEN l1=sort_dec+ CLOSE OPEN l2=datatype_dec+ CLOSE CLOSE
    { let res =
        try
          List.map2 (fun (s, _) (vars, constructors) -> s, vars, constructors) l1 l2
        with Invalid_argument _ ->
          let loc = L.mk_pos $startpos($3) $endpos($8) in
          mismatched_lists ~loc
            (l1, "sort declaration", "sort declarations")
            (l2, "datatype definition", "datatype definitions")
      in
      let loc = L.mk_pos $startpos $endpos in
      S.datatypes ~loc res }
  | OPEN DECLARE_FUN s=SYMBOL OPEN args=sort* CLOSE ty=sort CLOSE
    { let id = I.(mk term s) in
      let loc = L.mk_pos $startpos $endpos in
      S.fun_decl ~loc id [] args ty }
  | OPEN DECLARE_SORT s=SYMBOL n=NUM CLOSE
    { let id = I.(mk sort s) in
      let loc = L.mk_pos $startpos $endpos in
      S.type_decl ~loc id (int_of_string n) }
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
  | OPEN DEFINE_SYS f=system_def CLOSE
    { let id, vars, subs, init, trans, inv = f in
      let input, output, local = vars in
      let loc = L.mk_pos $startpos $endpos in
      S.sys_def ~loc id ~input ~output ~local ~subs ~init ~trans ~inv }
  | OPEN DECLARE_ENUM_SORT s=SYMBOL OPEN d=enum_constructor_dec+ CLOSE CLOSE
    {
      let constructors = d in
      let loc = L.mk_pos $startpos $endpos in
      S.datatypes ~loc [I.(mk sort s), [], constructors]
    }
  | OPEN CHECK_SYS sc=system_check CLOSE
    { let sid, vars, assumption, reachable, queries = sc in
      let input, output, local = vars in
      let loc = L.mk_pos $startpos $endpos in
      S.sys_check ~loc sid ~input ~output ~local ~assumption ~reachable ~queries
    }
  | OPEN DEFINE_SORT s=SYMBOL OPEN args=SYMBOL* CLOSE ty=sort CLOSE
    { let id = I.(mk sort s) in
      let l = List.map I.(mk sort) args in
      let loc = L.mk_pos $startpos $endpos in
      S.type_def ~loc id l ty }
  | OPEN ECHO s=STR CLOSE
    { let loc = L.mk_pos $startpos $endpos in
      S.echo ~loc s }

  | OPEN EXIT CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.exit ~loc () }

  | OPEN GET_ASSERTIONS CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.get_assertions ~loc () }
  | OPEN GET_ASSIGNMENT CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.get_assignment ~loc () }
  | OPEN GET_INFO i=info_flag CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.get_info ~loc i }
  | OPEN GET_MODEL CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.get_model ~loc () }
  | OPEN GET_OPTION k=KEYWORD CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.get_option ~loc k }
  | OPEN GET_PROOF CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.get_proof ~loc () }
  | OPEN GET_UNSAT_ASSUMPTIONS CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.get_unsat_assumptions ~loc () }
  | OPEN GET_UNSAT_CORE CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.get_unsat_core ~loc () }
  | OPEN GET_VALUE OPEN l=term+ CLOSE CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.get_value ~loc l }

  | OPEN POP n=NUM CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.pop ~loc (int_of_string n) }
  | OPEN PUSH n=NUM CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.push ~loc (int_of_string n) }
  | OPEN RESET CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.reset ~loc () }
  | OPEN RESET_ASSERTIONS CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.reset_assertions ~loc () }

  | OPEN SET_INFO c=command_option CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.set_info ~loc c }
  | OPEN SET_LOGIC s=SYMBOL CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.set_logic ~loc s }
  | OPEN SET_OPTION c=command_option CLOSE
    { let loc = L.mk_pos $startpos $endpos in S.set_option ~loc c }
;

file:
  | l=command* EOF
    { l }
;

input:
  | EOF
    { None }
  | c=command
    { Some c }

%%
