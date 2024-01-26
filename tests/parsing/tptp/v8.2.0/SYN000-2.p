%------------------------------------------------------------------------------
% File     : SYN000-2 : TPTP v8.2.0. Bugfixed v7.1.0.
% Domain   : Syntactic
% Problem  : Advanced TPTP CNF syntax
% Version  : Biased.
% English  : 

% Refs     :
% Source   : [TPTP]
% Names    :

% Status   : Satisfiable
% Rating   : 0.80 v8.2.0, 0.78 v8.1.0, 0.86 v7.5.0, 0.88 v7.4.0, 0.70 v7.3.0, 0.56 v7.1.0
% Syntax   : Number of clauses     :   16 (  16 unt;   0 nHn;   9 RR)
%            Number of literals    :   16 (   2 equ;   1 neg)
%            Maximal clause size   :    1 (   1 avg)
%            Maximal term depth    :    2 (   1 avg)
%            Number of predicates  :    7 (   6 usr;   2 prp; 0-3 aty)
%            Number of functors    :   10 (   8 usr;   8 con; 0-3 aty)
%            Number of variables   :    7 (   7 sgn)
% SPC      : CNF_SAT_RFO_EQU_NUE

% Comments :
% Bugfixes : v4.0.1 - Added more numbers, particularly rationals.
%          : v4.1.1 - Removed rationals with negative denominators.
%          : v7.1.0 - Removed numbers
%------------------------------------------------------------------------------
%----Quoted symbols
cnf(distinct_object,axiom,
    "An Apple" != "A \"Microsoft \\ escape\"" ).

%----Roles - seen axiom already
cnf(role_definition,definition,
    f(d) = f(X) ).

cnf(role_assumption,assumption,
    p(a) ).

cnf(role_lemma,lemma,
    p(l) ).

cnf(role_theorem,theorem,
    p(t) ).

cnf(role_unknown,unknown,
    p(u) ).

%----Selective include directive
include('Axioms/SYN000-0.ax',[ia1,ia3]).

%----Source
cnf(source_unknown,axiom,
    p(X),
    unknown ).

cnf(source,axiom,
    p(X),
    file('SYN000-1.p') ).

cnf(source_name,axiom,
    p(X),
    file('SYN000-1.p',source_unknown) ).

cnf(source_copy,axiom,
    p(X),
    source_unknown ).

cnf(source_introduced_assumption,axiom,
    p(X),
    introduced(assumption,[from,the,world]) ).

cnf(source_inference,axiom,
    p(a),
    inference(magic,[status(thm),assumptions([source_introduced_assumption])],[theory(equality),source_unknown]) ).

cnf(source_inference_with_bind,axiom,
    p(a),
    inference(magic,[status(thm)],[theory(equality),source_unknown:[bind(X,$fot(a))]]) ).

%----Useful info
cnf(useful_info,axiom,
    p(X),
    unknown,
    [simple,prolog(like,Data,[nested,12.2]),AVariable,12.2,"A distinct object",$cnf( p(X) | ~ q(X,a) | r(X,f(Y),g(X,f(Y),Z)) | ~ s(f(f(f(b)))) ),data(name):[colon,list,2],[simple,prolog(like,Data,[nested,12.2]),AVariable,12.2]] ).

%------------------------------------------------------------------------------
