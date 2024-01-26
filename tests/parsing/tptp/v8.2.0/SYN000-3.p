%------------------------------------------------------------------------------
% File     : SYN000-3 : TPTP v8.2.0. Released v8.0.0.
% Domain   : Syntactic
% Problem  : Typed TPTP CNF syntax
% Version  : Biased.
% English  : 

% Refs     :
% Source   : [TPTP]
% Names    :

% Status   : Satisfiable
% Rating   : ? v8.0.0
% Syntax   : Number of clauses     :    8 (   2 unt;   6 nHn;   4 RR)
%            Number of literals    :   24 (   3 equ;   8 neg)
%            Maximal clause size   :    5 (   3 avg)
%            Maximal term depth    :    4 (   2 avg)
%            Number of types       :    1 (   0 usr)
%            Number of type conns  :    0 (   0   >;   0   *;   0   +;   0  <<)
%            Number of predicates  :   13 (  10 usr;   7 prp; 0-3 aty)
%            Number of functors    :    8 (   8 usr;   5 con; 0-3 aty)
%            Number of variables   :   11 (   0 sgn  11   !;   0   ?;  11   :)
% SPC      : TCF_SAT_RFO_EQU_NUE

% Comments :
%------------------------------------------------------------------------------
%----Propositional
tcf(propositional,axiom,
    ( p0
    | ~ q0
    | r0
    | ~ s0 ) ).

%----First-order
tcf(first_order_tcf,axiom,
    ! [X: $i,Y: $i,Z: $i] :
      ( p(X)
      | ~ q(X,a)
      | r(X,f(Y),g(X,f(Y),Z))
      | ~ s(f(f(f(b)))) ) ).

%----Equality
tcf(equality,axiom,
    ! [X: $i,Y: $i,Z: $i] :
      ( f(Y) = g(X,f(Y),Z)
      | f(f(f(b))) != a
      | X = f(Y) ) ).

%      ( ( f(Y) = g(X,f(Y),Z) )
%      | ( f(f(f(b))) != a )
%      | ( X = f(Y) ) ) ).

%----True and false
tcf(true_false,axiom,
    ( $true
    | $false ) ).

%----Quoted symbols
tcf(single_quoted,axiom,
    ! [Y: $i] :
      ( 'A proposition'
      | 'A predicate'(Y)
      | p('A constant')
      | p('A function'(a))
      | p('A \'quoted \\ escape\'') ) ).

%----Connectives - seen them all already

%----Annotated formula names
tcf(123,axiom,
    ! [X: $i,Y: $i,Z: $i] :
      ( p(X)
      | ~ q(X,a)
      | r(X,f(Y),g(X,f(Y),Z))
      | ~ s(f(f(f(b)))) ) ).

%----Roles - seen axiom already
tcf(role_hypothesis,hypothesis,
    p(h) ).

tcf(role_negated_conjecture,negated_conjecture,
    ! [X: $i] : ~ p(X) ).

%------------------------------------------------------------------------------
