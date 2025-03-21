%------------------------------------------------------------------------------
% File     : SYN000+1 : TPTP v8.2.0. Released v4.0.0.
% Domain   : Syntactic
% Problem  : Basic TPTP FOF syntax
% Version  : Biased.
% English  : Basic TPTP FOF syntax that you can't survive without parsing.

% Refs     :
% Source   : [TPTP]
% Names    :

% Status   : Theorem
% Rating   : 0.19 v8.2.0, 0.17 v8.1.0, 0.19 v7.4.0, 0.17 v7.0.0, 0.20 v6.4.0, 0.19 v6.3.0, 0.25 v6.2.0, 0.28 v6.1.0, 0.33 v6.0.0, 0.43 v5.5.0, 0.48 v5.4.0, 0.46 v5.3.0, 0.52 v5.2.0, 0.40 v5.1.0, 0.43 v5.0.0, 0.54 v4.1.0, 0.57 v4.0.1, 0.78 v4.0.0
% Syntax   : Number of formulae    :   12 (   5 unt;   0 def)
%            Number of atoms       :   31 (   3 equ)
%            Maximal formula atoms :    5 (   2 avg)
%            Number of connectives :   28 (   9   ~;  10   |;   3   &)
%                                         (   1 <=>;   3  =>;   1  <=;   1 <~>)
%            Maximal formula depth :    7 (   4 avg)
%            Maximal term depth    :    4 (   2 avg)
%            Number of predicates  :   16 (  13 usr;  10 prp; 0-3 aty)
%            Number of functors    :    8 (   8 usr;   5 con; 0-3 aty)
%            Number of variables   :   13 (   5   !;   8   ?)
% SPC      : FOF_THM_RFO_SEQ

% Comments :
%------------------------------------------------------------------------------
%----Propositional
fof(propositional,axiom,
    ( ( p0
      & ~ q0 )
   => ( r0
      | ~ s0 ) ) ).

%----First-order
fof(first_order,axiom,
    ! [X] :
      ( ( p(X)
        | ~ q(X,a) )
     => ? [Y,Z] :
          ( r(X,f(Y),g(X,f(Y),Z))
          & ~ s(f(f(f(b)))) ) ) ).

%----Equality
fof(equality,axiom,
    ? [Y] :
    ! [X,Z] :
      ( f(Y) = g(X,f(Y),Z)
      | f(f(f(b))) != a
      | X = f(Y) ) ).

%----True and false
fof(true_false,axiom,
    ( $true
    | $false ) ).

%----Quoted symbols
fof(single_quoted,axiom,
    ( 'A proposition'
    | 'A predicate'(a)
    | p('A constant')
    | p('A function'(a))
    | p('A \'quoted \\ escape\'') ) ).

%----Connectives - seen |, &, =>, ~ already
fof(useful_connectives,axiom,
    ! [X] :
      ( ( p(X)
       <= ~ q(X,a) )
    <=> ? [Y,Z] :
          ( r(X,f(Y),g(X,f(Y),Z))
        <~> ~ s(f(f(f(b)))) ) ) ).

%----Annotated formula names
fof(123,axiom,
    ! [X] :
      ( ( p(X)
        | ~ q(X,a) )
     => ? [Y,Z] :
          ( r(X,f(Y),g(X,f(Y),Z))
          & ~ s(f(f(f(b)))) ) ) ).

%----Roles
fof(role_hypothesis,hypothesis,
    p(h) ).

fof(role_conjecture,conjecture,
    ? [X] : p(X) ).

%----Include directive
include('Axioms/SYN000+0.ax').

%----Comments
/* This
   is a block
   comment.
*/

%------------------------------------------------------------------------------
