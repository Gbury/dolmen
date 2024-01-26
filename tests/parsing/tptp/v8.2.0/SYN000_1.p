%------------------------------------------------------------------------------
% File     : SYN000_1 : TPTP v8.2.0. Released v5.0.0.
% Domain   : Syntactic
% Problem  : Basic TPTP TF0 syntax without arithmetic
% Version  : Biased.
% English  : Basic TPTP TF0 syntax that you can't survive without parsing.

% Refs     :
% Source   : [TPTP]
% Names    :

% Status   : Theorem
% Rating   : 0.11 v8.2.0, 0.10 v8.1.0, 0.18 v7.5.0, 0.38 v7.4.0, 0.20 v7.3.0, 0.17 v7.1.0, 0.00 v6.0.0, 0.40 v5.5.0, 0.25 v5.4.0, 0.33 v5.2.0, 0.67 v5.0.0
% Syntax   : Number of formulae    :   38 (   6 unt;  25 typ;   0 def)
%            Number of atoms       :   32 (   3 equ)
%            Maximal formula atoms :    5 (   0 avg)
%            Number of connectives :   28 (   9   ~;  10   |;   3   &)
%                                         (   1 <=>;   3  =>;   1  <=;   1 <~>)
%            Maximal formula depth :    7 (   4 avg)
%            Maximal term depth    :    4 (   2 avg)
%            Number of types       :    3 (   1 usr)
%            Number of type conns  :   17 (  10   >;   7   *;   0   +;   0  <<)
%            Number of predicates  :   17 (  14 usr;  10 prp; 0-3 aty)
%            Number of functors    :   10 (  10 usr;   6 con; 0-3 aty)
%            Number of variables   :   14 (   6   !;   8   ?;  14   :)
% SPC      : TF0_THM_EQU_NAR

% Comments :
%------------------------------------------------------------------------------
%----Propositional
tff(p0_type,type,
    p0: $o ).

tff(q0_type,type,
    q0: $o ).

tff(r0_type,type,
    r0: $o ).

tff(s0_type,type,
    s0: $o ).

tff(propositional,axiom,
    ( ( p0
      & ~ q0 )
   => ( r0
      | ~ s0 ) ) ).

%----First-order
tff(a_type,type,
    a: $i ).

tff(b_type,type,
    b: $i ).

tff(h_type,type,
    h: $i ).

tff(f_type,type,
    f: $i > $i ).

tff(g_type,type,
    g: ( $i * $i * $i ) > $i ).

tff(p_type,type,
    p: $i > $o ).

tff(q_type,type,
    q: ( $i * $i ) > $o ).

tff(r_type,type,
    r: ( $i * $i * $i ) > $o ).

tff(s_type,type,
    s: $i > $o ).

tff(first_order,axiom,
    ! [X: $i] :
      ( ( p(X)
        | ~ q(X,a) )
     => ? [Y: $i,Z: $i] :
          ( r(X,f(Y),g(X,f(Y),Z))
          & ~ s(f(f(f(b)))) ) ) ).

%----Equality
tff(equality,axiom,
    ? [Y: $i] :
    ! [X: $i,Z: $i] :
      ( ( f(Y) = g(X,f(Y),Z) )
      | ( f(f(f(b))) != a )
      | ( X = f(Y) ) ) ).

%----True and false
tff(true_false,axiom,
    ( $true
    | $false ) ).

tff(quoted_proposition_type,type,
    'A proposition': $o ).

tff(quoted_predicate_type,type,
    'A predicate': $i > $o ).

tff(quoted_constant_type,type,
    'A constant': $i ).

tff(quoted_function_type,type,
    'A function': $i > $i ).

tff(quoted_escape_type,type,
    'A \'quoted \\ escape\'': $i ).

%----Quoted symbols
tff(single_quoted,axiom,
    ( 'A proposition'
    | 'A predicate'(a)
    | p('A constant')
    | p('A function'(a))
    | p('A \'quoted \\ escape\'') ) ).

%----Connectives - seen |, &, =>, ~ already
tff(useful_connectives,axiom,
    ! [X: $i] :
      ( ( p(X)
       <= ~ q(X,a) )
    <=> ? [Y: $i,Z: $i] :
          ( r(X,f(Y),g(X,f(Y),Z))
        <~> ~ s(f(f(f(b)))) ) ) ).

%----New types
tff(new_type,type,
    new: $tType ).

tff(newc_type,type,
    newc: new ).

tff(newf_type,type,
    newf: ( new * $i ) > new ).

tff(newp_type,type,
    newp: ( new * $i ) > $o ).

tff(new_axiom,axiom,
    ! [X: new] : newp(newf(newc,a),a) ).

%----Annotated formula names
tff(123,axiom,
    ! [X: $i] :
      ( ( p(X)
        | ~ q(X,a) )
     => ? [Y: $i,Z: $i] :
          ( r(X,f(Y),g(X,f(Y),Z))
          & ~ s(f(f(f(b)))) ) ) ).

%----Roles
tff(role_hypothesis,hypothesis,
    p(h) ).

tff(role_conjecture,conjecture,
    ? [X: $i] : p(X) ).

%----Include directive
include('Axioms/SYN000_0.ax').

%----Comments
/* This
   is a block
   comment.
*/

%------------------------------------------------------------------------------
