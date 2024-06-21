%------------------------------------------------------------------------------
% File     : SYN000_4 : TPTP v8.2.0. Released v7.1.0.
% Domain   : Syntactic
% Problem  : TPTP TXF syntax
% Version  : Biased.
% English  : 

% Refs     :
% Source   : [TPTP]
% Names    :

% Status   : Satisfiable
% Rating   : ? v7.1.0
% Syntax   : Number of formulae    :   43 (  13 unt;  28 typ;   0 def)
%            Number of atoms       :   35 (   4 equ)
%            Maximal formula atoms :    3 (   0 avg)
%            Number of connectives :    2 (   2   ~;   0   |;   0   &)
%                                         (   0 <=>;   0  =>;   0  <=;   0 <~>)
%            Maximal formula depth :    4 (   2 avg)
%            Maximal term depth    :    3 (   1 avg)
%            Number of FOOLs       :   16 (  16 fml;   0 var)
%            Number of X terms     :   16 (   6  [];   6 ite;   4 let)
%            Number arithmetic     :   24 (   7 atm;   0 fun;   3 num;  14 var)
%            Number of types       :    5 (   1 usr;   2 ari)
%            Number of type conns  :   33 (  18   >;  15   *;   0   +;   0  <<)
%            Number of predicates  :   21 (  17 usr;   4 prp; 0-3 aty)
%            Number of functors    :   18 (  16 usr;  10 con; 0-4 aty)
%            Number of variables   :   16 (  16   !;   0   ?;  16   :)
% SPC      : TX0_SAT_EQU_ARI

% Comments : 
%------------------------------------------------------------------------------
%----Formulae as terms
tff(p1_type,type,
    p1: ( $i * $o * $int ) > $o ).

tff(q1_type,type,
    q1: ( $int * $i ) > $o ).

tff(me_type,type,
    me: $i ).

tff(fool_1,axiom,
    ! [X: $int] :
      p1(me,
        ! [Y: $i] : q1(X,Y),
        27) ).

tff(p2_type,type,
    p2: $i > $o ).

tff(q2_type,type,
    q2: $o > $o ).

tff(fool_2,axiom,
    q2(~ p2(me) != q2($true)) ).

%----Tuples
tff(tt_type,type,
    tt: $tType ).

tff(a_type,type,
    a: $i ).

tff(at_type,type,
    at: tt ).

tff(dt_type,type,
    dt: 
      [$i,tt,$i] ).

tff(pt_type,type,
    pt: [tt,$i] > $o ).

tff(ft_type,type,
    ft: ( $i * [$i,tt,$i] ) > [tt,$i] ).

tff(tuple_1,axiom,
    pt(ft(a,dt)) ).

tff(tuple_2,axiom,
    pt(ft(a,[a,at,a])) ).

%----Tuples with Booleans
tff(pt1_type,type,
    pt1: ( [$int,$i,$o] * $o * $int ) > $o ).

tff(qt1_type,type,
    qt1: ( $int * $i ) > $o ).

tff(tuples_1,axiom,
    ! [X: $int] :
      pt1([33,me,$true],
        ! [Y: $i] : qt1(X,Y),
        27) ).

%----Conditional expressions
tff(pc1_type,type,
    pc1: $int > $o ).

tff(qc1_type,type,
    qc1: $int > $o ).

tff(max_type,type,
    max: ( $int * $int ) > $int ).

tff(ite_1,axiom,
    ! [X: $int,Y: $int] :
      $ite($greater(X,Y),pc1(X),pc1(Y)) ).

tff(ite_2,axiom,
    ! [X: $int,Y: $int] :
      qc1($ite($greater(X,Y),X,Y)) ).

tff(max_defn,axiom,
    ! [X: $int,Y: $int] :
      max(X,Y) = $ite($greatereq(X,Y),X,Y) ).

tff(max_property,axiom,
    ! [X: $int,Y: $int] :
      $ite(max(X,Y) = X,$greatereq(X,Y),$greatereq(Y,X)) ).

%----Conditional expressions with tuples
tff(pct1_type,type,
    pct1: [$int,$int] > $o ).

tff(dct1_type,type,
    dct1: 
      [$int,$int] ).

tff(ite_3,axiom,
    ! [X: $int,Y: $int] :
      pct1($ite($greater(X,Y),[X,Y],[Y,X])) ).

tff(ite_4,axiom,
    ! [X: $int,Y: $int] :
      dct1 = $ite($greater(X,Y),[X,Y],[Y,X]) ).

%----Let expressions
tff(pl1_type,type,
    pl1: $int > $o ).

tff(let_1,axiom,
    $let(
      c: $int,
      c:= 27,
      pl1(c) ) ).

tff(il2_type,type,
    il2: $int ).

tff(jl2_type,type,
    jl2: $int ).

tff(fl2_type,type,
    fl2: ( $int * $int * $int * $int ) > $rat ).

tff(pl2_type,type,
    pl2: $rat > $o ).

tff(let_2,axiom,
    $let(
      ff: ( $int * $int ) > $rat,
      ff(X,Y):= fl2(X,X,Y,Y),
      pl2(ff(il2,jl2)) ) ).

%----Let expression with tuples (and shadowing)
tff(al3_type,type,
    al3: $int ).

tff(bl3_type,type,
    bl3: $int ).

tff(pl3_type,type,
    pl3: ( $int * $int ) > $o ).

tff(let_tuple_1,axiom,
    $let(
      [ al3: $int,
        bl3: $int ],
      [ al3:= bl3,
        bl3:= al3 ],
      pl3(al3,bl3) ) ).

tff(il4_type,type,
    il4: $int ).

tff(fl4_type,type,
    fl4: ( $int * $int * $int * $int ) > $int ).

tff(pl4_type,type,
    pl4: $int > $o ).

tff(let_tuple_2,axiom,
    $let(
      [ ff: ( $int * $int ) > $int,
        gg: $int > $int ],
      [ ff(X,Y):= fl4(X,X,Y,Y),
        gg(Z):= fl4(Z,Z,Z,Z) ],
      pl4(ff(il4,gg(il4))) ) ).

%-------------------------------------------------------------------------------
