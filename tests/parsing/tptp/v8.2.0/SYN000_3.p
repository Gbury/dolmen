%------------------------------------------------------------------------------
% File     : SYN000_3 : TPTP v8.2.0. Released v7.1.0.
% Domain   : Syntactic
% Problem  : TPTP TF1 syntax
% Version  : Biased.
% English  : 

% Refs     :
% Source   : [TPTP]
% Names    :

% Status   : Satisfiable
% Rating   : 0.86 v8.1.0, 1.00 v7.5.0, 0.93 v7.4.0, 1.00 v7.1.0
% Syntax   : Number of formulae    :   14 (   2 unt;  10 typ;   0 def)
%            Number of atoms       :    6 (   5 equ)
%            Maximal formula atoms :    2 (   0 avg)
%            Number of connectives :    3 (   1   ~;   0   |;   0   &)
%                                         (   0 <=>;   2  =>;   0  <=;   0 <~>)
%            Maximal formula depth :    9 (   6 avg)
%            Maximal term depth    :    3 (   1 avg)
%            Number of types       :    3 (   2 usr)
%            Number of type conns  :   12 (   7   >;   5   *;   0   +;   0  <<)
%            Number of predicates  :    2 (   1 usr;   0 prp; 1-2 aty)
%            Number of functors    :    7 (   7 usr;   1 con; 0-5 aty)
%            Number of variables   :   22 (  17   !;   0   ?;  22   :)
%                                         (   5  !>;   0  ?*;   0  @-;   0  @+)
% SPC      : TF1_SAT_EQU_NAR

% Comments : 
%------------------------------------------------------------------------------
tff(beverage_type,type,
    beverage: $tType ).

tff(syrup_type,type,
    syrup: $tType ).

%----Type constructor
tff(cup_of_type,type,
    cup_of: $tType > $tType ).

tff(full_cup_type,type,
    full_cup: beverage > cup_of(beverage) ).

tff(coffee_type,type,
    coffee: beverage ).

tff(help_stay_awake_type,type,
    help_stay_awake: cup_of(beverage) > $o ).

%----Polymorphic symbol
tff(mixture_type,type,
    mixture: 
      !>[BeverageOrSyrup: $tType] : ( ( BeverageOrSyrup * syrup ) > BeverageOrSyrup ) ).

%----Use of polymorphic symbol
tff(mixture_of_coffee_help_stay_awake,axiom,
    ! [S: syrup] : help_stay_awake(full_cup(mixture(beverage,coffee,S))) ).

%----Type constructor
tff(map,type,
    map: ( $tType * $tType ) > $tType ).

%----Polymorphic symbols
tff(lookup,type,
    lookup: 
      !>[A: $tType,B: $tType] : ( ( map(A,B) * A ) > B ) ).

tff(update,type,
    update: 
      !>[A: $tType,B: $tType] : ( ( map(A,B) * A * B ) > map(A,B) ) ).

%----Use of polymorphic symbols
tff(lookup_update_same,axiom,
    ! [A: $tType,B: $tType,M: map(A,B),K: A,V: B] : lookup(A,B,update(A,B,M,K,V),K) = V ).

tff(lookup_update_diff,axiom,
    ! [A: $tType,B: $tType,M: map(A,B),V: B,K: A,L: A] :
      ( ( K != L )
     => ( lookup(A,B,update(A,B,M,K,V),L) = lookup(A,B,M,L) ) ) ).

tff(map_ext,axiom,
    ! [A: $tType,B: $tType,M: map(A,B),N: map(A,B)] :
      ( ! [K: A] : lookup(A,B,M,K) = lookup(A,B,N,K)
     => ( M = N ) ) ).

%------------------------------------------------------------------------------
