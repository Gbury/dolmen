%------------------------------------------------------------------------------
% File     : SYN000^3 : TPTP v8.2.0. Released v7.1.0.
% Domain   : Syntactic
% Problem  : TPTP TH1 syntax
% Version  : Biased.
% English  : 

% Refs     :
% Source   : [TPTP]
% Names    :

% Status   : Satisfiable
% Rating   : 1.00 v7.1.0
% Syntax   : Number of formulae    :   16 (   2 unt;  11 typ;   0 def)
%            Number of atoms       :   12 (   7 equ;   0 cnn)
%            Maximal formula atoms :    2 (   2 avg)
%            Number of connectives :   40 (   0   ~;   0   |;   0   &;  36   @)
%                                         (   0 <=>;   0  =>;   0  <=;   0 <~>)
%            Maximal formula depth :    5 (   3 avg)
%            Number of types       :    4 (   2 usr)
%            Number of type conns  :   18 (  18   >;   0   *;   0   +;   0  <<)
%            Number of symbols     :   15 (   9 usr;   6 con; 0-5 aty;   1  @=)
%                                         (   1  !!;   1  ??;   1 @@+;   1 @@-)
%            Number of variables   :   12 (   4   ^;   4   !;   0   ?;  12   :)
%                                         (   4  !>;   0  ?*;   0  @-;   0  @+)
% SPC      : TH1_SAT_EQU_NAR

% Comments : 
%------------------------------------------------------------------------------
thf(bird_type,type,
    bird: $tType ).

thf(tweety_type,type,
    tweety: bird ).

%----Type constructors
thf(list_type,type,
    list: $tType > $tType ).

thf(map_type,type,
    map: $tType > $tType > $tType ).

%----Polymorphic symbols
thf(bird_lookup_type,type,
    bird_lookup: 
      !>[A: $tType,B: $tType] : ( ( map @ A @ B ) > A > B ) ).

thf(bird_update_type,type,
    bird_update: 
      !>[A: $tType,B: $tType] : ( ( map @ A @ B ) > A > B > ( map @ A @ B ) ) ).

%----Use of polymorphic symbols
thf(bird_lookup_update_same,axiom,
    ! [RangeType: $tType,Map: map @ bird @ RangeType,Key: bird,Value: RangeType] :
      ( ( bird_lookup @ bird @ RangeType @ ( bird_update @ bird @ RangeType @ Map @ Key @ Value ) @ Key )
      = Value ) ).

%----Universal and existential quantification - !! and ??
thf(a_type,type,
    a_type: $tType ).

thf(apply_both_type,type,
    apply_both: a_type > a_type ).

thf(the_function_type,type,
    the_function: a_type > a_type > a_type ).

thf(can_prove_this,axiom,
    ( !! @ a_type
    @ ^ [Y: a_type] :
        ( ( the_function @ Y @ Y )
        = ( apply_both @ Y ) ) ) ).

thf(cant_prove_this,axiom,
    ( ?? @ a_type
    @ ^ [Y: a_type] :
        ( ( the_function @ Y @ Y )
        = ( apply_both @ Y ) ) ) ).

%----Definite and indefinite description - @@+ and @@-
thf(has_fixed_point_type,type,
    has_fixed_point: a_type > a_type ).

thf(broken_fixed_point,axiom,
    ( ( has_fixed_point
      @ ( @@+ @ a_type
        @ ^ [Y: a_type] :
            ( ( has_fixed_point @ Y )
            = Y ) ) )
    = ( @@- @ a_type
      @ ^ [Y: a_type] :
          ( ( has_fixed_point @ Y )
          = Y ) ) ) ).

%----Equality - @=
thf(is_symmetric_type,type,
    is_symmetric: ( ( $i > a_type ) > ( $i > a_type ) > $o ) > $o ).

thf(is_symmetric_property,conjecture,
    is_symmetric @ ( @= @ ( $i > a_type ) ) ).

%------------------------------------------------------------------------------
