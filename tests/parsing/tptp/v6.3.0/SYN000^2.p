%------------------------------------------------------------------------------
% File     : SYN000^2 : TPTP v6.1.0. Bugfixed v5.5.0.
% Domain   : Syntactic
% Problem  : Advanced TPTP 
% Version  : Biased.
% English  : Advanced TPTP TH0 syntax that you will encounter some time.

% Refs     :
% Source   : [TPTP]
% Names    :

% Status   : Satisfiable
% Rating   : 1.00 v5.5.0
% Syntax   : Number of formulae    :   38 (   6 unit;  12 type;   1 defn)
%            Number of atoms       :  151 (  10 equality;  36 variable)
%            Maximal formula depth :    8 (   4 average)
%            Number of connectives :   83 (   2   ~;   9   |;   3   &;  62   @)
%                                         (   1 <=>;   2  =>;   0  <=;   0 <~>)
%                                         (   1  ~|;   1  ~&;   1  !!;   1  ??)
%            Number of type conns  :   12 (   9   >;   3   *;   0   +;   0  <<)
%            Number of symbols     :   38 (  12   :)
%            Number of variables   :   25 (   1 sgn;  20   !;   3   ?;   0   ^)
%                                         (  25   :;   0  !>;   0  ?*)
%                                         (   1  @-;   1  @+)
%            Arithmetic symbols    :    3 (   3 pred;    0 func;    0 numbers)
% SPC      : TH0_SAT_EQU

% Comments : 
% Bugfixes : v4.0.1 - Fixed connective_terms and pi_sigma_operators so they're
%            well typed. 
%          : v4.0.1 - Added more numbers, particularly rationals.
%          : v4.1.1 - Removed rationals with negative denominators.
%          : v4.1.1 - Fixed p_real_type
%          : v5.5.0 - Fixed tff to thf in 4 formulae
%------------------------------------------------------------------------------
%----Quoted symbols
thf(distinct_object,axiom,(
    "An Apple" != "A \"Microsoft \\ escape\"" )).

%----Numbers
thf(p_int_type,type,(
    p_int: $int > $o )).

thf(p_rat_type,type,(
    p_rat: $rat > $o )).

thf(p_real_type,type,(
    p_real: $real > $o )).

thf(integers,axiom,
    ( ( p_int @ 123 )
    | ( p_int @ -123 ) )).

thf(rationals,axiom,
    ( ( p_rat @ 123/456 )
    | ( p_rat @ -123/456 )
    | ( p_rat @ +123/456 ) )).

thf(reals,axiom,
    ( ( p_real @ 123.456 )
    | ( p_real @ -123.456 )
    | ( p_real @ 123.456E789 )
    | ( p_real @ 123.456e789 )
    | ( p_real @ -123.456E789 )
    | ( p_real @ 123.456E-789 )
    | ( p_real @ -123.456E-789 ) )).

%----Types for stuff below
thf(a_type,type,(
    a: $i )).

thf(b_type,type,(
    b: $i )).

thf(f_type,type,(
    f: $i > $i )).

thf(g_type,type,(
    g: ( $i * $i ) > $i )).

thf(h_type,type,(
    h: ( $i * $i * $i ) > $i )).

thf(p_type,type,(
    p: $i > $o )).

thf(q_type,type,(
    q: $i > $i > $o )).

%----Conditional constructs
thf(if_then_else_thf,axiom,
    ! [Z: $i] :
      $ite_f(
        ? [X: $i] : ( p @ X)
      , ! [X: $i] : (q @ X @ X)
      , ( q @ Z @ $ite_f(! [X: $i] : ( p @ X), ( f @ a), ( f@ Z))) ) ).

%----Let binders
thf(let_binders_thf,axiom,(
    ! [X: $i] :
      $let_ff(
        ! [Y1: $i,Y2: $i] :
          ( ( p @ Y1 @ Y2)
        <=> ( q @ Y1) )
      , $let_tf(
          ! [X1: $i,X2: $i] : ( ( g @ X1 @ X2) = (h @ X1 @ X1 @ X1) )
        , ( p @ ( g @ a @ b) ) ) ) )).

%----Connective terms
thf(equal_equal_equal,axiom,(
    = = = )).

thf(connective_terms,axiom,(
    ! [P: $o,C: $i] :
      ( ( & @ ( p @ C ) @ P )
      = ( ~ @ ( ~& @ ( p @ C ) @ P ) ) ) )).

%----Connectives - seen |, &, =>, ~ already
thf(pi_sigma_operators,axiom,
    ( ( !! ( p )
      & ?? ( p ) )
    = ( ! [X: $i] :
        ? [Y: $i] :
          ( ( p @ X )
          & ( p @ Y ) ) ) )).

thf(description_choice,axiom,
    ( ? [X: $i] :
        ( ( p @ X )
        & ! [Y: $i] :
            ( ( p @ Y )
           => ( X = Y ) ) )
   => ( ( @-[X: $i] :
            ( p @ X ) )
      = ( @+[X: $i] :
            ( p @ X ) ) ) )).

thf(never_used_connectives,axiom,(
    ! [X: $i] :
      ( ( ( p @ X )
       ~| ~ ( q @ X @ a ) )
     ~& ( p @ X ) ) )).

%----Roles
thf(role_definition,definition,(
    ! [X: $i] :
      ( ( f @ a )
      = ( f @ X ) ) )).

thf(role_assumption,assumption,
    ( p @ a )).

thf(role_lemma,lemma,
    ( p @ a )).

thf(role_theorem,theorem,
    ( p @ a )).

thf(role_unknown,unknown,
    ( p @ a )).

%----Selective include directive
include('Axioms/SYN000^0.ax',[ia1_type,ia1,ia3_type,ia3]).

%----Source
thf(source_unknown,axiom,(
    ! [X: $i] :
      ( p @ X ) ),
    unknown).

thf(source,axiom,(
    ! [X: $i] :
      ( p @ X ) ),
    file('SYN000-1.p')).

thf(source_name,axiom,(
    ! [X: $i] :
      ( p @ X ) ),
    file('SYN000-1.p',source_unknown)).

thf(source_copy,axiom,(
    ! [X: $i] :
      ( p @ X ) ),
    source_unknown).

thf(source_introduced_assumption,axiom,(
    ! [X: $i] :
      ( p @ X ) ),
    introduced(assumption,[from,the,world])).

thf(source_inference,axiom,
    ( p @ a ),
    inference(magic,[status(thm),assumptions([source_introduced_assumption])],[theory(equality),source_unknown])).

thf(source_inference_with_bind,axiom,
    ( p @ a ),
    inference(magic,[status(thm)],[theory(equality),source_unknown:[bind(X,$fot(a))]])).

%----Useful info
thf(useful_info,axiom,(
    ! [X: $i] :
      ( p @ X ) ),
    unknown,
    [simple,prolog(like,Data,[nested,12.2]),AVariable,12.2,"A distinct object",$thf(( p @ X ) | ~ ( q @ X @ a )),data(name):[colon,list,2],[simple,prolog(like,Data,[nested,12.2]),AVariable,12.2]]).

%------------------------------------------------------------------------------
