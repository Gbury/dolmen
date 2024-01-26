%------------------------------------------------------------------------------
% File     : SYN000^2 : TPTP v8.2.0. Bugfixed v8.1.1.
% Domain   : Syntactic
% Problem  : Advanced TPTP TH0 syntax
% Version  : Biased.
% English  : 

% Refs     :
% Source   : [TPTP]
% Names    :

% Status   : Satisfiable
% Rating   : 1.00 v8.2.0
% Syntax   : Number of formulae    :   63 (  26 unt;  27 typ;   1 def)
%            Number of atoms       :   76 (   9 equ;   3 cnn)
%            Maximal formula atoms :    7 (   2 avg)
%            Number of connectives :  124 (   3   ~;   9   |;   2   &; 105   @)
%                                         (   0 <=>;   2  =>;   0  <=;   0 <~>)
%                                         (   1  ~|;   2  ~&}
%            Maximal formula depth :    9 (   4 avg)
%            Number of X terms     :   19 (   5  [];   6 ite;   8 let)
%            Number arithmetic     :   32 (   7 atm;   0 fun;  13 num;  12 var)
%            Number of types       :    6 (   1 usr;   3 ari)
%            Number of type conns  :   33 (  30   >;   3   *;   0   +;   0  <<)
%            Number of symbols     :   54 (  30 usr;  31 con; 0-4 aty)
%            Number of variables   :   27 (   1   ^;  23   !;   1   ?;  27   :)
%                                         (   0  !>;   0  ?*;   1  @-;   1  @+)
% SPC      : TH0_SAT_EQU_ARI

% Comments : 
% Bugfixes : v4.0.1 - Fixed connective_terms and pi_sigma_operators so they're
%            well typed. 
%          : v4.0.1 - Added more numbers, particularly rationals.
%          : v4.1.1 - Removed rationals with negative denominators.
%          : v4.1.1 - Fixed p_real_type
%          : v5.5.0 - Fixed tff to thf in 4 formulae
%          : v8.1.1 - Changed $ite to applications
%------------------------------------------------------------------------------
%----Quoted symbols
thf(distinct_object,axiom,
    ( "An Apple" != "A \"Microsoft \\ escape\"" ) ).
%    "An Apple" != "A \"Microsoft \\ escape\"" ).

%----Numbers
thf(p_int_type,type,
    p_int: $int > $o ).

thf(p_rat_type,type,
    p_rat: $rat > $o ).

thf(p_real_type,type,
    p_real: $real > $o ).

thf(integers,axiom,
    ( ( p_int @ 123 )
    | ( p_int @ -123 ) ) ).

thf(rationals,axiom,
    ( ( p_rat @ 123/456 )
    | ( p_rat @ -123/456 )
    | ( p_rat @ 123/456 ) ) ).

thf(reals,axiom,
    ( ( p_real @ 123.456 )
    | ( p_real @ -123.456 )
    | ( p_real @ 123.456E78 )
    | ( p_real @ 123.456e78 )
    | ( p_real @ -123.456E78 )
    | ( p_real @ 123.456E-78 )
    | ( p_real @ -123.456E-78 ) ) ).

%----Types for stuff below
thf(a_type,type,
    a: $i ).

thf(b_type,type,
    b: $i ).

thf(f_type,type,
    f: $i > $i ).

thf(g_type,type,
    g: ( $i * $i ) > $i ).

thf(h_type,type,
    h: ( $i * $i * $i ) > $i ).

thf(p_type,type,
    p: $i > $o ).

thf(q_type,type,
    q: $i > $i > $o ).

thf(tt_type,type,
    tt: $tType ).

thf(dt_type,type,
    dt: 
      [$i,tt,$int] ).

thf(pt_type,type,
    pt: [tt,$i] > $o ).

thf(ft_type,type,
    ft: $o > [$i,tt,$int] > [tt,$i] ).

thf(ptt_type,type,
    ptt: [$int,$i,$o] > $o > $i > $o ).

%----Tuples
thf(tuples_1,axiom,
    pt @ ( ft @ $true @ dt ) ).

thf(tuples_2,axiom,
    ( p
    = ( ^ [X: $i] : ( ptt @ [33,a,$true] @ ( q @ a @ b ) ) ) ) ).

%----Types for stuff below
thf(il_type,type,
    il: $int ).

thf(jl_type,type,
    jl: $int ).

thf(fl_type,type,
    fl: $int > $int > $int > $int > $rat ).

thf(pl_type,type,
    pl: $rat > $o ).

thf(ql_type,type,
    ql: $int > $int > $o ).

thf(fll_type,type,
    fll: $int > $int > $int > $int > $int ).

thf(pll_type,type,
    pll: $int > $o ).

thf(max_type,type,
    max: $int > $int > $int ).

thf(pc_type,type,
    pc: [$int,$int] > $o ).

thf(dc_type,type,
    dc: 
      [$int,$int] ).

%----Conditional constructs. 
thf(ite_1,axiom,
    ! [X: $int,Y: $int] : ( $ite @ ( $greater @ X @ Y ) @ ( pll @ X ) @ ( pll @ Y ) ) ).

thf(ite_2,axiom,
    ! [X: $int,Y: $int] : ( pll @ ( $ite @ ( $greater @ X @ Y ) @ X @ Y ) ) ).

thf(max_defn,axiom,
    ! [X: $int,Y: $int] :
      ( ( max @ X @ Y )
      = ( $ite @ ( $greatereq @ X @ Y ) @ X @ Y ) ) ).

thf(max_property,axiom,
    ! [X: $int,Y: $int] :
      ( $ite
      @ ( ( max @ X @ Y )
        = X )
      @ ( $greatereq @ X @ Y )
      @ ( $greatereq @ Y @ X ) ) ).

thf(ite_tuple_1,axiom,
    ! [X: $int,Y: $int] : ( pc @ ( $ite @ ( $greater @ X @ Y ) @ [X,Y] @ [Y,X] ) ) ).

thf(ite_tuple_2,axiom,
    ! [X: $int,Y: $int] :
      ( dc
      = ( $ite @ ( $greater @ X @ Y ) @ [X,Y] @ [Y,X] ) ) ).

%----Let binders. 
thf(let_1,axiom,
    $let(
      ff: $int > $int > $rat,
      ff @ X @ Y:= fl @ X @ X @ Y @ Y,
      pl @ ( ff @ il @ jl ) ) ).

thf(let_2,axiom,
    $let(
      ff: $int > $int > $rat,
      ff:= ^ [X: $int,Y: $int] : ( fl @ X @ X @ Y @ Y ),
      pl @ ( ff @ il @ jl ) ) ).

thf(let_tuple_1,axiom,
    $let(
      [ a: $int,
        b: $int ],
      [ a:= b,
        b:= a ],
      ql @ a @ b ) ).

thf(let_tuple_2,axiom,
    $let(
      [ ff: $int > $int > $int,
        gg: $int > $int ],
      [ ff @ X @ Y:= fll @ X @ X @ Y @ Y,
        gg @ Z:= fll @ Z @ Z @ Z @ Z ],
      pll @ ( ff @ il @ ( gg @ il ) ) ) ).

thf(let_tuple_3,axiom,
    $let(
      ff: $int > $int > $int,
      ff @ X @ Y:= fll @ X @ X @ Y @ Y,
      $let(
        gg: $int > $int,
        gg @ Z:= ff @ Z @ Z,
        pll @ ( gg @ il ) ) ) ).

thf(let_tuple_4,axiom,
    $let(
      [ a: $int,
        b: $int ],
      [a,b]:= 
        [27,28],
      qll @ a @ b ) ).

thf(let_tuple_5,axiom,
    $let(
      d: 
        [$int,$int],
      d:= 
        [27,28],
      pc @ d ) ).

%----Connective terms
thf(connective_terms,axiom,
    ! [P: $o,C: $i] :
      ( ( (&) @ ( p @ C ) @ P )
      = ( (~) @ ( (~&) @ ( p @ C ) @ P ) ) ) ).

%----Connectives - seen |, &, =>, ~ already
thf(description_choice,axiom,
    ( ? [X: $i] :
        ( ( p @ X )
        & ! [Y: $i] :
            ( ( p @ Y )
           => ( X = Y ) ) )
   => ( ( @-[X: $i] : ( p @ X ) )
      = ( @+[X: $i] : ( p @ X ) ) ) ) ).

thf(never_used_connectives,axiom,
    ! [X: $i] :
      ( ( ( p @ X )
       ~| ~ ( q @ X @ a ) )
     ~& ( p @ X ) ) ).

%----Roles
thf(role_definition,definition,
    ! [X: $i] :
      ( ( f @ a )
      = ( f @ X ) ) ).

thf(role_assumption,assumption,
    p @ a ).

thf(role_lemma,lemma,
    p @ a ).

thf(role_theorem,theorem,
    p @ a ).

thf(role_unknown,unknown,
    p @ a ).

%----Selective include directive
include('Axioms/SYN000^0.ax',[ia1_type,ia1,ia3_type,ia3]).

%----Source
thf(source_unknown,axiom,
    ! [X: $i] : ( p @ X ),
    unknown ).

thf(source,axiom,
    ! [X: $i] : ( p @ X ),
    file('SYN000-1.p') ).

thf(source_name,axiom,
    ! [X: $i] : ( p @ X ),
    file('SYN000-1.p',source_unknown) ).

thf(source_copy,axiom,
    ! [X: $i] : ( p @ X ),
    source_unknown ).

thf(source_introduced_assumption,axiom,
    ! [X: $i] : ( p @ X ),
    introduced(assumption,[from,the,world]) ).

thf(source_inference,axiom,
    p @ a,
    inference(magic,[status(thm),assumptions([source_introduced_assumption])],[theory(equality),source_unknown]) ).

thf(source_inference_with_bind,axiom,
    p @ a,
    inference(magic,[status(thm)],[theory(equality),source_unknown:[bind(X,$fot(a))]]) ).

%----Useful info
thf(useful_info,axiom,
    ! [X: $i] : ( p @ X ),
    unknown,
    [simple,prolog(like,Data,[nested,12.2]),AVariable,12.2,"A distinct object",$thf( ( p @ X ) | ~ ( q @ X @ a ) ),data(name):[colon,list,2],[simple,prolog(like,Data,[nested,12.2]),AVariable,12.2]] ).

%------------------------------------------------------------------------------
