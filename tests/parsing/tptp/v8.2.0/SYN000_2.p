%------------------------------------------------------------------------------
% File     : SYN000_2 : TPTP v8.2.0. Bugfixed v5.5.1.
% Domain   : Syntactic
% Problem  : Advanced TPTP TF0 syntax without arithmetic
% Version  : Biased.
% English  : 

% Refs     :
% Source   : [TPTP]
% Names    :

% Status   : Satisfiable
% Rating   : 0.67 v8.2.0, 1.00 v7.4.0, 0.67 v7.2.0, 1.00 v6.0.0
% Syntax   : Number of formulae    :   24 (  16 unt;   7 typ;   1 def)
%            Number of atoms       :   19 (   2 equ)
%            Maximal formula atoms :    3 (   0 avg)
%            Number of connectives :    4 (   2   ~;   0   |;   0   &)
%                                         (   0 <=>;   0  =>;   0  <=;   0 <~>)
%                                         (   1  ~|;   1  ~&}
%            Maximal formula depth :    5 (   2 avg)
%            Maximal term depth    :    2 (   1 avg)
%            Number of types       :    2 (   0 usr)
%            Number of type conns  :    9 (   5   >;   4   *;   0   +;   0  <<)
%            Number of predicates  :    5 (   4 usr;   2 prp; 0-2 aty)
%            Number of functors    :    7 (   5 usr;   4 con; 0-3 aty)
%            Number of variables   :    8 (   8   !;   0   ?;   8   :)
% SPC      : TF0_SAT_EQU_NAR

% Comments : 
% Bugfixes : v5.5.1 - Fixed let_binders.
%------------------------------------------------------------------------------
%----Quoted symbols
tff(distinct_object,axiom,
    "An Apple" != "A \"Microsoft \\ escape\"" ).

%----Types for stuff below
tff(a_type,type,
    a: $i ).

tff(b_type,type,
    b: $i ).

tff(f_type,type,
    f: $i > $i ).

tff(g_type,type,
    g: ( $i * $i ) > $i ).

tff(h_type,type,
    h: ( $i * $i * $i ) > $i ).

tff(p_type,type,
    p: $i > $o ).

tff(q_type,type,
    q: ( $i * $i ) > $o ).

%----Rare connectives
tff(never_used_connectives,axiom,
    ! [X: $i] :
      ( ( p(X)
       ~| ~ q(X,a) )
     ~& p(X) ) ).

%----Roles
tff(role_definition,definition,
    ! [X: $i] : f(a) = f(X) ).

tff(role_assumption,assumption,
    p(a) ).

tff(role_lemma,lemma,
    p(a) ).

tff(role_theorem,theorem,
    p(a) ).

tff(role_unknown,unknown,
    p(a) ).

%----Selective include directive
include('Axioms/SYN000_0.ax',[ia1,ia3]).

%----Source
tff(source_unknown,axiom,
    ! [X: $i] : p(X),
    unknown ).

tff(source,axiom,
    ! [X: $i] : p(X),
    file('SYN000-1.p') ).

tff(source_name,axiom,
    ! [X: $i] : p(X),
    file('SYN000-1.p',source_unknown) ).

tff(source_copy,axiom,
    ! [X: $i] : p(X),
    source_unknown ).

tff(source_introduced_assumption,axiom,
    ! [X: $i] : p(X),
    introduced(assumption,[from,the,world]) ).

tff(source_inference,axiom,
    p(a),
    inference(magic,[status(thm),assumptions([source_introduced_assumption])],[theory(equality),source_unknown]) ).

tff(source_inference_with_bind,axiom,
    p(a),
    inference(magic,[status(thm)],[theory(equality),source_unknown:[bind(X,$fot(a))]]) ).

%----Useful info
tff(useful_info,axiom,
    ! [X: $i] : p(X),
    unknown,
    [simple,prolog(like,Data,[nested,12.2]),AVariable,12.2,"A distinct object",$tff( p(X) | ~ ( q(X,a) ) ),data(name):[colon,list,2],[simple,prolog(like,Data,[nested,12.2]),AVariable,12.2]] ).

%------------------------------------------------------------------------------
