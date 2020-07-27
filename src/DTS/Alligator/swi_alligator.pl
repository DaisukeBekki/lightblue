%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                       %%%
%%%                    ALLIGATOR 1.0                      %%%
%%%                                                       %%%
%%%       A THEOREM PROVER FOR PURE TYPE SYSTEMS          %%%
%%%           WITH SIGMA TYPES AND DEFINITIONS            %%%
%%%                                                       %%%
%%%                    Author: Paul Piwek                 %%%
%%%            IPR Owner: The Open University, UK         %%%
%%%                     March   2006                      %%%
%%%                                                       %%%
%%%     URL: http://mcs.open.ac.uk/pp2464/alligator       %%%
%%%                                                       %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* This work is licensed under the Creative Commons
Attribution-NonCommercial-ShareAlike 2.0 England & Wales License.
To view a copy of this licence, visit
http://creativecommons.org/licenses/by-nc-sa/2.0/uk/
or send a letter to Creative Commons, 559 Nathan Abbott Way,
Stanford, California 94305, USA. */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                       %%%
%%%   SWI IMPORTED AND BUILT-IN PREDICATES                %%%
%%%   We take the Sicstus Prolog version as the basis     %%%
%%%   and here "redefine" Sicstus predicates using SWI    %%%
%%%   built-in and imported predicates.                   %%%
%%%                                                       %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% I am using append and reverse from the library lists (same as
% in Sicstus):
:- module(swi_alligator,[prove/2]).
:-use_module(library(lists)).

% we replace variant with =@=
% =@= from SWI manual: Two terms are structurally equal if their
% tree representation is identical and they have the same �pattern�
% of variables.

variant(A,B):-
   A=@=B.

% cyclic_term and acyclic_term are available both in Sicstus in SWI

% Furthermore, I am using the built-in predicates
% copy_term, format and statistics (same in Sicstus and SWI,
% except that we have system_time instead of walltime).

get_now_time(Time):-
    statistics(system_time,[Time,_]).

below_max_time(STime,ETime):-
    statistics(system_time,[CurrentTime,_]),
    EllapsedTime is (CurrentTime - STime)/1000,
    EllapsedTime < ETime.

ellapsed_time(StartTime,Ellapsed):-
    statistics(system_time,[EndTime,_]),
    Ellapsed is EndTime - StartTime.

global_stack_info(Free):-
   statistics(global_stack,[_Used,Free]).

% In Sicstus we use the space_out predicate for memory management
% to avoid the resource_error insufficient memory and
% timeout for computations that are too long.

% Here we make no use of space_out:

space_out(Goal,_Space,Result):-
   call(Goal),
   Result = success.

% we capture both time and space with time_out
% note that sicstus time is in milliseconds and
% swi time in seconds, hence /1000.


time_out(Goal,Time1,TimeOut):-
   Time2 is Time1/1000,
   catch(call_with_time_limit(Time2,Goal),Error,true),
   getValueTimeOut(Error,TimeOut).

getValueTimeOut(Error,success):-
   var(Error),!.

getValueTimeOut(Error,time_out):-
   \+ var(Error),!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                           %
%   The source code below is identical for both the SWI     %
%   and Sicstus implementations of ALLIGATOR                %
%                                                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% DYNAMIC PREDICATES

:-dynamic val/2.

:- dynamic var_mem/4.

%%% GLOBAL PARAMETERS

setval(X,Y):-
    retractall(val(X,_Z)),
    assert(val(X,Y)).

getval(X,Y):-
    val(X,Y).

val(feedback,on).

val(base_context,classic).

% Make sure that false:prop is always part of the context
val(base_context,falsum).

% we start searching for proofs at depth 1
val(startdepth,1).

% the depth for searching for proofs is maximally
% 50
val(maxdepth,50).

% proving can take no longer than X seconds.
val(maxtime,3600).

% Reductions can take no longer than 2.5 seconds
val(reduce_time_out,2500).

% Reductions can use no more than 0.9 of the global
% stack
val(reduce_space_out,0.9).

var_mem(a_0,a,a_,0).
var_mem(v_0,v,v_,0).

%%% OPERATORS

:- op(500,xfx,:).    % A:B reads A inhabits B / B is the type of A

:- op(300,yfx,-).    % function application (left associative)

:- op(400,xfy,=>).   % pi "implication" (right associative)

:- op(400,xfy,/\).   % sigma "conjunction" (right associative)

:- op(400,xfy,&).    % propositional sigma "conjunction" (right associative)

:- op(400,xfy,->).   % "implication" (non-dependent)

:- op(350,xfy,\/).   % "disjunction" (non-dependent)

:- op(300,fy,~).     % "negation"

%%% PROVE (THE MAIN PREDICATE)

% prove(+Pseudo_Context,-Reduced_Proof:+Goal)

prove(Input_Context,P3:B):-
    reverse(Input_Context,I_Context),%OK
    get_act_contexts(Base_Context),%OK
    append(I_Context,Base_Context,Context),%OK
    initialize_fresh_vars(Context,[v,a]),%pass
    standard_context_notat(Context,N_Context),%OK
    reduce_context(N_Context,NR_Context),%OK
    %feedback('~n ...context normalized to standard notation and reduced.~n',[]),
    check_context(NR_Context),%OK
    %feedback('~n ...checked context.~n',[]),
    arrow_context_notat(NR_Context,A_Context),%ok
    standard_notat(B,NB),%ok
    reduce_term(NB,RNB),%ok
    arrow_notat(RNB,BA),%ok
    %feedback('~n ...context normalized to arrow notation.~n',[]),
    forward_context(A_Context,F_Context),%ok?
    %feedback('~n ...forward inferences completed.~n',[]),
    %feedback('~n The following Context will be used to construct a proof:~2n',[]),
    %feedback_list(F_Context),%print
    getval(startdepth,StartDepth),%pass
    getval(maxdepth,MaxDepth),%pass
    get_now_time(StartTime),%pass
    getval(maxtime,MaxTime),%pass
    %feedback('~n ...searching at depth: ',[]),!,
    search_proof(F_Context,P:BA,StartDepth,MaxDepth,StartTime,MaxTime),
    ellapsed_time(StartTime,_),
    %ellapsed_time(StartTime,EndTime),
    %feedback('~2n ...proof found after ~3d seconds.~n',[EndTime]),
    %feedback('~n ...the goal in input notation: ~p ~n',[B]),
    %feedback('~n ...the goal in arrow notation: ~p ~n',[BA]),
    %feedback('~n ...the reduced goal in standard notation: ~p ~n',[RNB]),
    %feedback('~n ...the unreduced proof with atoms and arrows: ~p ~n',[P]),
    standard_notat(P,P1),
    replace_atoms_final_term(P1,P2),
    %feedback('~n ...the unreduced proof: ~p ~n',[P2]),
    reduce_term(P2,P3),
    %feedback('~n ...the reduced proof: ~p ~n',[P3]),
    %check_type_with_feedback(P3,F_TYPE,NR_Context),
    %feedback('~n ...type checking of the proof succeeded ~n',[]),
    %check_type_with_feedback(F_TYPE,S,NR_Context),
    %feedback('~n ...the type of ~p is ~p ~n',[F_TYPE,S]).
    !.

%%% BASE CONTEXTS

get_act_contexts(B):-
    base_contexts(C),
    select_contexts(C,B).

select_contexts([],[]).

select_contexts([(Id,C)|T1],Result1):-
    getval(base_context,Id),!,
    select_contexts(T1,Result2),
    append(C,Result2,Result1).

select_contexts([(_Id,_C)|T1],Result1):-
    select_contexts(T1,Result1).

get_base(Id,Base):-
    base_contexts(Contexts),
    member((Id,Base),Contexts),!.

get_base(Id,_Contexts):-
    feedback('~n ALLIGATOR ERROR - The context ~w does not exist~.n',[Id]),
    throw(error_context_not_existing),
    !,fail.

% Note that internally the statements in a context appear
% in reverse order!
% The order in which base contexts appear is also the reverse of the
% one in which they appear below (hence falsum is below classic)!
% Important: note that variables in the body base_context(X)
% have scope across the contexts that are declared, and consequently
% it is important to use fresh Prolog variables for each context
% to avoid inadvertent binding

base_contexts(X):-
  X =
  [(equality,[to_be_added]),
   (leibniz,[to_be_added]),
%  (contra,[cp1:[P:prop,Q:prop,_G1:([_:P]=>Q),_G2:([_:Q]=>false)]=>([_:P]=>false),5
%
%          cp2:[P1:prop,Q1:prop,_G11:([_:Q1]=>false)]=>([_:P1]=>false)
%            ]),
   (classic,
      [
   %   dn_set:([S:set] =>(((S->false)->false)->S)),  % If we add this one, we
                                                     % need to also modify pi_rules
                                                     % (type_set,set) and (prop,set)
       dn_pr:([P:prop]=>(((P->false)->false)->P))
      ]),
   (falsum,[false:prop])].

% Note that in principle we need double negation for both propositions and types.
% This is because judgements which we express with types in type systems
% come out as propositional in ordinary logic, e.g., x:man is equivalent with
% there is a man.

report_active_contexts:-
    base_contexts(C),
    report_contexts(C).

report_contexts([]):-!.

report_contexts([(Id,_C)|T1]):-
    getval(Id,yes),!,
    format('~5c - the ~w context has been loaded; ~n',[0' ,Id]),
    report_contexts(T1).

report_contexts([_|T1]):-
    report_contexts(T1).

%%% CONSECUTIVE-BOUNDED DEPTH-FIRST SEARCH

search_proof(C,G,Depth,_Max,STime,ETime):-
    below_max_time(STime,ETime),
    % feedback('~d ',[Depth]),
    % deduce(_,C,[Depth,STime,ETime|_T],G)
    deduce(Rule,C,[Depth,STime,ETime|_T],G),
    feedback('~n ...~p deduce rule:  ~p ~p : ~p ~n',[C,Rule,G,[Depth,STime,ETime]])
    .

search_proof(C,G,Depth,Max,STime,ETime):-
    below_max_time(STime,ETime),
    increase_depth(Depth,Max,NewDepth),
    search_proof(C,G,NewDepth,Max,STime,ETime).

increase_depth(Depth,Max,NewDepth):-
    Depth < Max,
    NewDepth is Depth + 1,!.

%%% NOTATION NORMALIZATION RULES FOR PI AND SIGMA

standard_notat(A,A):-
  var(A),!.

standard_notat(A,A):-
  atom(A),!.

standard_notat(A=>B,R):-
  standard_notat(B,B1),
  standard_notat2(A=>B1,R),!.

standard_notat(~A,pi(_X:A1,false)):-
  standard_notat(A,A1),!.

standard_notat(A->B,pi(_X:A1,B1)):-
  standard_notat(A,A1),
  standard_notat(B,B1),!.

standard_notat(A\/B,pi(_X:pi(_Y:A1,false),B1)):-
  standard_notat(A,A1),
  standard_notat(B,B1),!.

standard_notat(A & B,sigma(_X:A1,B1)):-
  standard_notat(A,A1),
  standard_notat(B,B1),!.

standard_notat(A/\B,R):-
  standard_notat(B,B1),
  standard_notat2(A/\B1,R),!.

standard_notat(pi(X:A,B),pi(X:A1,B1)):-
  standard_notat(A,A1),
  standard_notat(B,B1),!.

standard_notat(sigma(X:A,B),sigma(X:A1,B1)):-
  standard_notat(A,A1),
  standard_notat(B,B1),!.

standard_notat(A-B,A1-B1):-
  standard_notat(A,A1),
  standard_notat(B,B1),!.

standard_notat(pair(A,B),pair(A1,B1)):-
  standard_notat(A,A1),
  standard_notat(B,B1),!.

standard_notat(pi1(A1),pi1(A2)):-
  standard_notat(A1,A2),!.

standard_notat(pi2(A1),pi2(A2)):-
  standard_notat(A1,A2),!.

standard_notat(lambda(X:A,B),lambda(X:A1,B1)):-
  standard_notat(A,A1),
  standard_notat(B,B1),!.

standard_notat(def(X,Y1),def(X,Y2)):-
  standard_notat(Y1,Y2),!.

standard_notat2([A:B1|T]=>C,pi(A:B2,D)):-
  standard_notat(B1,B2),
  standard_notat(T=>C,D),!.

standard_notat2([]=>C,C):-!.

standard_notat2([A:B1|T]/\C,sigma(A:B2,D)):-
  standard_notat(B1,B2),
  standard_notat(T/\C,D),!.

standard_notat2([]/\C,C):-!.

standard_context_notat([],[]):-!.

standard_context_notat([def(X,A1):B1|T1],[def(Xc,A2c):B2c|T2]):-
  f_variable(X),
  standard_notat(A1,A2),
  standard_notat(B1,B2),
  copy_term((def(X,A2):B2),def(Xc,A2c):B2c),!,
  standard_context_notat(T1,T2).

standard_context_notat([A:B1|T1],[Ac:B2c|T2]):-
  f_variable(A),
  standard_notat(B1,B2),
  copy_term(A:B2,Ac:B2c),
  !,
  standard_context_notat(T1,T2).

standard_context_notat(_,_):-
  feedback('~n ALLIGATOR ERROR - syntactically incorrect variable in context.~n',[]),
  throw(error_syntax_var_incorrect),
  !,fail.

%%% ARROW NOTATATION NORMALIZATION RULES FOR PI AND SIGMA

arrow_notat(A,A):-%ok
  var(A),!.

arrow_notat(A,A):-%ok
  atom(A),!.

arrow_notat(A => B,A1 => B1):-%pass
  arrow_segment_notat(A,A1),
  arrow_notat(B,B1),!.

arrow_notat(~A,([_X:A1] => false)):-%ok
  arrow_notat(A,A1),!.

arrow_notat(A->B,([_X:A1] => B1)):-%pass
  arrow_notat(A,A1),
  arrow_notat(B,B1),!.

arrow_notat(A\/B,([_X:([_Y:A1] => false)] => B1)):-%pass
  arrow_notat(A,A1),
  arrow_notat(B,B1),!.

arrow_notat(A & B,(sigma(_X:A1,B1))):-%pass
  arrow_notat(A,A1),
  arrow_notat(B,B1),!.

arrow_notat(A/\B,R):-%pass
  arrow_notat(B,B1),
  arrow_notat2(A/\B1,R),!.

arrow_notat(pi(X:A,B),[X:A1] => B1):-%ok
  arrow_notat(A,A1),
  arrow_notat(B,B1),!.

arrow_notat(sigma(X:A,B),sigma(X:A1,B1)):-%ok
  arrow_notat(A,A1),
  arrow_notat(B,B1),!.

arrow_notat(A-B,A1-B1):-%ok
  arrow_notat(A,A1),
  arrow_notat(B,B1),!.

arrow_notat(pair(A,B),pair(A1,B1)):-%ok
  arrow_notat(A,A1),
  arrow_notat(B,B1),!.

arrow_notat(pi1(A1),pi1(A2)):-%ok
  arrow_notat(A1,A2),!.

arrow_notat(pi2(A1),pi2(A2)):-%ok
  arrow_notat(A1,A2),!.

arrow_notat(lambda(X:A,B),lambda(X:A1,B1)):-%ok
  arrow_notat(A,A1),
  arrow_notat(B,B1),!.

arrow_notat(def(X,Y1),def(X,Y2)):-%pass
  arrow_notat(Y1,Y2),!.

arrow_notat2([A:B1|T]/\C,sigma(A:B2,D)):-%pass
  arrow_notat(B1,B2),
  arrow_notat(T/\C,D),!.

arrow_notat2([]/\C,C):-!.%pass

arrow_context_notat([],[]):-!.

arrow_context_notat([def(X,A1):B1|T1],[def(Xc,A2c):B2c|T2]):-
  f_variable(X),
  arrow_notat(A1,A2),
  arrow_notat(B1,B2),
  copy_term((def(X,A2):B2),def(Xc,A2c):B2c),!,
  arrow_context_notat(T1,T2).

arrow_context_notat([A:B1|T1],[Ac:B2c|T2]):-
  f_variable(A),%bruijn
  arrow_notat(B1,B2),
  copy_term(A:B2,Ac:B2c),
  !,
  arrow_context_notat(T1,T2).

arrow_context_notat(_,_):-
  feedback('~n ALLIGATOR ERROR - syntactically incorrect variable in context.~n',[]),
  throw(error_synt_incorrect_var_in_context),
  !,fail.


arrow_segment_notat([],[]):-!.

arrow_segment_notat([A:B1|T1],[A:B2|T2]):-
  arrow_notat(B1,B2),
  !,
  arrow_segment_notat(T1,T2).

%%% CHECKING THE CONTEXT

check_context(C):-
    check_syntax(C),
    check_dependencies(C).

% check_syntax %OK

check_syntax([]):-!.

check_syntax([V:T|Tail]):-
   f_variable(V),
   pterm(T),!,
   check_syntax(Tail).

check_syntax([def(V,X):T|Tail]):-
   f_variable(V),
   pterm(X),
   pterm(T),!,
   check_syntax(Tail).

% variables

variable(X):-
   b_variable(X).

variable(X):-
   f_variable(X).

% bound variables

b_variable(X):-
   var(X).

% free variables

f_variable(X):-
   atom(X),
   sorts(S),
   \+ member(X,S).

% fresh

fresh(_X,[]):-!.

fresh(X,[def(Y,_Z):_T|Tail]):-
   \+ X=Y,!,
   fresh(X,Tail).

fresh(X,[Y:_T|Tail]):-
   \+ X=Y,!,
   fresh(X,Tail).

% pseudo terms

pterm(X):-
   sorts(S),
   \+var(X),
   member(X,S).

pterm(X):-%Haskellの型推論
   variable(X).

pterm(X-Y):-
   pterm(X),
   pterm(Y).

pterm(pair(X,Y)):-
   pterm(X),
   pterm(Y).

pterm(pi1(X)):-
   pterm(X).

pterm(pi2(X)):-
   pterm(X).

pterm(lambda(V:X,Y)):-
   var(V),
   pterm(X),
   pterm(Y).

pterm(pi(V:X,Y)):-
   var(V),
   pterm(X),
   pterm(Y).

pterm(sigma(V:X,Y)):-
   var(V),
   pterm(X),
   pterm(Y).

% initialize_fresh_vars (+Context,+[Variable_Names])

% We make sure that our fresh variables are longer than
% any of the variables in the context. Thus the fresh
% variables will be genuinely new relative to the context.

initialize_fresh_vars(_,[]):-!.

initialize_fresh_vars([],[H|T]):-
     !,
     retractall(var_mem(_X,H,_Z,_Num)),
     concat_atoms(H,0,Var),
     assert(var_mem(Var,H,H,0)),
     initialize_fresh_vars([],T).

initialize_fresh_vars(C1,VarTypes):-
     retractall(var_mem(_Var,_Head,_Initial,_Num)),
     get_vars(C1,Vars),
     member(X,Vars),
     name(X,ListX),
     ListX = [118,95|_],
     \+ exist_longer_than(X,Vars),!,
     length(ListX,LX),
     make_prefix(LX,Prefix),
     initialize_fresh_vars2(C1,VarTypes,Prefix).

initialize_fresh_vars(C1,VarTypes):-
     retractall(var_mem(_Var,_Head,_Initial,_Num)),
     make_prefix(1,Prefix),
     initialize_fresh_vars2(C1,VarTypes,Prefix).

initialize_fresh_vars2(_C,[],_Prefix):-!.

initialize_fresh_vars2(C1,[H|T],Prefix):-
     concat_atoms(H,Prefix,Initial),
     concat_atoms(Initial,0,Var),
     assert(var_mem(Var,H,Initial,0)),
     initialize_fresh_vars2(C1,T,Prefix).

get_vars([],[]):-!.

get_vars([def(X,_Y):_T|Tail],[X|Tail2]):-
    !,
    get_vars(Tail,Tail2).

get_vars([X:_T|Tail],[X|Tail2]):-
    get_vars(Tail,Tail2).

% exist_longer_than(X,List)
% An element of List start with v_ and is longer
% than X

exist_longer_than(X,List):-
    name(X,ListX),
    length(ListX,LX),
    member(Y,List),
    name(Y,ListY),
    ListY = [118,95|_],
    length(ListY,LY),
    LY > LX.

make_prefix(1,'_'):-!.

make_prefix(X,R1):-
    Z is X - 1,
    make_prefix(Z,R2),
    concat_atoms('_',R2,R1).

fresh_variable(Type,Var):-
    var_mem(Var,Type,Initial,Num),
    retractall(var_mem(Var,Type,Initial,Num)),
    NewNum is Num + 1,
    concat_atoms(Initial,NewNum,NewVar),
    assert(var_mem(NewVar,Type,Initial,NewNum)).

% safe_term: check whether there are any multiple bindings of the same variable
% in a term. If there are none, the answer is true, i.e., meaning that
% that we have a safe term in hands.

% We execute a top-down traversal of the term and replace variables
% bound by lambda, pi or sigma with a constant (my_atom). The term
% is safe if we do not hit on variable which has already been
% substituted with my_atom. This includes any variables in the typing
% part of abstractions, e.g., Type in lambda(X:Type,Body)

safe_term(Term,Boolean):-
   safe_term1(Term,Term,Boolean).

% Note that we are carrying the full term FT around
% to make sure that substitutions occur throughout
% the term.

safe_term1(X,_FT,true):-
   atom(X),!.

safe_term1(X,_FT,true):-
   var(X),!.

safe_term1(lambda(X:_Type,_T),_FT,false):-
   \+ var(X),
   X = my_atom,!.

safe_term1(lambda(X:Type,T),FT,TV):-
   var(X),
   X = my_atom,
   safe_term1(T,FT,TV1),
   safe_term1(Type,FT,TV2),
   boolean_and(TV1,TV2,TV).

safe_term1(sigma(X:_Type,_T),_FT,false):-
   \+var(X),
   X = my_atom,!.

safe_term1(sigma(X:Type,T),FT,TV):-
   var(X),
   X = my_atom,
   safe_term1(T,FT,TV1),
   safe_term1(Type,FT,TV2),
   boolean_and(TV1,TV2,TV).

safe_term1(pi(X:_Type,_T),_FT,false):-
   \+var(X),
   X = my_atom,!.

safe_term1(pi(X:Type,T),FT,TV):-
   var(X),
   X = my_atom,
   safe_term1(T,FT,TV1),
   safe_term1(Type,FT,TV2),
   boolean_and(TV1,TV2,TV).

safe_term1(X-Y,FT,TV):-
   safe_term1(X,FT,TV1),
   safe_term1(Y,FT,TV2),
   boolean_and(TV1,TV2,TV).

safe_term1(pair(X,Y),FT,TV):-
   safe_term1(X,FT,TV1),
   safe_term1(Y,FT,TV2),
   boolean_and(TV1,TV2,TV).

safe_term1(def(X,Y),FT,TV):-
   safe_term1(X,FT,TV1),
   safe_term1(Y,FT,TV2),
   boolean_and(TV1,TV2,TV).

safe_term1(pi1(X),FT,TV):-
   safe_term1(X,FT,TV).

safe_term1(pi2(X),FT,TV):-
   safe_term1(X,FT,TV).

% unbound_vars: check whether a term contains unbound variables.

no_free_vars(Term,TV):-
  replace_vars_with_constants(Term,m1_var(m),NewTerm),
  check_all_bound(NewTerm,[],TV).

replace_vars_with_constants(X,_,X):-
   atom(X),!.

replace_vars_with_constants(X,_,X):-
   \+ var(X),
   X = m1_var(_),!.

replace_vars_with_constants(X,m1_var(C),X):-
   var(X),
   X = m1_var(m(C)),!.

% We have "counter" which provides unique identifier for each variable
% in terms of it location in the tree (m:middle, l:left, r:right)
% embedded in m1_var

replace_vars_with_constants(lambda(X:Type,T),m1_var(C),lambda(X:Type2,T2)):-
   \+ var(X),
   replace_vars_with_constants(Type,m1_var(l(C)),Type2),
   replace_vars_with_constants(T,m1_var(r(C)),T2),!.

replace_vars_with_constants(lambda(X:Type,T),m1_var(C),lambda(X:Type2,T2)):-
   var(X),
   X = m1_var(l(C)),
   replace_vars_with_constants(Type,m1_var(m(C)),Type2),
   replace_vars_with_constants(T,m1_var(r(C)),T2),!.

replace_vars_with_constants(sigma(X:Type,T),m1_var(C),sigma(X:Type2,T2)):-
   \+ var(X),
   replace_vars_with_constants(Type,m1_var(l(C)),Type2),
   replace_vars_with_constants(T,m1_var(r(C)),T2),!.

replace_vars_with_constants(sigma(X:Type,T),m1_var(C),sigma(X:Type2,T2)):-
   var(X),
   X = m1_var(l(C)),
   replace_vars_with_constants(Type,m1_var(m(C)),Type2),
   replace_vars_with_constants(T,m1_var(r(C)),T2),!.

replace_vars_with_constants(pi(X:Type,T),m1_var(C),pi(X:Type2,T2)):-
   \+ var(X),
   replace_vars_with_constants(Type,m1_var(l(C)),Type2),
   replace_vars_with_constants(T,m1_var(r(C)),T2),!.

replace_vars_with_constants(pi(X:Type,T),m1_var(C),pi(X:Type2,T2)):-
   var(X),
   X = m1_var(l(C)),
   replace_vars_with_constants(Type,m1_var(m(C)),Type2),
   replace_vars_with_constants(T,m1_var(r(C)),T2),!.

replace_vars_with_constants(X-Y,m1_var(C),X2-Y2):-
   replace_vars_with_constants(X,m1_var(l(C)),X2),
   replace_vars_with_constants(Y,m1_var(r(C)),Y2).

replace_vars_with_constants(pair(X,Y),m1_var(C),pair(X2,Y2)):-
   replace_vars_with_constants(X,m1_var(l(C)),X2),
   replace_vars_with_constants(Y,m1_var(r(C)),Y2).

replace_vars_with_constants(def(X,Y),m1_var(C),def(X2,Y2)):-
   replace_vars_with_constants(X,m1_var(l(C)),X2),
   replace_vars_with_constants(Y,m1_var(r(C)),Y2).

replace_vars_with_constants(pi1(X),m1_var(C),pi1(X2)):-
   replace_vars_with_constants(X,m1_var(m(C)),X2).

replace_vars_with_constants(pi2(X),m1_var(C),pi2(X2)):-
   replace_vars_with_constants(X,m1_var(m(C)),X2).

check_all_bound(X,_,true):-
   atom(X),!.

check_all_bound(X,Binders,true):-
   \+ var(X),
   X = m1_var(_C),
   member(X,Binders),!.

check_all_bound(X,Binders,false):-
   \+ var(X),
   X = m1_var(_C),
   \+ member(X,Binders),!.

check_all_bound(lambda(X:Type,T),Binders,TV):-
   append(Binders,[X],NewBinders),
   check_all_bound(T,NewBinders,TV1),
   check_all_bound(Type,Binders,TV2),
   boolean_and(TV1,TV2,TV).

check_all_bound(sigma(X:Type,T),Binders,TV):-
   append(Binders,[X],NewBinders),
   check_all_bound(T,NewBinders,TV1),
   check_all_bound(Type,Binders,TV2),
   boolean_and(TV1,TV2,TV).

check_all_bound(pi(X:Type,T),Binders,TV):-
   append(Binders,[X],NewBinders),
   check_all_bound(T,NewBinders,TV1),
   check_all_bound(Type,Binders,TV2),
   boolean_and(TV1,TV2,TV).

check_all_bound(X-Y,Binders,TV):-
   check_all_bound(X,Binders,TV1),
   check_all_bound(Y,Binders,TV2),
   boolean_and(TV1,TV2,TV).

check_all_bound(pair(X,Y),Binders,TV):-
   check_all_bound(X,Binders,TV1),
   check_all_bound(Y,Binders,TV2),
   boolean_and(TV1,TV2,TV).

check_all_bound(def(X,Y),Binders,TV):-
   check_all_bound(X,Binders,TV1),
   check_all_bound(Y,Binders,TV2),
   boolean_and(TV1,TV2,TV).

check_all_bound(pi1(X),Binders,TV):-
   check_all_bound(X,Binders,TV).

check_all_bound(pi2(X),Binders,TV):-
   check_all_bound(X,Binders,TV).

boolean_and(true,true,true).
boolean_and(false,true,false).
boolean_and(true,false,false).
boolean_and(false,false,false).

% reduce_context

reduce_context([],[]):-!.

reduce_context([def(X,Y1):T1|Tail1],[def(X,Y2):T2|Tail2]):-
    reduce_term(Y1,Y2),
    reduce_term(T1,T2),
    reduce_context(Tail1,Tail2).

reduce_context([X:T1|Tail1],[X:T2|Tail2]):-
    \+ X = def(_,_),
    reduce_term(T1,T2),
    reduce_context(Tail1,Tail2).

reduce_term(T1,T2):-
    reduce(T1,T2,[TimeOut,SpaceOut,Cyclic,FreeVars,SafeTerm]),
    \+ var(TimeOut),
    \+ var(SpaceOut),
    \+ var(Cyclic),
    \+ var(FreeVars),
    \+ var(SafeTerm),
    TimeOut = success,
    SpaceOut = success,
    Cyclic = not_cyclic,
    FreeVars = no_free_vars,
    SafeTerm = safe,!.

reduce_term(T1,T2):-
    reduce(T1,T2,[TimeOut,SpaceOut,Cyclic,FreeVars,SafeTerm]),
    feedback('~n ALLIGATOR ERROR - Reduction of term ~p failed.~n',[T1]),
    throw(error_reduction_failed([TimeOut,SpaceOut,Cyclic,FreeVars,SafeTerm])),!,
    fail.


% Beta Reduction (for UNTYPED lambda calculus: type labels on
% terms are ignored)

% reduce(+Term,-Term,[-TimeOut,-SpaceOut,-Cyclic,-FreeVars,-SafeTerm])
% with TimeOut :== time_out | success
% with SpaceOut :== space_out | success
% with Cyclic   :== cyclic | not_cyclic
% with FreeVars :== free_vars | no_free_vars
% with SafeTerm :== safe | not_safe

% Do not reduce terms in which the same variable occurs bound several
% times

reduce(X,_Y,[_TimeOut,_SpaceOut,_Cyclic,FreeVars,_SafeTerm]):-
   copy_term(X,Test1),
   no_free_vars(Test1,false),
   FreeVars = free_vars,!.

% Do not reduce terms that contain unbound/free variables

reduce(X,_Y,[_TimeOut,_SpaceOut,_Cyclic,_FreeVars,SafeTerm]):-
   copy_term(X,Test1),
   safe_term(Test1,false),
   SafeTerm = not_safe,!.

reduce(X,Y,[TimeOut,SpaceOut,Cyclic,FreeVars,SafeTerm]):-
   copy_term(X,Test1),
   copy_term(X,Test2),
   no_free_vars(Test1,true),
   FreeVars = no_free_vars,
   safe_term(Test2,true),
   SafeTerm = safe,
   !,
   getval(reduce_time_out,TO),
   getval(reduce_space_out,SO),
                 % A reduction can take no longer than TO milliseconds
                 % and use SO of the global_stack
   garbage_collect,
   global_stack_info(Free),
   Free2 is Free*SO,
   Free3 is integer(Free2),
   copy_term(X,X1),
   time_out(space_out(reduce1(X1,Y),Free3,SpaceOut),TO,TimeOut),
   test_for_cycle(Y,Cyclic).

test_for_cycle(T,not_cyclic):-
   acyclic_term(T),!.

test_for_cycle(T,cyclic):-
   cyclic_term(T),!.

reduce1(X,X):-
    var(X),!.

reduce1(X,X):-
    atom(X),!.

% Note that it is crucial that we
% make sure that lambda(X:_A,B)-A2
% does not match in this rule
% with X-A2, hence the stepwise
% comparison of terms.

reduce1(T,R):-
    \+ var(T),
    T = A1-A2,
    \+ var(A1),
    A1 = lambda(X:_A,B),
    var(X),
    !,
    X = A2,
    reduce1(B,R).

reduce1(T,R):-
    \+ var(T),
    T = (X1-Y1),
    copy_term(X1-Y1,X3-Y3),
    reduce1(X1,X2),
    reduce1(Y1,Y2),
    \+ variant(X3-Y3,X2-Y2),  % avoid looping;
                              % variant: identical up to
                              % renaming of variables
    !,
    reduce1(X2-Y2,R).

reduce1(T,R):-
    \+ var(T),
    T = lambda(X:A1,B1),
    var(X),
    reduce1(A1,A2),
    reduce1(B1,B2),
    R = lambda(X:A2,B2),!.

% For lambda(X:a,X-a-(X-b))-lambda(Y:b,Y)
% After one step: (lambda(Y:b,Y)-a)-(lambda(Y:b,Y)-b)
% After two steps: a-(lambda(a:b,a)-b)
% We now use insert_var to replace the
% atom a in the lambda term again with fresh variable
% Var.

reduce1(T,R):-
    \+ var(T),
    T = lambda(X:A1,B1),
    \+ var(X),
    insert_var(lambda(X:A1,B1),X,Var,WV),
    WV = lambda(Var:A2,B2),
    reduce1(A2,A3),
    reduce1(B2,B3),
    R = lambda(Var:A3,B3),!.

reduce1(T,Y2):-
    \+ var(T),
    T = def(_X,Y1),
    reduce1(Y1,Y2),!.

reduce1(T,R):-
    \+ var(T),
    T = pair(X1,Y1),
    reduce1(X1,X2),
    reduce1(Y1,Y2),
    R = pair(X2,Y2),!.

reduce1(T,A2):-
    \+ var(T),
    T = pi1(Z),
    \+ var(Z),
    Z = pair(A1,_B1),
    reduce1(A1,A2),!.

reduce1(T,B2):-
    \+ var(T),
    T = pi2(Z),
    \+ var(Z),
    Z = pair(_A1,B1),
    reduce1(B1,B2),!.

reduce1(T,sigma(X:A2,B2)):-
    \+ var(T),
    T = sigma(X:A1,B1),
    reduce1(A1,A2),
    reduce1(B1,B2),!.

reduce1(T,pi(X:A2,B2)):-
    \+ var(T),
    T = pi(X:A1,B1),
    reduce1(A1,A2),
    reduce1(B1,B2),!.

reduce1(T,T).

% Replace non-variables in the term
% with real prolog variables.

replace_atoms_final_term(TermAt,TermVar):-
    !,
    replace_atoms_final_term1(TermAt,[],TermVar).

% Variables are mapped to themselves

replace_atoms_final_term1(X,_,X):-
    var(X),
    !.

replace_atoms_final_term1(pair(A1,B1),Subst,Var):-
    member((pair(A1,B1),Var),Subst),
    !.

replace_atoms_final_term1(pair(A1,B1),Subst,pair(A2,B2)):-
    !,
    replace_atoms_final_term1(A1,Subst,A2),
    replace_atoms_final_term1(B1,Subst,B2).


replace_atoms_final_term1(def(A1,B1),Subst,Var):-
    member((def(A1,B1),Var),Subst),
    !.

replace_atoms_final_term1(def(A1,B1),Subst,def(A2,B2)):-
    !,
    replace_atoms_final_term1(A1,Subst,A2),
    replace_atoms_final_term1(B1,Subst,B2).

replace_atoms_final_term1(A1-B1,Subst,Var):-
    member((A1-B1,Var),Subst),
    !.

replace_atoms_final_term1(A1-B1,Subst,A2-B2):-
    !,
    replace_atoms_final_term1(A1,Subst,A2),
    replace_atoms_final_term1(B1,Subst,B2).

replace_atoms_final_term1(pi1(A1),Subst,Var):-
    member((pi1(A1),Var),Subst),
    !.

replace_atoms_final_term1(pi1(A1),Subst,pi1(A2)):-
    !,
    replace_atoms_final_term1(A1,Subst,A2).

replace_atoms_final_term1(pi2(A1),Subst,Var):-
    member((pi2(A1),Var),Subst),
    !.

replace_atoms_final_term1(pi2(A1),Subst,pi2(A2)):-
    !,
    replace_atoms_final_term1(A1,Subst,A2).

replace_atoms_final_term1(lambda(X:A1,B1),Subst,Var):-
    member((lambda(X:A1,B1),Var),Subst),
    !.

% We have two clauses: one for cases where a binder
% has been substituted with an atom and one for
% those where that is not the case.

replace_atoms_final_term1(lambda(X:A1,B1),Subst,lambda(X:A2,B2)):-
    var(X),
    !,
    replace_atoms_final_term1(A1,Subst,A2),
    replace_atoms_final_term1(B1,Subst,B2).

replace_atoms_final_term1(lambda(X:A1,B1),Subst,lambda(Y:A2,B2)):-
    !,
    replace_atoms_final_term1(A1,Subst,A2),
    replace_atoms_final_term1(B1,[(X,Y)|Subst],B2).

replace_atoms_final_term1(sigma(X:A1,B1),Subst,Var):-
    member((sigma(X:A1,B1),Var),Subst),
    !.

replace_atoms_final_term1(sigma(X:A1,B1),Subst,sigma(X:A2,B2)):-
    var(X),
    !,
    replace_atoms_final_term1(A1,Subst,A2),
    replace_atoms_final_term1(B1,Subst,B2).

replace_atoms_final_term1(sigma(X:A1,B1),Subst,sigma(Y:A2,B2)):-
    !,
    replace_atoms_final_term1(A1,Subst,A2),
    replace_atoms_final_term1(B1,[(X,Y)|Subst],B2).

replace_atoms_final_term1(pi(X:A1,B1),Subst,Var):-
    member((pi(X:A1,B1),Var),Subst),
    !.

replace_atoms_final_term1(pi(X:A1,B1),Subst,pi(X:A2,B2)):-
    var(X),
    !,
    replace_atoms_final_term1(A1,Subst,A2),
    replace_atoms_final_term1(B1,Subst,B2).

replace_atoms_final_term1(pi(X:A1,B1),Subst,pi(Y:A2,B2)):-
    !,
    replace_atoms_final_term1(A1,Subst,A2),
    replace_atoms_final_term1(B1,[(X,Y)|Subst],B2).

replace_atoms_final_term1(A,Subst,B):-
    member((A,B),Subst),!.

replace_atoms_final_term1(A,_,A).

insert_var(T1,X,Var,T2):-
    \+ var(T1),
    T1 = lambda(X1:A1,B1),
    \+ var(X1),
    X = X1,!,
    T2 = lambda(Var:A1,B2),
    insert_var(B1,X,Var,B2).

insert_var(T1,X,Var,T2):-
    \+ var(T1),
    T1 = lambda(X1:A1,B1),
    var(X1),!,
    T2 = lambda(X1:A1,B2),
    insert_var(B1,X,Var,B2).

insert_var(T1,X,Var,T2):-
    \+ var(T1),
    T1 = pi(X1:A1,B1),!,
    T2 = pi(X1:A1,B2),
    insert_var(B1,X,Var,B2).

insert_var(T1,X,Var,T2):-
    \+ var(T1),
    T1 = sigma(X1:A1,B1),!,
    T2 = sigma(X1:A1,B2),
    insert_var(B1,X,Var,B2).

insert_var(T1,X,Var,T2):-
    \+ var(T1),
    T1 = pi1(B1),!,
    T2 = pi1(B2),
    insert_var(B1,X,Var,B2).

insert_var(T1,X,Var,T2):-
    \+ var(T1),
    T1 = pi2(B1),!,
    T2 = pi2(B2),
    insert_var(B1,X,Var,B2).

insert_var(T1,X,Var,T2):-
    \+ var(T1),
    T1 = B1-B2,!,
    T2 = B3-B4,
    insert_var(B1,X,Var,B3),
    insert_var(B2,X,Var,B4).

insert_var(T1,X,Var,T2):-
    \+ var(T1),
    T1 = pair(B1,B2),!,
    T2 = pair(B3,B4),
    insert_var(B1,X,Var,B3),
    insert_var(B2,X,Var,B4).

insert_var(T1,_X,_Var,T1):-
    var(T1),!.

insert_var(T1,X,Var,Var):-
    \+ var(T1),
    X = T1,!.


% Check dependencies

check_dependencies([]):-!.

check_dependencies([X:T1|Tail]):-
    f_variable(X),%de_bruijn
    fresh(X,Tail),%de_bruijn
    copy_term(T1,T2),
    check_type(T2,_T3,Tail),!,
    check_dependencies(Tail).

check_dependencies([def(X,_Y):T1|Tail]):-
    f_variable(X),
    fresh(X,Tail),  % further condition is _Y:T1 is provable on Tail
    copy_term(T1,T2),
    check_type(T2,_T3,Tail),!,
    check_dependencies(Tail).

check_dependencies([def(X,_Y):_|Tail]):-
    \+ fresh(X,Tail),
    feedback('~n ALLIGATOR ERROR - The variable ~p is not fresh.~n',[X]),
    throw(error_variable_not_fresh),
    !,fail.

check_dependencies([X:_T1|Tail]):-
    \+ fresh(X,Tail),
    feedback('~n ALLIGATOR ERROR - The variable ~p is not fresh.~n',[X]),
    throw(error_variable_not_fresh),
    !,fail.

check_type_with_feedback(T1,T2,Context):-
    check_type(T1,T2,Context),!.

check_type_with_feedback(T1,_T2,_C):-
    cyclic_term(T1),
    feedback('~n ALLIGATOR ERROR - The type ~p is cyclic. ~n',[T1]),
    throw(error_cyclic_type),
    !,fail.

check_type_with_feedback(T1,_T2,_C):-
    feedback('~n ALLIGATOR ERROR - ~p has no type. ~n',[T1]),
    throw(error_has_no_type),
    !,fail.

check_type(T1,S1,C1):-
    copy_term(T1,T2),
    copy_term(C1,C2),!,
    check_type2(T2,S1,C2).

check_type2(T1,_T2,_Context):-
    var(T1),
    feedback('~n ALLIGATOR ERROR - the type contains an unbound variable. ~n',[]),
    throw(error_unbound_variable),
    !,fail.

check_type2(T1,T2,Context1):-
    atom(T1),!,
    copy_term(Context1,Context2),
    member_or_def_or_sort(T1,T2,Context2).
%S2が返り値
check_type2(pi(X:A1,B1),S2,Context):-
    var(X),!,%de bruijnのためいらない
    sorts(S),
    member(S1,S),
    member(S2,S),
    pi_rules(Rules),
    member((S1,S2),Rules),%ok
    copy_term((pi(X:A1,B1),_B2,Context),(pi(CX:CA1,CB1),_CB2,CContext)),
    copy_term(A1,A3),
    check_type2(CA1,S1,CContext),
    fresh_variable(v,CX),
    check_type2(CB1,S2,[CX:A3|CContext]).

check_type2(sigma(X:A1,B1),S2,Context):-
    var(X),!,%ok
    sorts(S),
    member(S1,S),
    member(S2,S),
    sigma_rules(Rules),
    member((S1,S2),Rules),
    copy_term((sigma(X:A1,B1),_B2,Context),(sigma(CX:CA1,CB1),_CB2,CContext)),
    copy_term(A1,A3),
    check_type2(CA1,S1,CContext),
    fresh_variable(v,CX),
    check_type2(CB1,S2,[CX:A3|CContext]).

check_type2(A1-B1,T2,Context):-
    !,
    copy_term((A1-B1,T2,Context),(CA1-CB1,_CT2,CContext)),
    check_type2(CB1,T1,CContext),
    check_type2(CA1,pi(CX:T1,T2),CContext),
    CX=B1.

check_type2(lambda(X:A,B1),pi(X:A,B2),Context):-
    var(X),!,
    sorts(S),
    member(S1,S),
    copy_term((lambda(X:A,B1),pi(X:A,B2),Context),(lambda(CX:CA,CB1),pi(CX:CA,_CB2),CContext)),
    fresh_variable(v,CX),
    copy_term(A,A2),
    check_type2(CB1,B3,[CX:A2|CContext]),
    replace_atoms_final_term1(B3,[(CX,X)],B2),
    check_type2(pi(X:A,B2),S1,Context).

check_type2(pair(L,R),sigma(X:A1,B1),Context):-
    !,
    sorts(S),
    member(S1,S),
    copy_term((pair(L,R),sigma(X:A1,B1),Context),(pair(_CL,_CR),sigma(_CX:_CA1,CB1),CContext)),
    check_type2(L,A1,CContext),
    check_type2(R,CB1,CContext),
    replace_atoms_final_term1(CB1,[(L,X)],B1),
    copy_term(sigma(X:A1,B1),sigma(X3:A13,B13)),
    check_type2(sigma(X3:A13,B13),S1,CContext).

check_type2(pi1(X),A,Context):-
    !,
    copy_term((pi1(X),A,Context),(pi1(CX),_CA,CContext)),
    check_type2(CX,sigma(_Y:A,_B),CContext).

check_type2(pi2(X),B,Context):-
    !,
    copy_term((pi2(X),B,Context),(pi2(_CX),_CB,CContext)),
    check_type2(X,sigma(Y:_A,B1),CContext),
    copy_term((Y,B1),(Y2,B2)),
    Y2=dummy_atom,
    replace_atoms_final_term1(B2,[(Y2,pi1(X))],B).

member_or_def_or_sort(T1,T2,Context):-
    copy_term((T1,T2,Context),(CT1,_CT2,CContext)),
    member(CT1:T2,CContext),!.

member_or_def_or_sort(T1,T2,Context):-
   copy_term((T1,T2,Context),(CT1,_CT2,CContext)),
   member(def(CT1,_X):T2,CContext),!.

member_or_def_or_sort(T1,T2,_Context1):-
    copy_term((T1,T2,_Context2),(CT1,_CT2,_CContext)),
    sorts(S),
    axioms(A),
    member(CT1,S),
    member(T2,S),
    member((T1,T2),A),!.

%%% TYPE SYSTEM: LambdaPREDomega extended with Sigma Types

% The type system that we use here is LambdaPREDomega
% which can be used for higher order predicate logic.
% Sigma's can be added without losing consistency
% for Sigma(X:A:,B) with A:Set.


%%% SORTS

sorts([set,type_set,prop,type_prop]).

%%% AXIOMS

axioms([(set,type_set),
        (prop,type_prop)]).

%%% TYPE SYSTEM RULES

pi_rules([
       %  (type_set,set),        % maybe keep added for dn_set rule
       %  (prop,set),            % maybe keep added for dn_set rule
          (set,set),
          (set,type_prop),        % Can be used to make A->prop and
                                  % A->(A->prop).
	                            % The domains of unary preds and
                                  % binary relations.
          (type_prop,type_prop),  % (A->prop)->prop:type_prop;
                                  % ((A->prop)->prop->)prop:type_prop
                                  % Quantification over type_prop
                                  % covering all higher order
                                  % domains.
          (prop,prop),            % For forming implications.
          (set,prop),             % For universal quantification over set
                                  % types.
          (type_prop,prop)]).     % Quantification over domains of
                                  % type type_prop such
                                  % as A->prop and A->(A->prop).

sigma_rules([(set,prop),          % Existential quantification
                                  % over set types.
             (prop,prop)]).       % Conjunction.
                                  % Note that we cannot have the lambda
                                  % cube's (box,star) which is here
                                  % (type_set/type_prop,set/prop).
                                  % Addition of the latter rules makes
                                  % the system lose
                                  % strong normalization and consequently
                                  % consistency (see Geuvers, 1994).

%%% TYPE SYSTEMS INFERENCE RULES: FORWARD AND BACKWARD

%%% FORWARD INFERENCES

% forward(def(_X,Y):T,Result):-
%    forward(Y:T,Result),!.

forward(P:sigma(X:A,B),Count1,Result):-
   \+ var(A),
   \+ var(B),!,
   X = pi1(P),
   Count2 is Count1 + 1,
   forward(pi1(P):A,Count2,R1),
   forward(pi2(P):B,Count2,R2),
   append(R1,R2,Result).

forward(P:V,Count1,Result):-
   \+var(V),
   V = (A => W),
   \+var(W),
   W = sigma(Y:B,C),!,
   Count2 is Count1 + 1,
   Y = pi1(R0),
   get_i_vars(A,Vars),
   norm_app(P-Vars,R0),
   norm_lab(lambda(A,pi1(R0)),Lab1),
   norm_lab(lambda(A,pi2(R0)),Lab2),
   copy_term(Lab1,Lab3),
   copy_term(Lab2,Lab4),
   forward(Lab3:(A => B),Count2,R1),
   forward(Lab4:(A => C),Count2,R2),
   append(R2,R1,Result).

forward(P:V,Count1,Result):-
   \+var(V),
   V = (A1 => W),
   \+var(W),
   W = (A2 => C),!,
   Count2 is Count1 + 1,
   append(A1,A2,A),
   forward(P:(A => C),Count2,Result).

% We do not forward stats to themselves
% Counter is zero, means no forward
% inferencing took place

forward(_X:_T,1,[]):-!.

% We do map statements to themselves if they
% are part of a chain of forward inferences.

forward(X:T,Count,[X:T]):-
    Count > 1,!.

get_i_vars([],[]):-!.

get_i_vars([(A:_B)|T1],[A|T2]):-
   get_i_vars(T1,T2).

norm_app(X-[Y],X-Y):-!.

norm_app(X-[H|T],R):-
   norm_app((X-H)-T,R).
/*
norm_lab(lambda([],C),C):-!.

norm_lab(lambda([H|T],C),lambda(H,R)):-
   norm_lab(lambda(T,C),R).
*/

norm_lab(lambda([],C),C):-!.

%リストをリストじゃなくしようとしてる(バラしてる)
norm_lab(lambda([(A:B)|T],C),lambda((A:B1),R)):-
   copy_term(B,B1),
   norm_lab(lambda(T,C),R).


forward2(Stat,ProperNewStats):-
   Count is 1,
   forward(Stat,Count,NewStats),
   reverse(NewStats,RNewStats),
   add_stats(RNewStats,ProperNewStats).

% Add as *definitions*.

add_stats([],[]):-!.

add_stats([A:B|Tail1],[A:B|Tail2]):-
   atom(A),!,
   add_stats(Tail1,Tail2).

add_stats([A:B|Tail1],[def(V,A):B|Tail2]):-
   fresh_variable(v,V),
   add_stats(Tail1,Tail2).

% removeFromList(L1,L2,R)

% The result R is obtained by removing
% stats with the same proof object in L1 and L2
% from L2

removeFromList([],L,L).

removeFromList([H:_|T],L,R2):-
   remove1FromList(H,L,R1),
   removeFromList(T,R1,R2).

% remove1FromList/3

remove1FromList(_X,[],[]).

remove1FromList(X,[X:_|T1],T2):-
   !,
   remove1FromList(X,T1,T2).

remove1FromList(X,[H:T|T1],[H:T|T2]):-
   remove1FromList(X,T1,T2).

forward_context(X,R):-
   copy_term(X,X1),
   reverse(X1,Y),
   forward_con(Y,R1),
   removeFromList(R1,X,X2),
   append(R1,X2,R).

forward_con([],[]):-!.

forward_con([X:T|Tail],Result):-
   forward2(X:T,ProperNewStats),
   forward_con(Tail,Result2),
   append(Result2,ProperNewStats,Result).

%%% DEDUCE (BACKWARDS INFERENCE ENGINE)

% backward inferencing
% deduce(Rulename,Context,ParameterList,Conclusion).

deduce(type-ax,_Context,[Depth,STime,ETime|_T],Sort1:Sort2):-%ok
   axioms(Ax),
   member((Sort1,Sort2),Ax),
   below_max_time(STime,ETime),
   below_max_depth(Depth,_).

% note that the membership deduction rule is
% split into one for atomic vars and one for definitions
%deduce(membership,[def(v_2, pi2(_20190)):d, def(v_3, pi1(_20190)):c],[2,142,3600 | _T],P:c).
deduce(membership,Context,[Depth,STime,ETime|_T],X:T):-%ok
   below_max_time(STime,ETime),
   below_max_depth(Depth,_),
   member(X:T,Context),
   acyclic_term(T).

deduce(membership,Context,[Depth,STime,ETime|_T],X:T):-%pass
   below_max_time(STime,ETime),
   below_max_depth(Depth,_),
   member(def(_Y,X):T,Context),
   acyclic_term(T).

% pi-form first for pi with one antecedent.

deduce(pi-form,Context,[Depth,STime,ETime|T],Pi:Sort2):-
%与えられた型がtypeかkindの時
%与えられたcontextにおいてtype型かkind型である型T2を持つAJudgement(項はT1)のリストを再帰で取ってくる
%((forward (T1:T2)<-base T1 T2):context)においてtype型かkind型である項B1を再帰で取ってくる
%Arrow [T1] B1を返す
   \+ var(Pi),
   Pi = A => B,
   A = [_H],!,
   below_max_time(STime,ETime),
   below_max_depth(Depth,NewDepth),
   copy_term(A=>B,A1=>B1),
   A1 = [_T1:T2],
   fresh_stats(A1,FStats),%pass(de_bruijn)
   forward_context(FStats,Ext),
   append(Ext,Context,ExtendedContext),
   sorts(S),
   member(Sort1,S),
   member(Sort2,S),
   pi_rules(Rules),
   member((Sort1,Sort2),Rules),
   deduce(_,Context,[NewDepth,STime,ETime|T],T2:Sort1),
   deduce(_,ExtendedContext,[NewDepth,STime,ETime|T],B1:Sort2).

% pi-form recursion for pi with multiple antecedents.

deduce(pi-form,Context,[Depth,STime,ETime|T],Pi:Sort2):-
%与えられた型がtypeかkindの時
%与えられたcontextにおいてtype型かkind型である型T2を持つAJudgement(項はT1)のリストを再帰で取ってくる
%((forward (T1:T2)<-base T1 T2):context)においてtype型かkind型である項[__]=>B1を再帰で取ってくる
%Arrow [T1] B1を返す
   \+ var(Pi),
   Pi = A => B,
   length(A,Num),
   Num>1,!,
   below_max_time(STime,ETime),
   below_max_depth(Depth,NewDepth),
   copy_term(A=>B,A1=>B1),
   A1 = [T1:T2|Tail],
   fresh_stats([T1:T2],FStats),
   forward_context(FStats,Ext),
   append(Ext,Context,ExtendedContext),
   sorts(S),
   member(Sort1,S),
   member(Sort2,S),
   pi_rules(Rules),
   member((Sort1,Sort2),Rules),
   deduce(_,Context,[NewDepth,STime,ETime|T],T2:Sort1),
   deduce(_,ExtendedContext,[NewDepth,STime,ETime|T],(Tail=>B1):Sort2).

% Checking of arrow type omitted
%forward_context([u0:q,u1:p,u2:sigma(B:p,u0)],R).
%search_proof([def(v_21, pi2(u2)):u0, def(v_20, pi1(u2)):p, u0:q, u1:p, u2:sigma(B:p, u0)],P:[C:p]=>u0,2,5,142,3600).
deduce(pi-intro,Context,[Depth,STime,ETime|T],Lab:(A => B)):-%ok
%与えられた型がArrow _ _の時
%(forward_context A):ContextでdeduceするとB型を持つC
%AをばらしてLam a1 Lam a2 ... C
   below_max_time(STime,ETime),
   below_max_depth(Depth,NewDepth),
   copy_term((A => B),(A1 => B1)),
   reverse(A1,A2),
   fresh_stats(A2,FStats),
   forward_context(FStats,Ext),
   append(Ext,Context,ExtendedContext),
   deduce(_,ExtendedContext,[NewDepth,STime,ETime|T],C:B1),
   norm_lab(lambda(A1,C),Lab).

% This one is tricky. We want to avoid variable capture

deduce(pi-elim,Context,[Depth,STime,ETime|T],App:B1):-
%
%contextにA=>B1型のFがある
%今のcontextでdeduceしてAが全部項を持つ
%Aから項を全部取ってくる[Vars]
%[Vars]をばらしていい感じにApp App
   below_max_time(STime,ETime),
   below_max_depth(Depth,NewDepth),
   member(F:(A2 => B2),Context),
   copy_term((A2 => B2),(A1 => B1)),
   acyclic_term((A1 => B1)),
   deduce_list(Context,[NewDepth,STime,ETime|T],A1),
   get_i_vars(A1,Vars),
   norm_app(F-Vars,App).


deduce(sigma-form,Context,[Depth,STime,ETime|T],Sigma:Sort2):-
%与えられた型がtypeかkindの時
%与えられたcontextにおいてtype型かkind型である型A1を持つAJudgement(項はT1)のリストを再帰で取ってくる
%((forward A1<-orgin X<-base A1 ):context)においてtype型かkind型である項B1を再帰で取ってくる
%Arrow_Sigma A1 B1
   \+ var(Sigma),
   Sigma = sigma(X:A,B),
   below_max_time(STime,ETime),
   below_max_depth(Depth,NewDepth),
   copy_term(sigma(X:A,B),sigma(X1:A1,B1)),
   fresh_stats([X1:A1],FStats),
   forward_context(FStats,Ext),
   append(Ext,Context,ExtendedContext),
   sorts(S),
   member(Sort1,S),
   member(Sort2,S),
   sigma_rules(Rules),
   member((Sort1,Sort2),Rules),
   deduce(_,Context,[NewDepth,STime,ETime|T],A1:Sort1),
   deduce(_,ExtendedContext,[NewDepth,STime,ETime|T],B1:Sort2).

%deduce(sigma-intro,[def(v_2, pi2(_20190)):d, def(v_3, pi1(_20190)):c],[2,142,3600 | _T],P:sigma(T:c,d)).
deduce(sigma-intro,Context,[Depth,STime,ETime|T],pair(C1,C2):sigma(X:A,B1)):-%ok
%与えられたcontextにおいてA型のC1がある
%与えられたコンテクストにおいてB1型のC2がある
%pair C1 C2
   below_max_time(STime,ETime),
   below_max_depth(Depth,NewDepth),
   copy_term(sigma(X:A,B1),sigma(Y1:A1,B2)),
   deduce(_,Context,[NewDepth,STime,ETime|T],C1:A1),
   Y1 = C1,
   deduce(_,Context,[NewDepth,STime,ETime|T],C2:B2).

% The pi1 and p2 elim rule are not present.
% These are taken care of by the forward inferencing
% rules.

% The beta conversion rule is not present because we
% normalize all types beforehand, so they are in normal form.
% Furthermore there is no eta reduction (extensionality).
% eta affects decidability of type checking (see Barendregt
% & Geuvers).

deduce_list(_Context,_,[]):-!.

deduce_list(Context,[NewDepth,STime,ETime|T],[Stat|Rest]):-
   deduce(_,Context,[NewDepth,STime,ETime|T],Stat),
   deduce_list(Context,[NewDepth,STime,ETime|T],Rest).

below_max_depth(Depth,NewDepth):-
   Depth > 0,
   NewDepth is Depth - 1.

fresh_stats([],[]):-!.%pass(de_bruijn)

fresh_stats([X:A|T1],[X:A|T2]):-%pass
   fresh_variable(v,V),
   X = V,
   fresh_stats(T1,T2).

%%% UTILITIES

feedback(A,B):-
    getval(feedback,on),!,
    format(A,B).

feedback(_A,_B).

feedback_list([]):-
   getval(feedback,on),!,
   format('       empty ~n',[]).

feedback_list(L1):-
   getval(feedback,on),!,
   reverse(L1,L2),
   present_list(L2).

feedback_list(_L).

present_list([]):-!.

present_list([H|T]):-
   format('       ~p ~n',[H]),
   present_list(T).

concat_atoms(P,N,V):-
   name(P,L),
   name(N,M),
   append(L,M,Z),
   name(V,Z).

%%% ON START UP

% :- nl.
% :- format('~4c Alligator 1.0, January 2006.~n',[0' ]).
% :- format('~4c Author: Paul Piwek.~2n',[0' ]).
% :- getval(feedback,Feedback),
%    format('~5c - feedback ~p; ~n',[0' ,Feedback]).
% :- report_active_contexts.
% :- getval(maxtime,MaxTime),
%    format('~5c - maximum search time: ~p seconds; ~n',[0' ,MaxTime]).
% :- getval(startdepth,StartDepth),
%    format('~5c - start search depth: ~p; ~n',[0' ,StartDepth]).
% :- getval(maxdepth,MaxDepth),
%    format('~5c - maximum search depth: ~p; ~n',[0' ,MaxDepth]).
% :- getval(reduce_time_out,Reduce_time_out),
%    format('~5c - maximum time for reduce: ~p milliseconds; ~n',[0' ,Reduce_time_out]).
% :- getval(reduce_space_out,Reduce_space_out),
%    format('~5c - maximal proportion of global stack for reduce: ~p. ~2n',[0' ,Reduce_space_out]).
