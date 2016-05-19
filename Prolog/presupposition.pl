:- use_module(betaConversion,[betaConvert/2]).

:- use_module(comsemPredicates,[substitute/4]).

:- use_module(alphaConversion,[alphaConvert/2,
                               alphabeticVariants/2]).

:- use_module(presupUnittest,[checkTypeTest/4,
                              resolvePresupTest/4]).

:- dynamic current_counter/1.

main :-
    current_prolog_flag(os_argv, AllArgs),
    append(_, [-- |[Args]], AllArgs),
    term_string(X,Args),
    resolvePresup(SR,[],X),
    write(SR).

current_counter(0).
counter(M):-
    retract(current_counter(N)),
    M is N+1,
    asserta(current_counter(M)),!.


%%%%% Main predicates %%%%%

resolvePresup(SR,GCxt,Form0):-
    addType(Form0,Form1),
    rewriteVar(Form1,Form),
    findall(PS,checkType(PS,GCxt,Form),Store),
    elimAsp(Store,GCxt,Form,SR0),
    betaConvertPi(SR0,SR1),
    betaConvert(SR1,SR).

addToStore(_,[],[]).

addToStore(Hyp,[[LCxt,Asp]|Store0],[[LCxtAccom,Asp]|Store]):-
    append([Hyp],LCxt,LCxtAccom),
    addToStore(Hyp,Store0,Store).

elimAsp([],_,Form,Form).

elimAsp([[LCxt,@(N,Type)]|Store0],GCxt,Form0,SR):-
    findProofTerm(LCxt,Term,Type),
    bindPresup(LCxt,Term,@(N,Type),Form0,Form),
    \+ violateConditionB(Form0,Form),
    substitute(Term,@(N,Type),Store0,Store),
    elimAsp(Store,GCxt,Form,SR), !.

elimAsp([[LCxt,@(N,Type)]|Store0],GCxt,Form0,SR):-
    counter(New),
    atom_concat(t,New,V),
    substitute(V,@(N,Type),Store0,Store1),
    substitute(V,@(N,Type),Form0,Form1),
    accomPresup(LCxt,[V,Type],Store1,Store,[],Form1,Form),
    elimAsp(Store,GCxt,Form,SR).


%%% Accommodation %%%

accomPresup(_,[V,Type],Store0,Store1,Binder,Form,exists(V,Type,Form)):-
    freeVar(Type,FV),
    subtract(FV,[V],Unbound),
    subset(Unbound,Binder),
    addToStore([V,Type],Store0,Store1), !.

accomPresup([[X,_]|Rest],[V,Type],Store0,Store1,Binder0,exists(X,T,A),exists(X,T,B)):-
    append(Binder0,[X],Binder),
    accomPresup(Rest,[V,Type],Store0,Store1,Binder,A,B), !.

accomPresup([[X,_]|Rest],[V,Type],Store0,Store1,Binder,exists(Y,T,A),exists(Y,T,B)):-
    X \== Y,
    accomPresup(Rest,[V,Type],Store0,Store1,Binder,A,B), !.

accomPresup([[X,_]|Rest],[V,Type],Store0,Store1,Binder0,forall(X,T,A),forall(X,T,B)):-
    append(Binder0,[X],Binder),
    accomPresup(Rest,[V,Type],Store0,Store1,Binder,A,B), !.

accomPresup([[X,_]|Rest],[V,Type],Store0,Store1,Binder,forall(Y,T,A),forall(Y,T,B)):-
    X \== Y,
    accomPresup(Rest,[V,Type],Store0,Store1,Binder,A,B), !.

accomPresup([[_,A1]|Rest],[V,Type],Store0,Store1,Binder,and(A1,A2),and(A1,B)):-
    accomPresup(Rest,[V,Type],Store0,Store1,Binder,A2,B), !.

accomPresup([[_,A1]|Rest],[V,Type],Store0,Store1,Binder,imp(A1,A2),imp(A1,B)):-
    accomPresup(Rest,[V,Type],Store0,Store1,Binder,A2,B), !.

accomPresup(LCxt,[V,Type],Store0,Store1,Binder,not(A),not(B)):-
    accomPresup(LCxt,[V,Type],Store0,Store1,Binder,A,B), !.

accomPresup(LCxt,[V,Type],Store0,Store1,Binder,acci(X,A),acci(X,B)):-
    accomPresup(LCxt,[V,Type],Store0,Store1,Binder,A,B), !.


%%% Binding %%%

bindPresup(LCxt,Term,@(N,Type),Form0,Form1):-
    subterm(po(u,Num),Term),
    member([po(u,Num),Body],LCxt),
    atom_concat(u,Num,BindVar),
    subterm(and(Body,Right),Form0),
    substitute(exists(BindVar,Body,Right),and(Body,Right),Form0,Form),
    substitute(BindVar,po(u,Num),Term,PTerm),
    substitute(PTerm,@(N,Type),Form,Form1), !.

bindPresup(LCxt,Term,@(N,Type),Form0,Form1):-
    subterm(po(u,Num),Term),
    member([po(u,Num),Body],LCxt),
    atom_concat(u,Num,BindVar),
    subterm(imp(Body,Right),Form0),
    substitute(forall(BindVar,Body,Right),imp(Body,Right),Form0,Form),
    substitute(BindVar,po(u,Num),Term,PTerm),
    substitute(PTerm,@(N,Type),Form,Form1), !.

bindPresup(_,Term,@(N,Type),Form0,Form1):-
    substitute(Term,@(N,Type),Form0,Form1).

violateConditionB(Form1,Form2):-
    subterm(Pred,Form2),
    Pred =.. [_,X,X],
    \+ subterm(Pred,Form1), !.

violateConditionB(Form1,Form2):-
    subterm(Pred1,Form2),
    Pred1 =.. [eq,Arg1,X],
    Arg1 =.. [subj,Y],
    subterm(Pred2,Form2),
    Pred2 =.. [eq,Arg2,X],
    Arg2 =.. [acc,Y],
    \+ (subterm(Pred1,Form1), subterm(Pred2,Form1)), !.


%%% Proof search for Dependent Type Theory %%%

findProofTermInit(Gamma,Term,Type0):-
    addType(Type0,Type),
    applyElim(Gamma,Redex0,Type),
    betaConvertPi(Redex0,Redex),
    betaConvert(Redex,Term).

findProofTerm(Gamma,Term,Type):-
    applyElim(Gamma,Redex0,Type),
    betaConvertPi(Redex0,Redex),
    betaConvert(Redex,Term).

applyElim(Gamma,Term,A):-
    \+ member([_,and(_,_)],Gamma),
    \+ member([_,exists(_,_,_)],Gamma), !,
    applyIntro(Gamma,Term,A).

applyElim([[T,and(A,B)]|Gamma],Term,C):-
    applyElim([[pi1(T),A],[pi2(T),B]|Gamma],Term,C).

applyElim([[T,exists(Y,Type,A)]|Gamma],Term,C):-
    substitute(pi1(T),Y,A,B),
    applyElim([[pi1(T),Type],[pi2(T),B]|Gamma],Term,C).

applyElim([[T,Body]|Gamma],Term,C):-
    \+ Body =.. [and|_],
    \+ Body =.. [exists|_],
    append(Gamma,[[T,Body]],Permuted),
    applyElim(Permuted,Term,C).

applyIntro(Gamma,Term,A):-
    member([Term,A],Gamma).

applyIntro(Gamma,[T,U],and(A,B)):-
    applyIntro(Gamma,T,A),
    applyIntro(Gamma,U,B).

applyIntro(Gamma,[T,U],exists(X,Type,A)):-
    applyElim(Gamma,T,Type),
    substitute(T,X,A,B),
    applyIntro(Gamma,U,B).


%%% Type-checking for Dependent Type Theory %%%

checkTypeInit(PS,Cxt,Form):- !,
    addType(Form,Form1),
    checkType(PS,Cxt,Form1).

checkType(PS,Cxt,exists(_,Type,_)):-
    checkType(PS,Cxt,Type).

checkType(PS,Cxt1,exists(X,Type,F)):-
    append(Cxt1,[[X,Type]],Cxt2), !,
    checkType(PS,Cxt2,F).

checkType(PS,Cxt,forall(_,Type,_)):-
    checkType(PS,Cxt,Type).

checkType(PS,Cxt1,forall(X,Type,F)):-
    append(Cxt1,[[X,Type]],Cxt2), !,
    checkType(PS,Cxt2,F).

checkType(PS,Cxt,and(F1,_)):-
    checkType(PS,Cxt,F1).

checkType(PS,Cxt1,and(F1,F2)):-
    counter(N),
    append(Cxt1,[[po(u,N),F1]],Cxt2), !,
    checkType(PS,Cxt2,F2).

checkType(PS,Cxt,imp(F1,_)):-
    checkType(PS,Cxt,F1).

checkType(PS,Cxt1,imp(F1,F2)):-
    counter(N),
    append(Cxt1,[[po(u,N),F1]],Cxt2), !,
    checkType(PS,Cxt2,F2).

checkType(PS,Cxt,F):-
    F =.. [_,T],
    checkType(PS,Cxt,T).

checkType(PS,Cxt,F):-
    F =.. [_,T,_],
    checkType(PS,Cxt,T).

checkType(PS,Cxt,F):-
    F =.. [_,_,T],
    checkType(PS,Cxt,T).

checkType(PS,Cxt,F):-
    F =.. [_,T,_,_],
    checkType(PS,Cxt,T).

checkType(PS,Cxt,F):-
    F =.. [_,_,T,_],
    checkType(PS,Cxt,T).

checkType(PS,Cxt,F):-
    F =.. [_,_,_,T],
    checkType(PS,Cxt,T).

checkType(PS,Cxt,F):-
    F =.. [_,T,_,_,_],
    checkType(PS,Cxt,T).

checkType(PS,Cxt,F):-
    F =.. [_,_,T,_,_],
    checkType(PS,Cxt,T).

checkType(PS,Cxt,F):-
    F =.. [_,_,_,T,_],
    checkType(PS,Cxt,T).

checkType(PS,Cxt,F):-
    F =.. [_,_,_,_,T],
    checkType(PS,Cxt,T).

checkType([Cxt,@(N,Type)],Cxt,@(N,Type)).


%%% Auxiliary predicates %%%

betaConvertPi(pi1([X,_]),Y):-
    betaConvertPi(X,Y), !.

betaConvertPi(pi2([_,X]),Y):-
    betaConvertPi(X,Y), !.

betaConvertPi(po(X,Y),po(X,Y)).

betaConvertPi(X,Y):-
    X =.. [F,A],
    betaConvertPi(A,B),
    Y =.. [F,B], !.
betaConvertPi(X,Y):-
    X =.. [F,A1,A2],
    betaConvertPi(A1,B1),
    betaConvertPi(A2,B2),
    Y =.. [F,B1,B2], !.
betaConvertPi(X,Y):-
    X =.. [F,A1,A2,A3],
    betaConvertPi(A1,B1),
    betaConvertPi(A2,B2),
    betaConvertPi(A3,B3),
    Y =.. [F,B1,B2,B3], !.
betaConvertPi(X,X).

freeVar(exists(X,Type,A),FV):-
    freeVar(Type,FV1),
    freeVar(A,FV2),
    subtract(FV2,[X],FV3),
    append(FV1,FV3,FV), !.

freeVar(exists(X,A),FV):-
    freeVar(A,FV1),
    subtract(FV1,[X],FV), !.

freeVar(forall(X,Type,A),FV):-
    freeVar(Type,FV1),
    freeVar(A,FV2),
    subtract(FV2,[X],FV3),
    append(FV1,FV3,FV), !.
  
freeVar(forall(X,A),FV):-
    freeVar(A,FV1),
    subtract(FV1,[X],FV), !.

freeVar(lam(X,Type,A),FV):-
    freeVar(Type,FV1),
    freeVar(A,FV2),
    subtract(FV2,[X],FV3),
    append(FV1,FV3,FV), !.

freeVar(lam(X,A),FV):-
    freeVar(A,FV1),
    subtract(FV1,[X],FV), !.

freeVar(Form,FV):-
    Form =.. [_,Arg],
    freeVar(Arg,FV), !.

freeVar(Form,FV):-
    Form =.. [_,Arg1,Arg2],
    freeVar(Arg1,FV1),
    freeVar(Arg2,FV2),
    append(FV1,FV2,FV), !.

freeVar(Form,FV):-
    Form =.. [_,Arg1,Arg2,Arg3],
    freeVar(Arg1,FV1),
    freeVar(Arg2,FV2),
    freeVar(Arg3,FV3),
    append(FV1,FV2,X),
    append(X,FV3,FV), !.

freeVar(Form,FV):-
    Form =.. [_,Arg1,Arg2,Arg3,Arg4],
    freeVar(Arg1,FV1),
    freeVar(Arg2,FV2),
    freeVar(Arg3,FV3),
    freeVar(Arg4,FV4),
    append(FV1,FV2,X),
    append(FV3,FV4,Y),
    append(X,Y,FV), !.

freeVar(entity,[]).
freeVar(event,[]).
freeVar(state,[]).

freeVar(A,[A]):-
    atom(A), !.

freeVar(_,[]).

addType(exists(X,A),exists(X,event,Form)):-
    X == v,
    addType(A,Form), !.

addType(exists(X,A),exists(X,entity,Form)):-
    addType(A,Form), !.

addType(forall(X,A),forall(X,event,Form)):-
    X == v,
    addType(A,Form), !.

addType(forall(X,A),forall(X,entity,Form)):-
    addType(A,Form), !.

addType(lam(X,A),lam(X,event,Form)):-
    X == v,
    addType(A,Form), !.

addType(lam(X,A),lam(X,entity,Form)):-
    addType(A,Form), !.
       
addType(and(A0,B0),and(A,B)):-
    addType(A0,A),
    addType(B0,B), !.

addType(imp(A0,B0),imp(A,B)):-
    addType(A0,A),
    addType(B0,B), !.

addType(not(A0),not(A)):-
    addType(A0,A), !.

addType(Form0,Form):-
    Form0 =.. [F,A0],
    addType(A0,A),
    Form =.. [F,A], !.

addType(Form0,Form):-
    Form0 =.. [F,A0,B0],
    addType(A0,A),
    addType(B0,B),
    Form =.. [F,A,B], !.

addType(Form0,Form):-
    Form0 =.. [F,A0,B0,C0],
    addType(A0,A),
    addType(B0,B),
    addType(C0,C),
    Form =.. [F,A,B,C], !.

addType(Form0,Form):-
    Form0 =.. [F,A0,B0,C0,D0],
    addType(A0,A),
    addType(B0,B),
    addType(C0,C),
    addType(D0,D),
    Form =.. [F,A,B,C,D], !.

addType(A,A):-
    atomic(A).

rewriteVar(Form1,Form2):-
    rewriteVar([],Form1,Form2).

rewriteVar(Stack,exists(X,Type,A),exists(V,Type,Form)):-
    member(X,Stack),
    counter(N),
    atom_concat(x,N,V),
    append(Stack,[V],NewStack),
    substitute(V,X,A,B),
    rewriteVar(NewStack,B,Form), !.

rewriteVar(Stack,exists(X,Type,A),exists(X,Type,Form)):-
    append(Stack,[X],NewStack),
    rewriteVar(NewStack,A,Form), !.

rewriteVar(Stack,forall(X,Type,A),forall(V,Type,Form)):-
    member(X,Stack),
    counter(N),
    atom_concat(x,N,V),
    append(Stack,[V],NewStack),
    substitute(V,X,A,B),
    rewriteVar(NewStack,B,Form), !.

rewriteVar(Stack,forall(X,Type,A),forall(X,Type,Form)):-
    append(Stack,[X],NewStack),
    rewriteVar(NewStack,A,Form), !.

rewriteVar(Stack,lam(X,A),lam(X,Form)):-
    member(X,Stack),
    counter(N),
    atom_concat(x,N,V),
    append(Stack,[X],NewStack),
    substitute(V,X,A,B),
    rewriteVar(NewStack,B,Form), !.

rewriteVar(Stack,lam(X,A),lam(X,B)):-
    append(Stack,[X],NewStack),
    rewriteVar(NewStack,A,B), !.

rewriteVar(Stack,lam(X,Type,A),lam(X,Type,Form)):-
    member(X,Stack),
    counter(N),
    atom_concat(x,N,V),
    append(Stack,[X],NewStack),
    substitute(V,X,A,B),
    rewriteVar(NewStack,B,Form), !.

rewriteVar(Stack,lam(X,Type,A),lam(X,Type,B)):-
    append(Stack,[X],NewStack),
    rewriteVar(NewStack,A,B), !.

rewriteVar(Stack,and(A0,B0),and(A,B)):-
    rewriteVar(Stack,A0,A),
    rewriteVar(Stack,B0,B), !.

rewriteVar(Stack,imp(A0,B0),imp(A,B)):-
    rewriteVar(Stack,A0,A),
    rewriteVar(Stack,B0,B), !.

rewriteVar(Stack,Form0,Form):-
    Form0 =.. [F,A0],
    rewriteVar(Stack,A0,A),
    Form =.. [F,A], !.

rewriteVar(Stack,Form0,Form):-
    Form0 =.. [F,A0,B0],
    rewriteVar(Stack,A0,A),
    rewriteVar(Stack,B0,B),
    Form =.. [F,A,B], !.

rewriteVar(_,A,A):-
    atomic(A).

containVar(Exp):-
    var(Exp), !.
containVar(Exp):-
    Exp =.. [_|Arg],
    member(A,Arg),
    containVar(A), !.

subterm(T,T).
subterm(T1,T2):-
    T2 =.. [_|As],
    member(A,As),
    subterm(T1,A).

writeline([H|T]):-
    write(H),nl,writeline(T).


%%%%%%%% Test Suite %%%%%%%%

testcheckType:-
   format('~n ##### Type-Checking Unittest #####',[]),
   checkTypeTest(Number,Gold,Cxt,Formula),
   numbervars(Gold),
   format('~n~nFormula ~p: ~p~nGold: ~p',[Number,Formula,Gold]),
   findall(Result,checkTypeInit(Result,Cxt,Formula),AllResult),
   numbervars(AllResult),
   format('~nSyst: ~p',[AllResult]),
   compareResults(Gold,AllResult,Answer),
   format('~nResult: ~p',[Answer]),
   retract(current_counter(_)),
   asserta(current_counter(0)),
   fail.

testcheckType.

testresolvePresup:-
   format('~n~n ##### Presupposition Resolution Unittest #####',[]),
   resolvePresupTest(Number,Gold,Cxt,Formula),
   numbervars(Gold),
   format('~n~nFormula ~p: ~p~nGold: ~p',[Number,Formula,Gold]),
   findall(Result,resolvePresup(Result,Cxt,Formula),AllResult),
   numbervars(AllResult),
   format('~nSyst: ~p',[AllResult]),
   compareResults(Gold,AllResult,Answer),
   format('~nResult: ~p',[Answer]),
   retract(current_counter(_)),
   asserta(current_counter(0)),
   fail.

testresolvePresup.

testAll:-
    testcheckType, testresolvePresup.

compareResults(A,B,Result):-
    (
     alphabeticVariants(A,B),
     !,
     Result=ok
    ;
     Result=error
    ).
