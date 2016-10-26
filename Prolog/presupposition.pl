:- use_module(betaConversion,[betaConvert/2]).

:- use_module(comsemPredicates,[substitute/4]).

:- use_module(alphaConversion,[alphaConvert/2,
                               alphabeticVariants/2]).

:- use_module(presupUnittest,[checkTypeTest/4,
                              resolvePresupTest/4]).

:- dynamic current_counter/1.

main :-
    current_prolog_flag(os_argv, AllArgs),
    append(_, [-- |[Arg1,Arg2]], AllArgs),
    term_string(UnspecSR,Arg1),
    term_string(GCxt,Arg2),
    resolvePresup(SR,GCxt,UnspecSR),
    write(SR).

% main :-
%     current_prolog_flag(os_argv, AllArgs),
%     append(_, [-- |[Args]], AllArgs),
%     term_string(X,Args),
%     resolvePresup(SR,[],X),
%     write(SR).

current_counter(0).
counter(M):-
    retract(current_counter(N)),
    M is N+1,
    asserta(current_counter(M)),!.


%%%%% Main predicates %%%%%

resolvePresup(SR,GCxt,Form0):-
    % elimCont(Form0,FormE), %%% temporary repair
    % elimSigma(FormE,FormF),
    addType(Form0,Form1),
    rewriteVar(Form1,Form2),
    elimTrue(Form2,Form),
    findall(PS,checkType(PS,[],Form),Store),
    elimAsp(Store,GCxt,Form,SR0),
    betaConvertPi(SR0,SR1),
    betaConvert(SR1,SR), !.

%%% temporary repair %%%
elimCont(lam(x0,A),B):-
    subterm(x0(X),A),
    substitute(true,x0(X),A,B), !.

elimCont(A,A).
%%%%%%%

% addToStore(_,[],[]).

% addToStore(Hyp,[[LCxt,Asp]|Store0],[[LCxtAccom,Asp]|Store]):-
%     append([Hyp],LCxt,LCxtAccom),
%     addToStore(Hyp,Store0,Store).

elimAsp([],_,Form,Form).

elimAsp([[LCxt,@(N,Type)]|Store0],GCxt,Form0,SR):-
    findProofTerm(GCxt,LCxt,Term,Type),
    bindPresup(LCxt,Term,@(N,Type),Form0,Form),
    \+ violateConditionB(Form0,Form),
    substitute(Term,@(N,Type),Store0,Store),
    elimAsp(Store,GCxt,Form,SR), !.

elimAsp([[_,@(N,Type)]|_],GCxt,Form,SR):-
    counter(New),
    atom_concat(t,New,V),
    substitute(V,@(N,Type),Form,FormTmp),
    freeVar(Type,FV),
    subtract(FV,[V],Unbound),
    accomPresup([V,Type],Unbound,FormTmp,FormAccom),
    findall(PS,checkType(PS,[],FormAccom),NewStore),
    % findall(PS,checkType(PS,GCxt,FormAccom),NewStore),
    elimAsp(NewStore,GCxt,FormAccom,SR).

% Pi-accommodation
accomPresup([V,Type],[],Form,forall(V,Type,Form)).

% Sigma-accommodation
% accomPresup([V,Type],[],Form,exists(V,Type,Form)).

accomPresup([V,Type],Unbound,exists(X,T1,A),exists(X,T2,A)):-
    boundVar(T1,BV),
    subset(Unbound,BV),
    accomPresup([V,Type],Unbound,T1,T2), !.

accomPresup([V,Type],Unbound,exists(X,T,A),exists(X,T,B)):-
    subtract(Unbound,[X],UnboundOut),
    accomPresup([V,Type],UnboundOut,A,B), !.

accomPresup([V,Type],Unbound,forall(X,T1,A),forall(X,T2,A)):-
    boundVar(T1,BV),
    subset(Unbound,BV),
    accomPresup([V,Type],Unbound,T1,T2), !.

accomPresup([V,Type],Unbound,forall(X,T,A),forall(X,T,B)):-
    subtract(Unbound,[X],UnboundOut),
    accomPresup([V,Type],UnboundOut,A,B), !.

accomPresup([V,Type],Unbound,Form1,Form2):-
    Form1 =.. [P,A],
    accomPresup([V,Type],Unbound,A,B),
    Form2 =.. [P,B], !.

accomPresup([V,Type],Unbound,Form1,Form2):-
    Form1 =.. [P,A1,A2],
    boundVar(A1,BV),
    subset(Unbound,BV),
    accomPresup([V,Type],Unbound,A1,B1),
    Form2 =.. [P,B1,A2], !.

accomPresup([V,Type],Unbound,Form1,Form2):-
    Form1 =.. [P,A1,A2],
    accomPresup([V,Type],Unbound,A2,B2),
    Form2 =.. [P,A1,B2], !.

accomPresup([V,Type],Unbound,Form1,Form2):-
    Form1 =.. [P,A1,A2,A3],
    boundVar(A1,BV),
    subset(Unbound,BV),
    accomPresup([V,Type],Unbound,A1,B1),
    Form2 =.. [P,B1,A2,A3], !.

accomPresup([V,Type],Unbound,Form1,Form2):-
    Form1 =.. [P,A1,A2,A3],
    boundVar(A2,BV),
    subset(Unbound,BV),
    accomPresup([V,Type],Unbound,A2,B2),
    Form2 =.. [P,A1,B2,A3], !.

accomPresup([V,Type],Unbound,Form1,Form2):-
    Form1 =.. [P,A1,A2,A3],
    accomPresup([V,Type],Unbound,A3,B3),
    Form2 =.. [P,A1,A2,B3], !.

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

% violateConditionB(Form1,Form2):-
%     subterm(Pred,Form2),
%     Pred =.. [_,X,X,_],
%     \+ subterm(Pred,Form1), !.

% violateConditionB(Form1,Form2):-
%     subterm(Pred,Form2),
%     Pred =.. [_,X,X,_,_],
%     \+ subterm(Pred,Form1), !.

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

findProofTerm(GCxt,Gamma,Term,Type):-
    applyElim(GCxt,Gamma,Redex0,Type),
    betaConvertPi(Redex0,Redex),
    betaConvert(Redex,Term).

applyElim(GCxt,Gamma,Term,A):-
    \+ member([_,and(_,_)],Gamma),
    \+ member([_,exists(_,_,_)],Gamma), !,
    applyIntro(GCxt,Gamma,Term,A).

applyElim(GCxt,[[T,and(A,B)]|Gamma],Term,C):-
    applyElim(GCxt,[[pi1(T),A],[pi2(T),B]|Gamma],Term,C).

applyElim(GCxt,[[T,exists(Y,Type,A)]|Gamma],Term,C):-
    substitute(pi1(T),Y,A,B),
    applyElim(GCxt,[[pi1(T),Type],[pi2(T),B]|Gamma],Term,C).

applyElim(GCxt,[[T,Body]|Gamma],Term,C):-
    \+ Body =.. [and|_],
    \+ Body =.. [exists|_],
    append(Gamma,[[T,Body]],Permuted),
    applyElim(GCxt,Permuted,Term,C).

applyIntro(_,Gamma,Term,A):-
    member([Term,A],Gamma).

applyIntro(GCxt,Gamma,[T,U],and(A,B)):-
    applyIntro(GCxt,Gamma,T,A),
    applyIntro(GCxt,Gamma,U,B).

applyIntro(GCxt,Gamma,[T,U],exists(X,Type,A)):-
    applyElim(GCxt,Gamma,T,Type),
    substitute(T,X,A,B),
    applyIntro(GCxt,Gamma,U,B).

applyIntro(GCxt,_,Term,A):-
    member([Term,A],GCxt).

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

freeVar(type,[]).
freeVar(entity,[]).
freeVar(event,[]).
freeVar(state,[]).

freeVar(A,[A]):-
    atom(A), !.

freeVar(_,[]).

boundVar(exists(X,Type,A),BV):-
    boundVar(Type,BV1),
    boundVar(A,BV2),
    append(BV1,BV2,BV3),
    append([X],BV3,BV), !.

boundVar(exists(X,A),BV):-
    boundVar(A,BV1),
    append([X],BV1,BV), !.

boundVar(forall(X,Type,A),BV):-
    boundVar(Type,BV1),
    boundVar(A,BV2),
    append(BV1,BV2,BV3),
    append([X],BV3,BV), !.
  
boundVar(forall(X,A),BV):-
    boundVar(A,BV1),
    append([X],BV1,BV), !.

boundVar(lam(X,Type,A),BV):-
    boundVar(Type,BV1),
    boundVar(A,BV2),
    append(BV1,BV2,BV3),
    append([X],BV3,BV), !.

boundVar(lam(X,A),BV):-
    boundVar(A,BV1),
    append([X],BV1,BV), !.

boundVar(Form,BV):-
    Form =.. [_,Arg],
    boundVar(Arg,BV), !.

boundVar(Form,BV):-
    Form =.. [_,Arg1,Arg2],
    boundVar(Arg1,BV1),
    boundVar(Arg2,BV2),
    append(BV1,BV2,BV), !.

boundVar(Form,BV):-
    Form =.. [_,Arg1,Arg2,Arg3],
    boundVar(Arg1,BV1),
    boundVar(Arg2,BV2),
    boundVar(Arg3,BV3),
    append(BV1,BV2,X),
    append(X,BV3,BV), !.

boundVar(Form,BV):-
    Form =.. [_,Arg1,Arg2,Arg3,Arg4],
    boundVar(Arg1,BV1),
    boundVar(Arg2,BV2),
    boundVar(Arg3,BV3),
    boundVar(Arg4,BV4),
    append(BV1,BV2,X),
    append(BV3,BV4,Y),
    append(X,Y,BV), !.

boundVar(type,[]).
boundVar(entity,[]).
boundVar(event,[]).
boundVar(state,[]).

boundVar(A,[]):-
    atom(A), !.

boundVar(_,[]).

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

% addType(lam(X,A),lam(X,event,Form)):-
%     X == v,
%     addType(A,Form), !.

% addType(lam(X,A),lam(X,entity,Form)):-
%     addType(A,Form), !.
       
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


%%%%% binary version %%%%%

rewriteVar(Form1,Form2):-
    rewriteVar([],Form1,Form2,_).

rewriteVar(InStack,exists(X,Type,A),exists(V,Type,Form),OutStack):-
    member(X,InStack),
    counter(N),
    atom_concat(x,N,V),
    append(InStack,[V],NewStack),
    substitute(V,X,A,B),
    rewriteVar(NewStack,B,Form,OutStack), !.

rewriteVar(InStack,exists(X,Type,A),exists(X,Type,Form),OutStack):-
    append(InStack,[X],NewStack),
    rewriteVar(NewStack,A,Form,OutStack), !.

rewriteVar(InStack,forall(X,Type,A),forall(V,Type,Form),OutStack):-
    member(X,InStack),
    counter(N),
    atom_concat(x,N,V),
    append(InStack,[V],NewStack),
    substitute(V,X,A,B),
    rewriteVar(NewStack,B,Form,OutStack), !.

rewriteVar(InStack,forall(X,Type,A),forall(X,Type,Form),OutStack):-
    append(InStack,[X],NewStack),
    rewriteVar(NewStack,A,Form,OutStack), !.

rewriteVar(InStack,lam(X,A),lam(V,Form),OutStack):-
    member(X,InStack),
    counter(N),
    atom_concat(x,N,V),
    append(InStack,[X],NewStack),
    substitute(V,X,A,B),
    rewriteVar(NewStack,B,Form,OutStack), !.

rewriteVar(InStack,lam(X,A),lam(X,B),OutStack):-
    append(InStack,[X],NewStack),
    rewriteVar(NewStack,A,B,OutStack), !.

rewriteVar(InStack,lam(X,Type,A),lam(V,Type,Form),OutStack):-
    member(X,InStack),
    counter(N),
    atom_concat(x,N,V),
    append(InStack,[X],NewStack),
    substitute(V,X,A,B),
    rewriteVar(NewStack,B,Form,OutStack), !.

rewriteVar(InStack,lam(X,Type,A),lam(X,Type,B),OutStack):-
    append(InStack,[X],NewStack),
    rewriteVar(NewStack,A,B,OutStack), !.

rewriteVar(InStack,Form0,Form,OutStack):-
    Form0 =.. [F,A0],
    rewriteVar(InStack,A0,A,OutStack),
    Form =.. [F,A], !.

rewriteVar(InStack,Form0,Form,OutStack):-
    Form0 =.. [F,A0,B0],
    rewriteVar(InStack,A0,A,MidStack),
    rewriteVar(MidStack,B0,B,OutStack),
    Form =.. [F,A,B], !.

rewriteVar(InStack,Form0,Form,OutStack):-
    Form0 =.. [F,A0,B0,C0],
    rewriteVar(InStack,A0,A,MidStack1),
    rewriteVar(MidStack1,B0,B,MidStack2),
    rewriteVar(MidStack2,C0,C,OutStack),
    Form =.. [F,A,B,C], !.

rewriteVar(InStack,Form0,Form,OutStack):-
    Form0 =.. [F,A0,B0,C0,D0],
    rewriteVar(InStack,A0,A,MidStack1),
    rewriteVar(MidStack1,B0,B,MidStack2),
    rewriteVar(MidStack2,C0,C,MidStack3),
    rewriteVar(MidStack3,D0,D,OutStack),
    Form =.. [F,A,B,C,D], !.

rewriteVar(X,A,A,X):-
    atomic(A).


%%% Eliminating True %%%

elimTrue(forall(_,A,B),C):-
    elimTrue(A,true),
    elimTrue(B,C), !.

elimTrue(exists(_,A,B),C):-
    elimTrue(A,true),
    elimTrue(B,C), !.

elimTrue(exists(_,A,B),C):-
    elimTrue(B,true),
    elimTrue(A,C), !.

elimTrue(and(A,B),C):-
    elimTrue(A,true),
    elimTrue(B,C), !.

elimTrue(and(A,B),C):-
    elimTrue(A,C),
    elimTrue(B,true), !.

elimTrue(or(A,_),true):-
    elimTrue(A,true), !.

elimTrue(or(_,A),true):-
    elimTrue(A,true), !.

elimTrue(imp(A,B),C):-
    elimTrue(A,true),
    elimTrue(B,C), !.

elimTrue(A,B):-
    A =.. [F,X],
    elimTrue(X,Y),
    B =.. [F,Y], !.

elimTrue(A,B):-
    A =.. [F,X1,X2],
    elimTrue(X1,Y1),
    elimTrue(X2,Y2),
    B =.. [F,Y1,Y2], !.

elimTrue(A,B):-
    A =.. [F,X1,X2,X3],
    elimTrue(X1,Y1),
    elimTrue(X2,Y2),
    elimTrue(X3,Y3),
    B =.. [F,Y1,Y2,Y3], !.

elimTrue(A,B):-
    A =.. [F,X1,X2,X3,X4],
    elimTrue(X1,Y1),
    elimTrue(X2,Y2),
    elimTrue(X3,Y3),
    elimTrue(X4,Y4),
    B =.. [F,Y1,Y2,Y3,Y4], !.

elimTrue(A,A).


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
