:- use_module(comsemPredicates,[substitute/4]).

main :-
    current_prolog_flag(os_argv, AllArgs),
    append(_, [-- |[Args]], AllArgs),
    term_string(X,Args),
    elimSigma(X,SR),
    write(SR).

nonSigma(F):-
    \+ F =.. [and|_],
    \+ F =.. [exists|_].

subterm(T,T).
subterm(T1,T2):-
    T2 =.. [_|As],
    member(A,As),
    subterm(T1,A).

elimSigma(exists(X,A,B),exists(X,A,C)):-
    nonSigma(A),
    subterm(X,B),
    elimSigma(B,C), !.

elimSigma(exists(X,A,B),and(A,C)):-
    nonSigma(A),
    \+ subterm(X,B),
    elimSigma(B,C), !.

elimSigma(forall(X,A,B),forall(X,A,C)):-
    nonSigma(A),
    subterm(X,B),
    elimSigma(B,C), !.

elimSigma(forall(X,A,B),imp(A,C)):-
    nonSigma(A),
    \+ subterm(X,B),
    elimSigma(B,C), !.

elimSigma(exists(U,exists(T,A,B),C),O):-
    substitute(T,pi1(U),C,C0),
    substitute(U,pi2(U),C0,C1),
    elimSigma(exists(T,A,exists(U,B,C1)),O), !.

elimSigma(forall(U,exists(T,A,B),C),O):-
    substitute(T,pi1(U),C,C0),
    substitute(U,pi2(U),C0,C1),
    elimSigma(forall(T,A,forall(U,B,C1)),O), !.

elimSigma(exists(U,and(A,B),C),O):-
    atom_concat(U,p1,U1),
    atom_concat(U,p2,U2),
    substitute(U1,pi1(U),C,C0),
    substitute(U2,pi2(U),C0,C1),
    elimSigma(exists(U1,A,exists(U2,B,C1)),O), !.

elimSigma(forall(U,and(A,B),C),O):-
    atom_concat(U,p1,U1),
    atom_concat(U,p2,U2),
    substitute(U1,pi1(U),C,C0),
    substitute(U2,pi2(U),C0,C1),
    elimSigma(forall(U1,A,forall(U2,B,C1)),O), !.

% elimSigma(lam(X,A),lam(X,B)):-
%     elimSigma(A,B), !.

% elimSigma(lam(X,Type,A),lam(X,Type,B)):-
%     elimSigma(A,B), !.

% elimSigma(and(A,B),and(F1,F2)):-
%     elimSigma(A,F1),
%     elimSigma(B,F2), !.

% elimSigma(imp(A,B),imp(F1,F2)):-
%     elimSigma(A,F1),
%     elimSigma(B,F2), !.

elimSigma(A,B):-
    A =.. [F,X],
    elimSigma(X,Y),
    B =.. [F,Y], !.

elimSigma(A,B):-
    A =.. [F,X1,X2],
    elimSigma(X1,Y1),
    elimSigma(X2,Y2),
    B =.. [F,Y1,Y2], !.

elimSigma(A,B):-
    A =.. [F,X1,X2,X3],
    elimSigma(X1,Y1),
    elimSigma(X2,Y2),
    elimSigma(X3,Y3),
    B =.. [F,Y1,Y2,Y3], !.

elimSigma(A,B):-
    A =.. [F,X1,X2,X3,X4],
    elimSigma(X1,Y1),
    elimSigma(X2,Y2),
    elimSigma(X3,Y3),
    elimSigma(X4,Y4),
    B =.. [F,Y1,Y2,Y3,Y4], !.

elimSigma(A,A).
