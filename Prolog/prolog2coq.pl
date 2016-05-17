main :-
    current_prolog_flag(os_argv, AllArgs),
    append(_, [-- |[Args]], AllArgs),
    term_string(X,Args),
    prolog2file(X).

prolog2file(F):-
    open('interpretation.txt',write,Stream,[encoding(utf8)]),
    numbervars(F,0,_),
    prolog2coq(F,Stream), !,
    close(Stream).

prolog2coq(exists(X,Type,F),Stream):- 
    write(Stream,'(exists '),
    write_term(Stream,X,[numbervars(true)]),
    write(Stream,' : '),
    prolog2coq(Type,Stream),
    write(Stream,','),
    prolog2coq(F,Stream),
    write(Stream,')').

prolog2coq(forall(X,Type,F),Stream):- 
    write(Stream,'(forall '),
    write_term(Stream,X,[numbervars(true)]),
    write(Stream,' : '),
    prolog2coq(Type,Stream),
    write(Stream,','),
    prolog2coq(F,Stream),
    write(Stream,')').

prolog2coq(exists(X,F),Stream):- 
    X == v,
    write(Stream,'(exists '),
    write_term(Stream,X,[numbervars(true)]),
    write(Stream,' : Event,'),
    prolog2coq(F,Stream),
    write(Stream,')').

prolog2coq(exists(X,F),Stream):- 
    write(Stream,'(exists '),
    write_term(Stream,X,[numbervars(true)]),
    write(Stream,' : Entity,'),
    prolog2coq(F,Stream),
    write(Stream,')').

prolog2coq(forall(X,F),Stream):- 
    X == v,
    write(Stream,'(forall '),
    write_term(Stream,X,[numbervars(true)]),
    write(Stream,' : Event,'),
    prolog2coq(F,Stream),
    write(Stream,')').

prolog2coq(forall(X,F),Stream):- 
    write(Stream,'(forall '),
    write_term(Stream,X,[numbervars(true)]),
    write(Stream,' : Entity,'),
    prolog2coq(F,Stream),
    write(Stream,')').

prolog2coq(lam(X,F),Stream):- !,
    write(Stream,'(fun '),
    write_term(Stream,X,[numbervars(true)]),
    write(Stream,' => '),
    prolog2coq(F,Stream),
    write(Stream,')').

prolog2coq(lam(X,Type,F),Stream):- !,
    write(Stream,'(fun '),
    write_term(Stream,X,[numbervars(true)]),
    write(Stream,' : '),
    prolog2coq(Type,Stream),
    write(Stream,','),
    prolog2coq(F,Stream),
    write(Stream,')').

prolog2coq(and(F1,F2),Stream):- !,
    write(Stream,'(and '),
    prolog2coq(F1,Stream),
    write(Stream,' '),
    prolog2coq(F2,Stream),
    write(Stream,')').

prolog2coq(imp(F1,F2),Stream):- !,
    write(Stream,'('),
    prolog2coq(F1,Stream),
    write(Stream,' -> '),
    prolog2coq(F2,Stream),
    write(Stream,')').

prolog2coq(or(F1,F2),Stream):- !,
    write(Stream,'(or '),
    prolog2coq(F1,Stream),
    write(Stream,' '),
    prolog2coq(F2,Stream),
    write(Stream,')').

prolog2coq(eq(A,B),Stream):-
    write(Stream,'('),
    prolog2coq(A,Stream),
    write(Stream,' = '),
    prolog2coq(B,Stream),
    write(Stream,')').

prolog2coq(not(F),Stream):-
    write(Stream,'(not '),
    prolog2coq(F,Stream),
    write(Stream,')').

prolog2coq(entity,Stream):-
    write(Stream,'Entity').

prolog2coq(event,Stream):-
    write(Stream,'Event').

prolog2coq(state,Stream):-
    write(Stream,'State').

prolog2coq([A,B],Stream):-
    write(Stream,'(pair '),
    prolog2coq(A,Stream),
    write(Stream,','),
    prolog2coq(B,Stream),
    write(Stream,')').

prolog2coq(F,Stream):-
    F =.. [Symbol,Form],
    Symbol == pi1,
    write(Stream,'(projT1 '),
    prolog2coq(Form,Stream),
    write(Stream,')').

prolog2coq(F,Stream):-
    F =.. [Symbol,Form],
    Symbol == pi2,
    write(Stream,'(projT2 '),
    prolog2coq(Form,Stream),
    write(Stream,')').

prolog2coq(F,Stream):-
    F =.. [Symbol,Form],
    member(Symbol,[subj,top,acc,acci,acce,dat,attr,deg,argof]),
    first_char_uppercase(Symbol,SymbolUp),
    write(Stream,'('),
    write_term(Stream,SymbolUp,[numbervars(true)]),
    write(Stream,' '),
    prolog2coq(Form,Stream),
    write(Stream,')').

prolog2coq(F,Stream):-
    F =.. [@,N,T],
    write(Stream,'@'),
    write_term(Stream,N,[numbervars(true)]),
    write(Stream,' : '),
    prolog2coq(T,Stream).

prolog2coq(F,Stream):-
    F =.. [Symbol,Form],
    write(Stream,'(_'),
    write_term(Stream,Symbol,[numbervars(true)]),
    write(Stream,' '),
    prolog2coq(Form,Stream),
    write(Stream,')').

prolog2coq(F,Stream):-
    F =.. [Symbol,Arg1,Arg2],
    write(Stream,'(_'),
    write_term(Stream,Symbol,[numbervars(true)]),
    write(Stream,' '),
    prolog2coq(Arg1,Stream),
    write(Stream,' '),
    prolog2coq(Arg2,Stream),
    write(Stream,')').

prolog2coq(F,Stream):-
    F =.. [Symbol,Arg1,Arg2,Arg3],
    write(Stream,'(_'),
    write_term(Stream,Symbol,[numbervars(true)]),
    write(Stream,' '),
    prolog2coq(Arg1,Stream),
    write(Stream,' '),
    prolog2coq(Arg2,Stream),
    write(Stream,' '),
    prolog2coq(Arg3,Stream),
    write(Stream,')').

prolog2coq(F,Stream):-
    atom(F),
    atom_length(F,L),
    L is 1,
    sub_atom(F,0,1,_,X),
    member(X,[t,u,v,w,x,y,z]),
    write_term(Stream,F,[numbervars(true)]).

prolog2coq(F,Stream):-
    atom(F),
    sub_atom(F,0,1,_,X),
    sub_atom(F,1,1,_,Y),
    member(X,[t,u,v,w,x,y,z]),
    atom_number(Y,N),
    number(N),
    write_term(Stream,F,[numbervars(true)]).

prolog2coq(F,Stream):-
    atomic(F),
    write(Stream,'_'),
    write_term(Stream,F,[numbervars(true)]).

first_char_uppercase(WordLC, WordUC) :-
    atom_chars(WordLC, [FirstChLow|LWordLC]),
    atom_chars(FirstLow, [FirstChLow]),
    string_upper(FirstLow, FirstUpp),
    atom_chars(FirstUpp, [FirstChUpp]),
    atom_chars(WordUC, [FirstChUpp|LWordLC]).
