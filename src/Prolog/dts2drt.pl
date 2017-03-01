:- module(dts2drt,[dts2drt/2]).

:- use_module(comsemPredicates,[substitute/4]).

% drs([X1...Xn],[C1...Cm])

% exists(x,entity,and(app(run,x),app(smile(x))))
% ==> drs([x],[app(run,x),app(smile,x)])
% exists(x,entity,exists(v,event,and(app(run,v),eq(subj(v),x))))
% ==> drs([x,v],[app(run,v),eq(subj(v),x)])
% exists(x,entity,exists(v,event,imp(app(run,v),eq(subj(v),x))))
% ==>
% drs([x1,x2],[imp(drs([],[app(run,x2)]),drs([],[app(run,x2),eq(subj(x2),x1)]))]).


% dts2drt(exists(x,entity,and(app(run,x),app(smile,x))),X).
% dts2drt(exists(x,entity,exists(v,event,and(app(run,v),eq(app(subj,v),x)))),X).
% dts2drt(exists(x,entity,exists(v,event,imp(app(run,v),eq(app(subj,v),x)))),X).

:- dynamic current_counter/1.

current_counter(0).
counter(M):-
    retract(current_counter(N)),
    M is N+1,
    asserta(current_counter(M)),!.

dts2drt(FormDTS,DRS):-
    dts2drt([],drs([],[]),FormDTS,_,DRS).

% dts2drt(exists(x,entity,and(app(run,x),app(smile,x))),drs([x],[app(run,x),app(smile,x)])).
% dts2drt(exists(x,entity,exists(v,event,and(app(run,v),eq(subj(v),x)))),drs([x,v],[app(run,v),eq(subj(v),x)])).

dts2drt(StackIn,drs(DR1,Con),exists(X,entity,A),StackOut,DRSOut):-
    counter(N),
    atom_concat(x,N,V),
    append(DR1,[V],DR2),
    append(StackIn,[V],StackInt),
    substitute(V,X,A,B),
    dts2drt(StackInt,drs(DR2,Con),B,StackOut,DRSOut), !.

dts2drt(StackIn,drs(DR1,Con),exists(X,event,A),StackOut,DRSOut):-
    counter(N),
    atom_concat(e,N,V),
    append(DR1,[V],DR2),
    append(StackIn,[V],StackInt),
    substitute(V,X,A,B),
    dts2drt(StackInt,drs(DR2,Con),B,StackOut,DRSOut), !.

dts2drt(StackIn,drs(DR1,Con),exists(X,A),StackOut,DRSOut):-
    X == v,
    counter(N),
    atom_concat(e,N,V),
    append(DR1,[V],DR2),
    append(StackIn,[V],StackInt),
    substitute(V,X,A,B),
    dts2drt(StackInt,drs(DR2,Con),B,StackOut,DRSOut), !.

dts2drt(StackIn,drs(DR1,Con),exists(X,A),StackOut,DRSOut):-
    counter(N),
    atom_concat(x,N,V),
    append(DR1,[V],DR2),
    append(StackIn,[V],StackInt),
    substitute(V,X,A,B),
    dts2drt(StackInt,drs(DR2,Con),B,StackOut,DRSOut), !.

dts2drt(StackIn,drs(DR,Con),forall(X,A,B),StackOut,drs(DR,ConOut)):-
    % counter(N),
    % atom_concat(x,N,V),
    % append(DR1,[V],DR2),
    % append(StackIn,[V],StackInt),
    % substitute(V,X,A,B),
    dts2drt(StackIn,drs([X],[]),A,StackInt,DRSInt),
    dts2drt(StackInt,drs([],[]),B,StackOut,DRSOut),
    append(Con,[imp(DRSInt,DRSOut)],ConOut), !.

dts2drt(StackIn,drs(DR1,Con),and(A,B),StackOut,DRSOut):-
    dts2drt(StackIn,drs(DR1,Con),A,StackInt,DRSInt),
    dts2drt(StackInt,DRSInt,B,StackOut,DRSOut), !.

dts2drt(StackIn,drs(DR,Con),imp(A,B),StackOut,drs(DR,ConOut)):-
    dts2drt(StackIn,drs([],[]),A,StackInt,DRSInt),
    dts2drt(StackInt,drs([],[]),B,StackOut,DRSOut),
    append(Con,[imp(DRSInt,DRSOut)],ConOut), !.

% OK?
dts2drt(StackIn,drs(DR,Con),or(A,B),StackOut,drs(DR,ConOut)):-
    dts2drt(StackIn,drs([],[]),A,StackInt,DRSInt),
    dts2drt(StackInt,drs([],[]),B,StackOut,DRSOut),
    append(Con,[or(DRSInt,DRSOut)],ConOut), !.

dts2drt(_,drs(DR,Con1),eq(A,B),_,drs(DR,Con2)):-
   append(Con1,[eq(A,B)],Con2), !.

dts2drt(StackIn,drs(DR,Con1),not(A),StackOut,drs(DR,Con2)):-
   dts2drt(StackIn,drs([],[]),A,StackOut,DRSOut),
   append(Con1,[not(DRSOut)],Con2), !.

dts2drt(_,drs(DR,Con1),Form,_,drs(DR,Con2)):-
   append(Con1,[Form],Con2), !.

% dts2drt(StackIn,drs(DR,Con1),Form,StackOut,drs(DR,Con2)):-
%    Form =.. [_,A1],
%    dts2drt(StackIn,drs([],[]),A1,StackOut,DRSOut),
%    Form1 = .. [F,
%    append(Con1,[Form],Con2), !.

% dts2drt(_,drs(DR,Con1),app(F,A1,A2),_,drs(DR,Con2)):-
%    append(Con1,[app(F,A1,A2)],Con2), !.

% dts2drt(_,drs(DR,Con1),app(F,A1,A2,A3),_,drs(DR,Con2)):-
%    append(Con1,[app(F,A1,A2,A3)],Con2), !.

% dts2drt(_,drs(DR,Con1),app(F,A1,A2,A3,A4),_,drs(DR,Con2)):-
%    append(Con1,[app(F,A1,A2,A3,A4)],Con2), !.

dts2drt(_,drs(DR,Con1),app(F,A1),_,drs(DR,Con2)):-
   append(Con1,[app(F,A1)],Con2).

dts2drt(_,drs(DR,Con1),app(F,A1,A2),_,drs(DR,Con2)):-
   append(Con1,[app(F,A1,A2)],Con2).

dts2drt(_,drs(DR,Con1),app(F,A1,A2,A3),_,drs(DR,Con2)):-
   append(Con1,[app(F,A1,A2,A3)],Con2).

dts2drt(_,drs(DR,Con1),app(F,A1,A2,A3,A4),_,drs(DR,Con2)):-
   append(Con1,[app(F,A1,A2,A3,A4)],Con2).



% dts2drt(FormDTS,DRS):-
%     dts2drt(drs([],[]),FormDTS,DRS).

% % dts2drt(exists(x,entity,and(app(run,x),app(smile,x))),drs([x],[app(run,x),app(smile,x)])).
% % dts2drt(exists(x,entity,exists(v,event,and(app(run,v),eq(subj(v),x)))),drs([x,v],[app(run,v),eq(subj(v),x)])).

% dts2drt(drs(DR1,Con),exists(_,entity,A),DRSOut):-
%     % member(X,DR1),
%     counter(N),
%     atom_concat(x,N,V),
%     append(DR1,[V],DR2),
%     % append(DRsetIn,[V],DRsetOut),
%     dts2drt(drs(DR2,Con),A,DRSOut), !.

% dts2drt(drs(DR1,Con),exists(_,event,A),DRSOut):-
%     % member(X,DRset),
%     counter(N),
%     atom_concat(e,N,V),
%     append(DR1,[V],DR2),
%     % append(DRsetIn,[V],DRsetOut),
%     dts2drt(drs(DR2,Con),A,DRSOut), !.

% % dts2drt(drs(DR1,Con),exists(X,_,A),DRSOut):-
% %     % \+ member(X,DR1),
% %     append(DR1,[X],DR2),
% %     dts2drt(drs(DR2,Con),A,DRSOut), !.

% dts2drt(drs(DR1,Con),and(A,B),DRSOut):-
%     dts2drt(drs(DR1,Con),A,DRSInt),
%     dts2drt(DRSInt,B,DRSOut).

% dts2drt(drs(DR,Con),imp(A,B),drs(DR,ConOut)):-
%     dts2drt(drs(DR,Con),A,DRSInt),
%     dts2drt(DRSInt,B,DRSOut),
%     append(Con,[imp(DRSInt,DRSOut)],ConOut).

% dts2drt(drs(DR,Con1),app(F,Arg),drs(DR,Con2)):-
%    append(Con1,[app(F,Arg)],Con2).

% dts2drt(drs(DR,Con1),eq(A,B),drs(DR,Con2)):-
%    append(Con1,[eq(A,B)],Con2).


