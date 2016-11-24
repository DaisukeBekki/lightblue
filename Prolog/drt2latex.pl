:- use_module(dts2drt,[dts2drt/2]).

:- dynamic current_counter/1.

current_counter(0).
counter(M):-
    retract(current_counter(N)),
    M is N+1,
    asserta(current_counter(M)),!.

main :-
    current_prolog_flag(os_argv, AllArgs),
    append(_, [-- |[Args1,Args2,Args3,Args4]], AllArgs),
    term_string(X1,Args1),
    term_string(X2,Args2),
    term_string(X3,Args3),
    term_string(X4,Args4),
    dts2drt(X1,Y1),
    dts2drt(X3,Y3),
    drt2file(Y1,X2,Y3,X4).

% main :-
%     current_prolog_flag(os_argv, AllArgs),
%     append(_, [-- |[Args]], AllArgs),
%     term_string(X,Args),
%     drt2file(X).

drs2latexlist([],Stream) :- nl(Stream).
drs2latexlist([X|List],Stream) :-
    counter(Num),
    write(Stream, '\\noindent'),
    write(Stream, '\\textsc{SR}'),
    write(Stream, Num),
    write(Stream, ':'),
    nl(Stream),
    nl(Stream),
    write(Stream, '\\medskip'),
    write(Stream,'$'),
    dts2drt(X,Y),
    drs2latex(Y,Stream),
    write(Stream,'$'),
    nl(Stream),
    nl(Stream),
    write(Stream, '\\medskip'),
    drs2latexlist(List,Stream).

drt2file(F1,F2,F3,F4):-
    open('drs.tex',write,Stream,[encoding(utf8)]),
    write(Stream,'\\documentclass{jsarticle}'),
    nl(Stream),
    write(Stream,'\\pagestyle{empty}'),
    nl(Stream),
    write(Stream,'%\\usepackage{lscape}'),
    nl(Stream),
    write(Stream,'\\usepackage{drs}'),
    nl(Stream),
    write(Stream,'\\renewcommand\\drsvarfont{\\sf}'),
    nl(Stream),
    write(Stream,'\\begin{document}'),
    nl(Stream),
    write(Stream,'%\\begin{landscape}'),
    nl(Stream),
    write(Stream,'\\tiny'),
    % write(Stream,'$'), % added line
    % drs2latex(F0,Stream), % added line
    % write(Stream,'$'), % added line
    nl(Stream),
    nl(Stream),
    write(Stream,'\\noindent\\textsc{Input:}'),
    nl(Stream),
    nl(Stream),
    write(Stream,'\\medskip'),
    numbervars(F1,0,_),
    write(Stream,'$'),
    drs2latex(F1,Stream),
    write(Stream,'$'),
    nl(Stream),
    nl(Stream),
    write(Stream,'\\bigskip'),
    write(Stream,'\\noindent\\textsc{Presupposition resolved:}'),
    nl(Stream),
    nl(Stream),
    write(Stream,'\\medskip'),
    % numbervars(F2,0,_),
    nl(Stream),
    drs2latexlist(F2,Stream),
    nl(Stream),
    write(Stream,'\\bigskip'),
    write(Stream,'\\noindent\\textsc{Normalized '),
    write(Stream,'(SR'),
    write(Stream,F4),
    % drs2latex(F4,Stream),
    write(Stream,')}:'),
    nl(Stream),
    nl(Stream),
    write(Stream,'\\medskip'),
    numbervars(F3,0,_),
    write(Stream,'$'),
    drs2latex(F3,Stream),
    write(Stream,'$'),
    nl(Stream),
    write(Stream,'%\\end{landscape}'),
    nl(Stream),
    write(Stream,'\\end{document}'),
    nl(Stream),
    close(Stream), !.

drt2file(F1):-
    open('drs.tex',write,Stream,[encoding(utf8)]),
    write(Stream,'\\documentclass{jsarticle}'),
    nl(Stream),
    write(Stream,'\\pagestyle{empty}'),
    nl(Stream),
    write(Stream,'%\\usepackage{lscape}'),
    nl(Stream),
    write(Stream,'\\usepackage{drs}'),
    nl(Stream),
    write(Stream,'\\renewcommand\\drsvarfont{\\sf}'),
    nl(Stream),
    write(Stream,'\\begin{document}'),
    nl(Stream),
    write(Stream,'%\\begin{landscape}'),
    nl(Stream),
    write(Stream,'\\scriptsize'),
    % nl(Stream),
    % nl(Stream),
    write(Stream,'\\noindent\\textsc{Output DRS:}'),
    nl(Stream),
    nl(Stream),
    write(Stream,'\\medskip'),
    write(Stream,'$'),
    drs2latex(F1,Stream),
    write(Stream,'$'),
    nl(Stream),
    write(Stream,'%\\end{landscape}'),
    nl(Stream),
    write(Stream,'\\end{document}'),
    nl(Stream),
    close(Stream), !.

% \drs{x y}{Jones(x) \\ Ulysses(y) \\ x owns y}
% \drs{x}{Jones(x) \\
%      \ifdrs{y}{donkey(y)\\x owns y}
%            {z w}{z = x\\ w = y\\ z feeds w}}

writeDR([],Stream):-
    write(Stream, ' ').
     % nl(Stream).
writeDR([X|List],Stream):-
    write(Stream,X),
    % write_term(Stream,X,[numbervars(true)]),
    write(Stream,' '),
    writeDR(List,Stream).

writeCon([],Stream):- nl(Stream).
writeCon([X|List],Stream):-
    drs2latex(X,Stream),
    write(Stream,' \\\\ '),
    writeCon(List,Stream).

drs2latex(F):-
    drs2latex(F,user).

drs2latex(drs(DR,Con),Stream):- !,
    write(Stream,'\\drs{'),
    writeDR(DR,Stream),
    % write_term(Stream,X,[numbervars(true)]),
    write(Stream,'}{'),
    writeCon(Con,Stream),
    write(Stream,'}').

drs2latex(F,Stream):-
    atomic(F),
    write(Stream,F).
    % write_term(Stream,F,[numbervars(true)]).

drs2latex(imp(drs(DR1,Con1),drs(DR2,Con2)),Stream):-
    write(Stream,'\\ifdrs{'),
    writeDR(DR1,Stream),
    write(Stream,'}{'),
    writeCon(Con1,Stream),
    write(Stream,'}{'),
    writeDR(DR2,Stream),
    write(Stream,'}{'),
    writeCon(Con2,Stream),
    write(Stream,'}').

drs2latex(lam(X,F),Stream):- !,
    write(Stream,'$\\lambda$'),
    write_term(Stream,X,[numbervars(true)]),
    write(Stream,'.'),
    drs2latex(F,Stream).

drs2latex(lam(X,_,F),Stream):- !,
    write(Stream,'$\\lambda$'),
    write_term(Stream,X,[numbervars(true)]),
    write(Stream,'.'),
    drs2latex(F,Stream).

drs2latex(eq(Form1,Form2),Stream):-
    drs2latex(Form1,Stream),
    write(Stream,' = '),
    drs2latex(Form2,Stream).

drs2latex(lt(F1,F2),Stream):- !,
    drs2latex(F1,Stream),
    write(Stream,' $<$ '),
    drs2latex(F2,Stream).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
drs2latex(F,Stream):-
    F =.. [Symbol,Form],
    write(Stream,Symbol),
    write(Stream,'('),
    drs2latex(Form,Stream),
    write(Stream,')').

drs2latex(F,Stream):-
    F =.. [Symbol,Arg1,Arg2],
    write(Stream,Symbol),
    write(Stream,'('),
    drs2latex(Arg1,Stream),
    write(Stream,','),
    drs2latex(Arg2,Stream),
    write(Stream,')').

drs2latex(F,Stream):-
    F =.. [Symbol,Arg1,Arg2,Arg3],
    write(Stream,Symbol),
    write(Stream,'('),
    drs2latex(Arg1,Stream),
    write(Stream,','),
    drs2latex(Arg2,Stream),
    write(Stream,','),
    drs2latex(Arg3,Stream),
    write(Stream,')').

drs2latex(F,Stream):-
    F =.. [Symbol,Arg1,Arg2,Arg3,Arg4],
    write(Stream,'('),
    write(Stream,Symbol),
    drs2latex(Arg1,Stream),
    write(Stream,','),
    drs2latex(Arg2,Stream),
    write(Stream,','),
    drs2latex(Arg3,Stream),
    write(Stream,','),
    drs2latex(Arg4,Stream),
    write(Stream,')').
%%%%%%%%%%%%%%%%%%%%%%%%%

drs2latex(F,Stream):-
    F =.. [app,Symbol,Form],
    write(Stream,Symbol),
    % write_term(Stream,Symbol,[numbervars(true)]),
    write(Stream,'('),
    drs2latex(Form,Stream),
    write(Stream,')').

drs2latex(F,Stream):-
    F =.. [app,Symbol,Arg1,Arg2],
    write(Stream,Symbol),
    write(Stream,'('),
    drs2latex(Arg1,Stream),
    write(Stream,','),
    drs2latex(Arg2,Stream),
    write(Stream,')').

drs2latex(F,Stream):-
    F =.. [app,Symbol,Arg1,Arg2,Arg3],
    write(Stream,Symbol),
    write(Stream,'('),
    drs2latex(Arg1,Stream),
    write(Stream,','),
    drs2latex(Arg2,Stream),
    write(Stream,','),
    drs2latex(Arg3,Stream),
    write(Stream,')').

drs2latex(F,Stream):-
    F =.. [app,Symbol,Arg1,Arg2,Arg3,Arg4],
    write(Stream,'('),
    write(Stream,Symbol),
    drs2latex(Arg1,Stream),
    write(Stream,','),
    drs2latex(Arg2,Stream),
    write(Stream,','),
    drs2latex(Arg3,Stream),
    write(Stream,','),
    drs2latex(Arg4,Stream),
    write(Stream,')').
