% :- set_stream(user_input, encoding(utf8)).
% :- set_stream(user_output, encoding(utf8)).

main :-
    current_prolog_flag(os_argv, AllArgs),
    append(_, [-- |[Args1,Args2,Args3]], AllArgs),
    term_string(X1,Args1),
    term_string(X2,Args2),
    term_string(X3,Args3),
    fol2file(X1,X2,X3).

fol2file(F1,F2,F3):-
    open('test.tex',write,Stream,[encoding(utf8)]),
    write(Stream,'\\documentclass{jsarticle}'),
    nl(Stream),
    write(Stream,'\\pagestyle{empty}'),
    nl(Stream),
    write(Stream,'%\\usepackage{lscape}'),
    nl(Stream),
    write(Stream,'\\newcommand{\\dConj}[2]{\\left[\\kern-0.3em\\begin{array}{l}\\vspace{0.1mm}#1\\\\#2\\end{array}\\kern-0.3em\\right]}'),
    nl(Stream),
    write(Stream,'\\newcommand{\\dPi}[2]{\\left(\\kern-0.3em\\begin{array}{l}#1\\end{array}\\kern-0.3em\\right) \\to #2}'),
    nl(Stream),
    write(Stream,'\\newcommand{\\dImp}[2]{#1 \\to #2}'),
    write(Stream,'\\newcommand{\\Entity}{\\ensuremath{\\mathbf{entity}}}'),
    nl(Stream),
    write(Stream,'\\newcommand{\\Event}{\\ensuremath{\\mathbf{event}}}'),
    nl(Stream),
    write(Stream,'\\begin{document}'),
    nl(Stream),
    write(Stream,'%\\begin{landscape}'),
    nl(Stream),
    write(Stream,'\\scriptsize'),
    nl(Stream),
    nl(Stream),
    write(Stream,'\\noindent\\textsc{Input:}'),
    nl(Stream),
    nl(Stream),
    write(Stream,'\\medskip'),
    numbervars(F1,0,_),
    write(Stream,'$'),
    fol2latex(F1,Stream),
    write(Stream,'$'),
    nl(Stream),
    nl(Stream),
    write(Stream,'\\bigskip'),
    write(Stream,'\\noindent\\textsc{Presupposition resolved:}'),
    nl(Stream),
    nl(Stream),
    write(Stream,'\\medskip'),
    numbervars(F2,0,_),
    write(Stream,'$'),
    fol2latex(F2,Stream),
    write(Stream,'$'),
    nl(Stream),
    nl(Stream),
    write(Stream,'\\bigskip'),
    write(Stream,'\\noindent\\textsc{Normalized:}'),
    nl(Stream),
    nl(Stream),
    write(Stream,'\\medskip'),
    numbervars(F3,0,_),
    write(Stream,'$'),
    fol2latex(F3,Stream),
    write(Stream,'$'),
    nl(Stream),
    write(Stream,'%\\end{landscape}'),
    nl(Stream),
    write(Stream,'\\end{document}'),
    nl(Stream),
    close(Stream), !.

% fol2file(F):-
%     open('test.tex',append,Stream,[encoding(utf8)]),
%     numbervars(F,0,_),
%     nl(Stream),
%     write(Stream,'$'),
%     fol2latex(F,Stream),
%     write(Stream,'$'),
%     nl(Stream),
%     close(Stream).

fol2latex(F):-
    fol2latex(F,user).

fol2latex(exists(X,Type,F),Stream):- 
    write(Stream,'\\dConj'),
    write(Stream,'{'),
    write_term(Stream,X,[numbervars(true)]),
    write(Stream,':'),
    fol2latex(Type,Stream),
    write(Stream,'}'),
    write(Stream,'{'),
    fol2latex(F,Stream),
    write(Stream,'}').

fol2latex(forall(X,Type,F),Stream):- !,
    write(Stream,'\\dPi'),
    write(Stream,'{'),
    write_term(Stream,X,[numbervars(true)]),
    write(Stream,':'),
    fol2latex(Type,Stream),
    write(Stream,'}'),
    write(Stream,'{'),
    fol2latex(F,Stream),
    write(Stream,'}').

fol2latex(exists(X,F),Stream):- 
    X == v,
    write(Stream,'\\dConj'),
    write(Stream,'{'),
    write_term(Stream,X,[numbervars(true)]),
    write(Stream,': \\Event}'),
    write(Stream,'{'),
    fol2latex(F,Stream),
    write(Stream,'}').

fol2latex(exists(X,F),Stream):- !,
    write(Stream,'\\dConj'),
    write(Stream,'{'),
    write_term(Stream,X,[numbervars(true)]),
    write(Stream,': \\Entity}'),
    write(Stream,'{'),
    fol2latex(F,Stream),
    write(Stream,'}').

fol2latex(forall(X,F),Stream):- !,
    write(Stream,'\\dPi'),
    write(Stream,'{'),
    write_term(Stream,X,[numbervars(true)]),
    write(Stream,': \\Entity}'),
    write(Stream,'{'),
    fol2latex(F,Stream),
    write(Stream,'}').


fol2latex(most(F1,F2),Stream):- !,
    write(Stream,'\\mathbf{most}'),
    write(Stream,'\\left('),
    fol2latex(F1,Stream),
    write(Stream,','),
    fol2latex(F2,Stream),
    write(Stream,'\\right)').

fol2latex(lam(X,F),Stream):- !,
    write(Stream,'\\lambda '),
    % write_term(Stream,X),
    write_term(Stream,X,[numbervars(true)]),
    write(Stream,'.'),
    fol2latex(F,Stream).

fol2latex(lam(X,Type,F),Stream):- !,
    write(Stream,'\\lambda '),
    write_term(Stream,X,[numbervars(true)]),
    write(Stream,':'),
    fol2latex(Type,Stream),
    write(Stream,'.'),
    fol2latex(F,Stream).

fol2latex(and(F1,F2),Stream):- !,
    write(Stream,'\\dConj'),
    write(Stream,'{'),
    fol2latex(F1,Stream),
    write(Stream,'}'),
    write(Stream,'{'),
    fol2latex(F2,Stream),
    write(Stream,'}').

fol2latex(imp(F1,F2),Stream):- !,
    write(Stream,'\\dImp'),
    write(Stream,'{'),
    fol2latex(F1,Stream),
    write(Stream,'}'),
    write(Stream,'{'),
    fol2latex(F2,Stream),
    write(Stream,'}').

fol2latex(or(F1,F2),Stream):- !,
    write(Stream,'\\left('),
    fol2latex(F1,Stream),
    write(Stream,' \\vee '),
    fol2latex(F2,Stream),
    write(Stream,'\\right)').

fol2latex(eq(A,B),Stream):-
    fol2latex(A,Stream),
    write(Stream,' = '),
    fol2latex(B,Stream), !.

fol2latex(not(F),Stream):- !,
    write(Stream,'\\neg'),
    fol2latex(F,Stream).


fol2latex(entity,Stream):-
    write(Stream,'\\mbox{\\textbf{entity}}').

fol2latex(event,Stream):-
    write(Stream,'\\mbox{\\textbf{event}}').

fol2latex(state,Stream):-
    write(Stream,'\\mbox{\\textbf{state}}').

fol2latex([A,B],Stream):-
    write(Stream,'('),
    fol2latex(A,Stream),
    write(Stream,','),
    fol2latex(B,Stream),
    write(Stream,')').

fol2latex(F,Stream):-
    atomic(F),
    write_term(Stream,F,[numbervars(true)]).

fol2latex(F,Stream):-
    F =.. [Symbol,Form],
    Symbol == pi1,
    write(Stream,'\\pi_{1}\\!\\left('),
    fol2latex(Form,Stream),
    write(Stream,'\\right)').

fol2latex(F,Stream):-
    F =.. [Symbol,Form],
    Symbol == pi2,
    write(Stream,'\\pi_{2}\\!\\left('),
    fol2latex(Form,Stream),
    write(Stream,'\\right)').

fol2latex(F,Stream):-
    F =.. [Symbol,Form],
    write(Stream,'\\mbox{\\textbf{'),
    write_term(Stream,Symbol,[numbervars(true)]),
    write(Stream,'}}'),
    write(Stream,'\\left('),
    fol2latex(Form,Stream),
    write(Stream,'\\right)').

fol2latex(F,Stream):-
    F =.. [@,N,T],
    write(Stream,'@'),
    write_term(Stream,N,[numbervars(true)]),
    write(Stream,' : '),
    fol2latex(T,Stream).

fol2latex(F,Stream):-
    F =.. [Symbol,Arg1,Arg2],
    write(Stream,'\\mbox{\\textbf{'),
    write_term(Stream,Symbol,[numbervars(true)]),
    write(Stream,'}}('),
    fol2latex(Arg2,Stream),
    write(Stream,','),
    fol2latex(Arg1,Stream),
    write(Stream,')').

fol2latex(F,Stream):-
    F =.. [Symbol,Arg1,Arg2,Arg3],
    write(Stream,'\\mbox{\\textbf{'),
    write_term(Stream,Symbol,[numbervars(true)]),
    write(Stream,'}}('),
    fol2latex(Arg3,Stream),
    write(Stream,','),
    fol2latex(Arg2,Stream),
    write(Stream,','),
    fol2latex(Arg1,Stream),
    write(Stream,')').

fol2latex(F,Stream):-
    F =.. [Symbol,Arg1,Arg2,Arg3,Arg4],
    write(Stream,'\\mbox{\\textbf{'),
    write_term(Stream,Symbol,[numbervars(true)]),
    write(Stream,'}}('),
    fol2latex(Arg4,Stream),
    write(Stream,','),
    fol2latex(Arg3,Stream),
    write(Stream,','),
    fol2latex(Arg2,Stream),
    write(Stream,','),
    fol2latex(Arg1,Stream),
    write(Stream,')').

