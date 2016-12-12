% :- set_stream(user_input, encoding(utf8)).
% :- set_stream(user_output, encoding(utf8)).

:- dynamic current_counter/1.

current_counter(0).
counter(M):-
    retract(current_counter(N)),
    M is N+1,
    asserta(current_counter(M)),!.

% main :-
%     current_prolog_flag(os_argv, AllArgs),
%     append(_, [-- |[Args0,Args1,Args2,Args3,Args4]], AllArgs), % modified line
%     term_string(Tmp,Args0),
%     readWord(Tmp,X0), % modified line
%     term_string(X1,Args1),
%     term_string(X2,Args2),
%     term_string(X3,Args3),
%     term_string(X4,Args4),
%     fol2file(X0,X1,X2,X3,X4).

main :-
    current_prolog_flag(os_argv, AllArgs),
    append(_, [-- |[Args1,Args2,Args3,Args4]], AllArgs),
    term_string(X1,Args1),
    term_string(X2,Args2),
    term_string(X3,Args3),
    term_string(X4,Args4),
    fol2file(X1,X2,X3,X4).

fol2latexlist([],Stream) :- nl(Stream).
fol2latexlist([X|List],Stream) :-
    counter(Num),
    write(Stream, '\\noindent'),
    write(Stream, '\\textsc{SR}'),
    write(Stream, Num),
    write(Stream, ':'),
    nl(Stream),
    nl(Stream),
    write(Stream, '\\medskip'),
    write(Stream,'$'),
    fol2latex(X,Stream),
    write(Stream,'$'),
    nl(Stream),
    nl(Stream),
    write(Stream, '\\medskip'),
    fol2latexlist(List,Stream).

% readWord(InStream,W) :-
%     get0(InStream,Char),
%     checkCharAndReadRest(Char,Chars,InStream),
%     atom_chars(W,Chars).
% checkCharAndReadRest(10,[],_) :- !. % Return
% checkCharAndReadRest(32,[],_) :- !. % Space
% checkCharAndReadRest(-1,[],_) :- !. % End of Stream
% checkCharAndReadRest(end_of_file,[],_) :- !.
% checkCharAndReadRest(Char,[Char|Chars],InStream) :-
%     get0(InStream,NextChar), checkCharAndReadRest(NextChar,Chars,InStream).

fol2file(F1,F2,F3,F4):-
    open('test.tex',write,Stream,[encoding(utf8)]),
    write(Stream,'\\documentclass{jsarticle}'),
    nl(Stream),
    write(Stream,'\\pagestyle{empty}'),
    nl(Stream),
    write(Stream,'%\\usepackage{lscape}'),
    nl(Stream),
    write(Stream,'\\newcommand{\\dConj}[2]{\\left[\\kern-0.3em\\begin{array}{l}\\vspace{0.1mm}#1\\\\#2\\end{array}\\kern-0.3em\\right]}'),
    nl(Stream),
    write(Stream,'\\newcommand{\\dPi}[2]{\\left(\\kern-0.3em\\begin{array}{l}#1\\end{array}\\kern-0.3em\\right)\\\\ \\qquad \\to #2.}'),
    nl(Stream),
    write(Stream,'\\newcommand{\\dImp}[2]{#1\\\\ \\qquad \\to #2.}'),
    nl(Stream),
    write(Stream,'\\newcommand{\\Entity}{\\ensuremath{\\mathbf{entity}}}'),
    nl(Stream),
    write(Stream,'\\newcommand{\\Event}{\\ensuremath{\\mathbf{event}}}'),
    nl(Stream),
    write(Stream,'\\newcommand{\\Prop}{\\ensuremath{\\mathbf{prop}}}'),
    nl(Stream),
    write(Stream,'\\begin{document}'),
    nl(Stream),
    write(Stream,'%\\begin{landscape}'),
    nl(Stream),
    write(Stream,'\\scriptsize'),
    % write(Stream,'$'), % added line
    % fol2latex(F0,Stream), % added line
    % write(Stream,'$'), % added line
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
    % numbervars(F2,0,_),
    nl(Stream),
    fol2latexlist(F2,Stream),
    nl(Stream),
    write(Stream,'\\bigskip'),
    write(Stream,'\\noindent\\textsc{Normalized '),
    write(Stream,'(SR'),
    % write(Stream,F4),
    fol2latex(F4,Stream),
    write(Stream,')}:'),
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

fol2latex(exists(X,Type,F),Stream):- !,
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
    X == v, !,
    write(Stream,'\\dConj'),
    write(Stream,'{'),
    write_term(Stream,X,[numbervars(true)]),
    write(Stream,': \\Event}'),
    write(Stream,'{'),
    fol2latex(F,Stream),
    write(Stream,'}').

fol2latex(exists(X,F),Stream):-
    atom_concat(v,_,X), !,
    write(Stream,'\\dConj'),
    write(Stream,'{'),
    write_term(Stream,X,[numbervars(true)]),
    write(Stream,': \\Event}'),
    write(Stream,'{'),
    fol2latex(F,Stream),
    write(Stream,'}').

fol2latex(exists(X,F),Stream):-
    X == g, !,
    write(Stream,'\\dConj'),
    write(Stream,'{'),
    write_term(Stream,X,[numbervars(true)]),
    write(Stream,': \\Entity \\to \\Prop}'),
    write(Stream,'{'),
    fol2latex(F,Stream),
    write(Stream,'}').

fol2latex(exists(X,F),Stream):-
    atom_concat(g,_,X), !,
    write(Stream,'\\dConj'),
    write(Stream,'{'),
    write_term(Stream,X,[numbervars(true)]),
    write(Stream,': \\Entity \\to \\Prop}'),
    write(Stream,'{'),
    fol2latex(F,Stream),
    write(Stream,'}').

fol2latex(exists(X,F),Stream):-
    write(Stream,'\\dConj'),
    write(Stream,'{'),
    write_term(Stream,X,[numbervars(true)]),
    write(Stream,': \\Entity}'),
    write(Stream,'{'),
    fol2latex(F,Stream),
    write(Stream,'}').

fol2latex(forall(X,F),Stream):-
    X == v, !,
    write(Stream,'\\dPi'),
    write(Stream,'{'),
    write_term(Stream,X,[numbervars(true)]),
    write(Stream,': \\Event}'),
    write(Stream,'{'),
    fol2latex(F,Stream),
    write(Stream,'}').

fol2latex(forall(X,F),Stream):-
    atom_concat(v,_,X), !,
    write(Stream,'\\dPi'),
    write(Stream,'{'),
    write_term(Stream,X,[numbervars(true)]),
    write(Stream,': \\Event}'),
    write(Stream,'{'),
    fol2latex(F,Stream),
    write(Stream,'}').

fol2latex(forall(X,F),Stream):-
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

fol2latex(lt(F1,F2),Stream):- !,
    write(Stream,'\\left('),
    fol2latex(F1,Stream),
    write(Stream,' < '),
    fol2latex(F2,Stream),
    write(Stream,'\\right)').

fol2latex(not(F),Stream):- !,
    write(Stream,'\\neg'),
    fol2latex(F,Stream).

fol2latex(entity,Stream):-
    write(Stream,'\\mbox{\\textbf{entity}}').

fol2latex(event,Stream):-
    write(Stream,'\\mbox{\\textbf{event}}').

fol2latex(state,Stream):-
    write(Stream,'\\mbox{\\textbf{state}}').

fol2latex(evt,Stream):-
    write(Stream,'\\mbox{\\textbf{evt}}').

fol2latex(prop,Stream):-
    write(Stream,'\\mbox{\\textbf{prop}}').

fol2latex(property,Stream):-
    write(Stream,'\\mbox{$\\mathbf{entity} \\to \\mathbf{prop}$}').

fol2latex([A,B],Stream):-
    write(Stream,'('),
    fol2latex(A,Stream),
    write(Stream,','),
    fol2latex(B,Stream),
    write(Stream,')').

% fol2latex(F,Stream):-
%     atomic(F),
%     sub_atom(F,0,1,_,c),
%     sub_atom(F,1,_,0,G),
%     write_term(Stream,G,[numbervars(true)]), !.

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
    F =.. [@,N,T],
    write(Stream,'@'),
    write_term(Stream,N,[numbervars(true)]),
    write(Stream,' : '),
    fol2latex(T,Stream).

% fol2latex(F,Stream):-
%     F =.. [CSymbol,Form],
%     write(Stream,'\\mbox{\\textbf{'),
%     sub_atom(CSymbol,0,1,_,c),
%     sub_atom(CSymbol,1,_,0,Symbol),
%     write_term(Stream,Symbol,[numbervars(true)]),
%     write(Stream,'}}'),
%     write(Stream,'\\left('),
%     fol2latex(Form,Stream),
%     write(Stream,'\\right)'), !.

fol2latex(F,Stream):-
    F =.. [Symbol,Form],
    write(Stream,'\\mbox{\\textbf{'),
    write_term(Stream,Symbol,[numbervars(true)]),
    write(Stream,'}}'),
    write(Stream,'\\left('),
    fol2latex(Form,Stream),
    write(Stream,'\\right)').

% fol2latex(F,Stream):-
%     F =.. [CSymbol,Arg1,Arg2],
%     write(Stream,'\\mbox{\\textbf{'),
%     sub_atom(CSymbol,0,1,_,c),
%     sub_atom(CSymbol,1,_,0,Symbol),
%     write_term(Stream,Symbol,[numbervars(true)]),
%     write(Stream,'}}('),
%     fol2latex(Arg2,Stream),
%     write(Stream,','),
%     fol2latex(Arg1,Stream),
%     write(Stream,')'), !.

fol2latex(F,Stream):-
    F =.. [Symbol,Arg1,Arg2],
    write(Stream,'\\mbox{\\textbf{'),
    write_term(Stream,Symbol,[numbervars(true)]),
    write(Stream,'}}('),
    fol2latex(Arg1,Stream),
    write(Stream,','),
    fol2latex(Arg2,Stream),
    write(Stream,')').

% fol2latex(F,Stream):-
%     F =.. [CSymbol,Arg1,Arg2,Arg3],
%     write(Stream,'\\mbox{\\textbf{'),
%     sub_atom(CSymbol,0,1,_,c),
%     sub_atom(CSymbol,1,_,0,Symbol),
%     write_term(Stream,Symbol,[numbervars(true)]),
%     write(Stream,'}}('),
%     fol2latex(Arg3,Stream),
%     write(Stream,','),
%     fol2latex(Arg2,Stream),
%     write(Stream,','),
%     fol2latex(Arg1,Stream),
%     write(Stream,')'), !.

fol2latex(F,Stream):-
    F =.. [Symbol,Arg1,Arg2,Arg3],
    write(Stream,'\\mbox{\\textbf{'),
    write_term(Stream,Symbol,[numbervars(true)]),
    write(Stream,'}}('),
    fol2latex(Arg1,Stream),
    write(Stream,','),
    fol2latex(Arg2,Stream),
    write(Stream,','),
    fol2latex(Arg3,Stream),
    write(Stream,')').

% fol2latex(F,Stream):-
%     F =.. [CSymbol,Arg1,Arg2,Arg3,Arg4],
%     write(Stream,'\\mbox{\\textbf{'),
%     sub_atom(CSymbol,0,1,_,c),
%     sub_atom(CSymbol,1,_,0,Symbol),
%     write_term(Stream,Symbol,[numbervars(true)]),
%     write(Stream,'}}('),
%     fol2latex(Arg4,Stream),
%     write(Stream,','),
%     fol2latex(Arg3,Stream),
%     write(Stream,','),
%     fol2latex(Arg2,Stream),
%     write(Stream,','),
%     fol2latex(Arg1,Stream),
%     write(Stream,')'), !.

fol2latex(F,Stream):-
    F =.. [Symbol,Arg1,Arg2,Arg3,Arg4],
    write(Stream,'\\mbox{\\textbf{'),
    write_term(Stream,Symbol,[numbervars(true)]),
    write(Stream,'}}('),
    fol2latex(Arg1,Stream),
    write(Stream,','),
    fol2latex(Arg2,Stream),
    write(Stream,','),
    fol2latex(Arg3,Stream),
    write(Stream,','),
    fol2latex(Arg4,Stream),
    write(Stream,')').

