:-use_module(swi_alligator,[prove/2]).

:- op(500,xfx,:).    % A:B reads A inhabits B / B is the type of A

:- op(300,yfx,-).    % function application (left associative)

:- op(400,xfy,=>).   % pi "implication" (right associative)

:- op(400,xfy,/\).   % sigma "conjunction" (right associative)

:- op(400,xfy,&).    % propositional sigma "conjunction" (right associative)

:- op(400,xfy,->).   % "implication" (non-dependent)

:- op(350,xfy,\/).   % "disjunction" (non-dependent)

:- op(300,fy,~).     % "negation"

:-dynamic checkTheorem/6.

% checkTheorem(ljt,1,[a:prop,b:prop],a -> (b -> a),yes,ab).

%%% Main Predicate %%%

writeResult(Logic,ID,Prediction,Gold) :-
   format('~w,~w,~w,~w,~w,~n', [Logic,ID,Prediction,Gold,Fname]).

eval(Logic,ID) :-
   checkTheorem(Logic,ID,Context,Theorem,Gold,Fname),
   ( prove(Context,_:Theorem) -> writeResult(Logic,ID,yes,Gold,Fname); writeResult(Logic,ID,no,Gold,Fname) ).
