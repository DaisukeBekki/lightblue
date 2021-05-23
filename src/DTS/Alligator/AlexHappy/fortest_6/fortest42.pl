:- style_check(-singleton).

:-use_module(swi_alligator,[prove/2]).
:- op(500,xfx,:).
:- op(300,yfx,-).
:- op(400,xfy,=>).
:- op(400,xfy,&).
:- op(300,fy,~).
:- dynamic checktheorem/6.
writeResult(Logic,ID,Prediction,Gold,Fname) :-
format('~w&~w&~w&~w&~w~n', [Logic,ID,Prediction,Gold,Fname]).
evalyes(Logic,ID) :-
  checkTheorem(Logic,ID,Context,Theorem,Gold,Fname),
  ( prove(Context,_ : Theorem) -> writeResult(Logic,ID,yes,Gold,Fname)).
evalno(Logic,ID) :-
  checkTheorem(Logic,ID,Context,Theorem,Gold,Fname),
  ( prove(Context,_ : ( ~ (Theorem) ) ) -> writeResult(Logic,ID,no,Gold,Fname) ).
checkTheorem(syn , 246 , [ type:set ,s4 : (( ( type ) ) -> ( ( prop ) )) , b : (( ( type ) ) -> ( ( prop ) )) ] , ~( ~( ( ( s4 ) )-( ( b ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN209-1.p').
checkTheorem(syn , 247 , [ type:set ,s4 : (( ( type ) ) -> ( ( prop ) )) , c : (( ( type ) ) -> ( ( prop ) )) ] , ~( ~( ( ( s4 ) )-( ( c ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN210-1.p').
checkTheorem(syn , 248 , [ type:set ,s4 : (( ( type ) ) -> ( ( prop ) )) , d : (( ( type ) ) -> ( ( prop ) )) ] , ~( ~( ( ( s4 ) )-( ( d ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN211-1.p').
checkTheorem(syn , 249 , [ type:set ,s4 : (( ( type ) ) -> ( ( prop ) )) , e : (( ( type ) ) -> ( ( prop ) )) ] , ~( ~( ( ( s4 ) )-( ( e ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN212-1.p').
checkTheorem(syn , 250 , [ type:set ,s5 : (( ( type ) ) -> ( ( prop ) )) , b : (( ( type ) ) -> ( ( prop ) )) ] , ~( ~( ( ( s5 ) )-( ( b ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN213-1.p').
checkTheorem(syn , 251 , [ type:set ,s5 : (( ( type ) ) -> ( ( prop ) )) , c : (( ( type ) ) -> ( ( prop ) )) ] , ~( ~( ( ( s5 ) )-( ( c ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN214-1.p').
