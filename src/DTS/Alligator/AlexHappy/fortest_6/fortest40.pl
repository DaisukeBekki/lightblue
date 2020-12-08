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
checkTheorem(syn , 234 , [ type:set ,s1 : (( ( type ) ) -> ( ( prop ) )) , b : (( ( type ) ) -> ( ( prop ) )) ] , ~( ~( ( ( s1 ) )-( ( b ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN197-1.p').
checkTheorem(syn , 235 , [ type:set ,s1 : (( ( type ) ) -> ( ( prop ) )) , c : (( ( type ) ) -> ( ( prop ) )) ] , ~( ~( ( ( s1 ) )-( ( c ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN198-1.p').
checkTheorem(syn , 236 , [ type:set ,s1 : (( ( type ) ) -> ( ( prop ) )) , d : (( ( type ) ) -> ( ( prop ) )) ] , ~( ~( ( ( s1 ) )-( ( d ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN199-1.p').
checkTheorem(syn , 237 , [ type:set ,s1 : (( ( type ) ) -> ( ( prop ) )) , e : (( ( type ) ) -> ( ( prop ) )) ] , ~( ~( ( ( s1 ) )-( ( e ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN200-1.p').
checkTheorem(syn , 238 , [ type:set ,s2 : (( ( type ) ) -> ( ( prop ) )) , b : (( ( type ) ) -> ( ( prop ) )) ] , ~( ~( ( ( s2 ) )-( ( b ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN201-1.p').
checkTheorem(syn , 239 , [ type:set ,s2 : (( ( type ) ) -> ( ( prop ) )) , e : (( ( type ) ) -> ( ( prop ) )) ] , ~( ~( ( ( s2 ) )-( ( e ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN202-1.p').
