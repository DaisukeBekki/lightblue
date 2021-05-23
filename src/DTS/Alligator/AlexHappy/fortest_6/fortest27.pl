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
checkTheorem(syn , 156 , [ type:set ,l2 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , b : (( type )) , a : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( ~( ( ( l2 ) )-( ( a ) )-( ( b ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN121-1.p').
checkTheorem(syn , 157 , [ type:set ,l2 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , e : (( type )) , b : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( ~( ( ( l2 ) )-( ( b ) )-( ( e ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN122-1.p').
checkTheorem(syn , 158 , [ type:set ,l2 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , d : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( ~( ( ( l2 ) )-( ( d ) )-( ( d ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN123-1.p').
checkTheorem(syn , 159 , [ type:set ,l3 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , a : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( ~( ( ( l3 ) )-( ( a ) )-( ( a ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN124-1.p').
checkTheorem(syn , 160 , [ type:set ,l3 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , c : (( type )) , e : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( ~( ( ( l3 ) )-( ( e ) )-( ( c ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN125-1.p').
checkTheorem(syn , 161 , [ type:set ,l4 : (( ( type ) ) -> ( ( prop ) )) , b : (( ( type ) ) -> ( ( prop ) )) ] , ~( ~( ( ( l4 ) )-( ( b ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN126-1.p').
