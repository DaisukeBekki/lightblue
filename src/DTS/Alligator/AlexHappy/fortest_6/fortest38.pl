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
checkTheorem(syn , 222 , [ type:set ,r1 : (( ( type ) ) -> ( ( prop ) )) , a : (( ( type ) ) -> ( ( prop ) )) ] , ~( ~( ( ( r1 ) )-( ( a ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN186-1.p').
checkTheorem(syn , 223 , [ type:set ,r1 : (( ( type ) ) -> ( ( prop ) )) , d : (( ( type ) ) -> ( ( prop ) )) ] , ~( ~( ( ( r1 ) )-( ( d ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN187-1.p').
checkTheorem(syn , 224 , [ type:set ,r1 : (( ( type ) ) -> ( ( prop ) )) , e : (( ( type ) ) -> ( ( prop ) )) ] , ~( ~( ( ( r1 ) )-( ( e ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN188-1.p').
checkTheorem(syn , 225 , [ type:set ,r2 : (( ( type ) ) -> ( ( prop ) )) , c : (( ( type ) ) -> ( ( prop ) )) ] , ~( ~( ( ( r2 ) )-( ( c ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN189-1.p').
checkTheorem(syn , 226 , [ type:set ,r3 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) , d : (( type )) , bigX : (( type )) , a : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) ] , ~( ~( ( ( r3 ) )-( ( a ) )-( ( bigX ) )-( ( d ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN190-1.p').
checkTheorem(syn , 227 , [ type:set ,r3 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) , b : (( type )) , c : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) ] , ~( ~( ( ( r3 ) )-( ( c ) )-( ( b ) )-( ( c ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN191-1.p').
