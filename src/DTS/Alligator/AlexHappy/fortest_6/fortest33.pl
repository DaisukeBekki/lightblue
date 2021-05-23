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
checkTheorem(syn , 192 , [ type:set ,n4 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , e : (( type )) , a : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( ~( ( ( n4 ) )-( ( a ) )-( ( e ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN156-1.p').
checkTheorem(syn , 193 , [ type:set ,n5 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , d : (( type )) , bigX : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( ~( ( ( n5 ) )-( ( bigX ) )-( ( d ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN157-1.p').
checkTheorem(syn , 194 , [ type:set ,n5 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , a : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( ~( ( ( n5 ) )-( ( a ) )-( ( a ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN158-1.p').
checkTheorem(syn , 195 , [ type:set ,n5 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , e : (( type )) , a : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( ~( ( ( n5 ) )-( ( a ) )-( ( e ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN159-1.p').
checkTheorem(syn , 196 , [ type:set ,n5 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , b : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( ~( ( ( n5 ) )-( ( b ) )-( ( b ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN160-1.p').
checkTheorem(syn , 197 , [ type:set ,n5 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , bigX : (( type )) , c : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( ~( ( ( n5 ) )-( ( c ) )-( ( bigX ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN161-1.p').
