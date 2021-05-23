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
checkTheorem(syn , 258 , [ type:set ,l3 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , c : (( type )) , bigX : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( ~( ( ( l3 ) )-( ( bigX ) )-( ( c ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN221-1.p').
checkTheorem(syn , 259 , [ type:set ,l3 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , d : (( type )) , bigX : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( ~( ( ( l3 ) )-( ( bigX ) )-( ( d ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN222-1.p').
checkTheorem(syn , 260 , [ type:set ,l3 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , e : (( type )) , bigX : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( ~( ( ( l3 ) )-( ( bigX ) )-( ( e ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN223-1.p').
checkTheorem(syn , 261 , [ type:set ,l3 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , bigX : (( type )) , a : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( ~( ( ( l3 ) )-( ( a ) )-( ( bigX ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN224-1.p').
checkTheorem(syn , 262 , [ type:set ,l3 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , a : (( type )) , b : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( ~( ( ( l3 ) )-( ( b ) )-( ( a ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN225-1.p').
checkTheorem(syn , 263 , [ type:set ,l3 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , b : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( ~( ( ( l3 ) )-( ( b ) )-( ( b ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN226-1.p').
