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
checkTheorem(syn , 252 , [ type:set ,s5 : (( ( type ) ) -> ( ( prop ) )) , d : (( ( type ) ) -> ( ( prop ) )) ] , ~( ~( ( ( s5 ) )-( ( d ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN215-1.p').
checkTheorem(syn , 253 , [ type:set ,l2 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , a : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( ~( ( ( l2 ) )-( ( a ) )-( ( a ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN216-1.p').
checkTheorem(syn , 254 , [ type:set ,l2 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , b : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( ~( ( ( l2 ) )-( ( b ) )-( ( b ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN217-1.p').
checkTheorem(syn , 255 , [ type:set ,l2 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , bigX : (( type )) , c : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( ~( ( ( l2 ) )-( ( c ) )-( ( bigX ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN218-1.p').
checkTheorem(syn , 256 , [ type:set ,l2 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , b : (( type )) , e : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( ~( ( ( l2 ) )-( ( e ) )-( ( b ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN219-1.p').
checkTheorem(syn , 257 , [ type:set ,l3 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , b : (( type )) , bigX : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( ~( ( ( l3 ) )-( ( bigX ) )-( ( b ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN220-1.p').
