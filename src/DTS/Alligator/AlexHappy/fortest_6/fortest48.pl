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
checkTheorem(syn , 282 , [ type:set ,m1 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) , bigX : (( type )) , c : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) ] , ~( ~( ( ( m1 ) )-( ( c ) )-( ( bigX ) )-( ( c ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN245-1.p').
checkTheorem(syn , 283 , [ type:set ,m1 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) , b : (( type )) , c : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) ] , ~( ~( ( ( m1 ) )-( ( c ) )-( ( b ) )-( ( c ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN246-1.p').
checkTheorem(syn , 284 , [ type:set ,m1 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) , bigY : (( type )) , bigX : (( type )) , d : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) ] , ~( ~( ( ( m1 ) )-( ( d ) )-( ( bigX ) )-( ( bigY ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN247-1.p').
checkTheorem(syn , 285 , [ type:set ,m1 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) , c : (( type )) , d : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) ] , ~( ~( ( ( m1 ) )-( ( d ) )-( ( c ) )-( ( c ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN248-1.p').
checkTheorem(syn , 286 , [ type:set ,m1 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) , e : (( type )) , d : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) ] , ~( ~( ( ( m1 ) )-( ( d ) )-( ( e ) )-( ( e ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN249-1.p').
checkTheorem(syn , 287 , [ type:set ,m1 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) , a : (( type )) , e : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) ] , ~( ~( ( ( m1 ) )-( ( e ) )-( ( a ) )-( ( a ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN250-1.p').
