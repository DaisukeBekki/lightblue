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
checkTheorem(syn , 312 , [ type:set ,q0 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , bigX : (( type )) , d : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( ~( ( ( q0 ) )-( ( d ) )-( ( bigX ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN275-1.p').
checkTheorem(syn , 313 , [ type:set ,q0 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , b : (( type )) , d : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( ~( ( ( q0 ) )-( ( d ) )-( ( b ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN276-1.p').
checkTheorem(syn , 314 , [ type:set ,q1 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) , d : (( type )) , bigX : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) ] , ~( ~( ( ( q1 ) )-( ( bigX ) )-( ( bigX ) )-( ( d ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN277-1.p').
checkTheorem(syn , 315 , [ type:set ,q1 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) , e : (( type )) , bigY : (( type )) , bigX : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) ] , ~( ~( ( ( q1 ) )-( ( bigX ) )-( ( bigY ) )-( ( e ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN278-1.p').
checkTheorem(syn , 316 , [ type:set ,q1 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) , c : (( type )) , a : (( type )) , bigX : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) ] , ~( ~( ( ( q1 ) )-( ( bigX ) )-( ( a ) )-( ( c ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN279-1.p').
checkTheorem(syn , 317 , [ type:set ,q1 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) , bigY : (( type )) , b : (( type )) , bigX : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) ] , ~( ~( ( ( q1 ) )-( ( bigX ) )-( ( b ) )-( ( bigY ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN280-1.p').
