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
checkTheorem(syn , 162 , [ type:set ,l4 : (( ( type ) ) -> ( ( prop ) )) , e : (( ( type ) ) -> ( ( prop ) )) ] , ~( ~( ( ( l4 ) )-( ( e ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN127-1.p').
checkTheorem(syn , 163 , [ type:set ,l5 : (( ( type ) ) -> ( ( prop ) )) , c : (( ( type ) ) -> ( ( prop ) )) ] , ~( ~( ( ( l5 ) )-( ( c ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN128-1.p').
checkTheorem(syn , 164 , [ type:set ,l5 : (( ( type ) ) -> ( ( prop ) )) , e : (( ( type ) ) -> ( ( prop ) )) ] , ~( ~( ( ( l5 ) )-( ( e ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN129-1.p').
checkTheorem(syn , 165 , [ type:set ,m0 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) , b : (( type )) , bigY : (( type )) , bigX : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) ] , ~( ~( ( ( m0 ) )-( ( bigX ) )-( ( bigY ) )-( ( b ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN130-1.p').
checkTheorem(syn , 166 , [ type:set ,m0 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) , e : (( type )) , bigY : (( type )) , bigX : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) ] , ~( ~( ( ( m0 ) )-( ( bigX ) )-( ( bigY ) )-( ( e ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN131-1.p').
checkTheorem(syn , 167 , [ type:set ,m0 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) , a : (( type )) , e : (( type )) , bigX : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) ] , ~( ~( ( ( m0 ) )-( ( bigX ) )-( ( e ) )-( ( a ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN132-1.p').
