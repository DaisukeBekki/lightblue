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
checkTheorem(syn , 180 , [ type:set ,m5 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , a : (( type )) , e : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( ~( ( ( m5 ) )-( ( e ) )-( ( a ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN144-1.p').
checkTheorem(syn , 181 , [ type:set ,n0 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , bigX : (( type )) , d : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( ~( ( ( n0 ) )-( ( d ) )-( ( bigX ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN145-1.p').
checkTheorem(syn , 182 , [ type:set ,n1 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) , bigY : (( type )) , c : (( type )) , bigX : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) ] , ~( ~( ( ( n1 ) )-( ( bigX ) )-( ( c ) )-( ( bigY ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN146-1.p').
checkTheorem(syn , 183 , [ type:set ,n1 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) , d : (( type )) , bigX : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) ] , ~( ~( ( ( n1 ) )-( ( bigX ) )-( ( d ) )-( ( bigX ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN147-1.p').
checkTheorem(syn , 184 , [ type:set ,n1 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) , d : (( type )) , c : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) ] , ~( ~( ( ( n1 ) )-( ( c ) )-( ( d ) )-( ( d ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN148-1.p').
checkTheorem(syn , 185 , [ type:set ,n1 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) , bigX : (( type )) , e : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) ] , ~( ~( ( ( n1 ) )-( ( e ) )-( ( e ) )-( ( bigX ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN149-1.p').
