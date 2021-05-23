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
checkTheorem(syn , 270 , [ type:set ,l3 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , bigX : (( type )) , e : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( ~( ( ( l3 ) )-( ( e ) )-( ( bigX ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN233-1.p').
checkTheorem(syn , 271 , [ type:set ,l3 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , a : (( type )) , e : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( ~( ( ( l3 ) )-( ( e ) )-( ( a ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN234-1.p').
checkTheorem(syn , 272 , [ type:set ,l3 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , b : (( type )) , e : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( ~( ( ( l3 ) )-( ( e ) )-( ( b ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN235-1.p').
checkTheorem(syn , 273 , [ type:set ,l3 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , e : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( ~( ( ( l3 ) )-( ( e ) )-( ( e ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN236-1.p').
checkTheorem(syn , 274 , [ type:set ,m1 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) , e : (( type )) , bigX : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) ] , ~( ~( ( ( m1 ) )-( ( bigX ) )-( ( bigX ) )-( ( e ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN237-1.p').
checkTheorem(syn , 275 , [ type:set ,m1 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) , bigY : (( type )) , b : (( type )) , bigX : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) ] , ~( ~( ( ( m1 ) )-( ( bigX ) )-( ( b ) )-( ( bigY ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN238-1.p').
