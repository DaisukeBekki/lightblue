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
checkTheorem(syn , 216 , [ type:set ,q4 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , e : (( type )) , a : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( ~( ( ( q4 ) )-( ( a ) )-( ( e ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN180-1.p').
checkTheorem(syn , 217 , [ type:set ,q4 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , b : (( type )) , d : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( ~( ( ( q4 ) )-( ( d ) )-( ( b ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN181-1.p').
checkTheorem(syn , 218 , [ type:set ,q5 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , c : (( type )) , bigX : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( ~( ( ( q5 ) )-( ( bigX ) )-( ( c ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN182-1.p').
checkTheorem(syn , 219 , [ type:set ,q5 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , e : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( ~( ( ( q5 ) )-( ( e ) )-( ( e ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN183-1.p').
checkTheorem(syn , 220 , [ type:set ,r0 : (( ( type ) ) -> ( ( prop ) )) , b : (( ( type ) ) -> ( ( prop ) )) ] , ~( ~( ( ( r0 ) )-( ( b ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN184-1.p').
checkTheorem(syn , 221 , [ type:set ,r0 : (( ( type ) ) -> ( ( prop ) )) , e : (( ( type ) ) -> ( ( prop ) )) ] , ~( ~( ( ( r0 ) )-( ( e ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN185-1.p').
