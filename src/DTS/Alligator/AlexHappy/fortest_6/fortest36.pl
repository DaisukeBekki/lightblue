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
checkTheorem(syn , 210 , [ type:set ,q1 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) , c : (( type )) , e : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) ] , ~( ~( ( ( q1 ) )-( ( e ) )-( ( c ) )-( ( e ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN174-1.p').
checkTheorem(syn , 211 , [ type:set ,q2 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) , bigY : (( type )) , d : (( type )) , bigX : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) ] , ~( ~( ( ( q2 ) )-( ( bigX ) )-( ( d ) )-( ( bigY ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN175-1.p').
checkTheorem(syn , 212 , [ type:set ,q2 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) , bigX : (( type )) , c : (( type )) , e : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) ] , ~( ~( ( ( q2 ) )-( ( e ) )-( ( c ) )-( ( bigX ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN176-1.p').
checkTheorem(syn , 213 , [ type:set ,q3 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , b : (( type )) , bigX : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( ~( ( ( q3 ) )-( ( bigX ) )-( ( b ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN177-1.p').
checkTheorem(syn , 214 , [ type:set ,q3 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , d : (( type )) , b : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( ~( ( ( q3 ) )-( ( b ) )-( ( d ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN178-1.p').
checkTheorem(syn , 215 , [ type:set ,q3 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , bigX : (( type )) , d : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( ~( ( ( q3 ) )-( ( d ) )-( ( bigX ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN179-1.p').
