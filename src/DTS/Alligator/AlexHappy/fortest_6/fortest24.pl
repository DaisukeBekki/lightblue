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
checkTheorem(syn , 138 , [ type:set ,k0 : (( ( type ) ) -> ( ( prop ) )) , b : (( ( type ) ) -> ( ( prop ) )) ] , ~( ~( ( ( k0 ) )-( ( b ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN103-1.p').
checkTheorem(syn , 139 , [ type:set ,k0 : (( ( type ) ) -> ( ( prop ) )) , e : (( ( type ) ) -> ( ( prop ) )) ] , ~( ~( ( ( k0 ) )-( ( e ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN104-1.p').
checkTheorem(syn , 140 , [ type:set ,k1 : (( ( type ) ) -> ( ( prop ) )) , b : (( ( type ) ) -> ( ( prop ) )) ] , ~( ~( ( ( k1 ) )-( ( b ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN105-1.p').
checkTheorem(syn , 141 , [ type:set ,k1 : (( ( type ) ) -> ( ( prop ) )) , e : (( ( type ) ) -> ( ( prop ) )) ] , ~( ~( ( ( k1 ) )-( ( e ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN106-1.p').
checkTheorem(syn , 142 , [ type:set ,k2 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , d : (( type )) , bigX : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( ~( ( ( k2 ) )-( ( bigX ) )-( ( d ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN107-1.p').
checkTheorem(syn , 143 , [ type:set ,k2 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , bigX : (( type )) , a : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( ~( ( ( k2 ) )-( ( a ) )-( ( bigX ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN108-1.p').
