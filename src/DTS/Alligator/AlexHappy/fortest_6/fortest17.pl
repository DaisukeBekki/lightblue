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
checkTheorem(syn , 96 , [ type:set ,b : (( type )) , big_j : (( type )) , bigY : (( type )) , a : (( type )) , big_h : (( type )) , big_g : (( ( type ) ) -> ( ( prop ) )) , f : (( type )) , big_f : (( ( type ) ) -> ( ( prop ) )) , bigX : (( type )) , axiom6 : (( ~( ( ( big_f ) )-( ( bigX ) ) ) ) \/ ( ( ( ( big_f ) )-( ( ( f ) )-( ( bigX ) ) ) ) \/ ( ( ( big_g ) )-( ( bigX ) ) ) )) , axiom5 : (( ~( ( ( big_f ) )-( ( bigX ) ) ) ) \/ ( ( ( ( big_h ) )-( ( ( f ) )-( ( bigX ) ) )-( ( bigX ) ) ) \/ ( ( ( big_g ) )-( ( bigX ) ) ) )) , axiom4 : (( ~( ( ( big_f ) )-( ( bigX ) ) ) ) \/ ( ( ~( ( ( big_g ) )-( ( ( f ) )-( ( bigX ) ) ) ) ) \/ ( ( ( big_g ) )-( ( bigX ) ) ) )) , axiom3 : (( ~( ( ( big_f ) )-( ( bigX ) ) ) ) \/ ( ( ( ( big_g ) )-( ( bigX ) ) ) \/ ( ( ( big_f ) )-( ( a ) ) ) )) , axiom2 : (( ~( ( ( big_f ) )-( ( bigX ) ) ) ) \/ ( ( ( ( big_g ) )-( ( bigX ) ) ) \/ ( ~( ( ( big_g ) )-( ( a ) ) ) ) )) , axiom1 : (( ~( ( ( big_f ) )-( ( bigX ) ) ) ) \/ ( ( ( ( big_g ) )-( ( bigX ) ) ) \/ ( ( ~( ( ( big_f ) )-( ( bigY ) ) ) ) \/ ( ( ( ( big_g ) )-( ( bigY ) ) ) \/ ( ( ( big_j ) )-( ( a ) )-( ( bigY ) ) ) ) ) )) , axiom0 : (( ~( ( ( big_f ) )-( ( bigX ) ) ) ) \/ ( ( ~( ( ( big_f ) )-( ( bigY ) ) ) ) \/ ( ( ~( ( ( big_h ) )-( ( bigX ) )-( ( bigY ) ) ) ) \/ ( ~( ( ( big_j ) )-( ( bigY ) )-( ( bigX ) ) ) ) ) )) ] , ( ~( ~( ( ( big_f ) )-( ( b ) ) ) ) ) & ( ~( ( ( big_g ) )-( ( b ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN070-1.p').
checkTheorem(syn , 97 , [ type:set ,big_j : (( type )) , big_g : (( ( type ) ) -> ( ( prop ) )) , big_h : (( type )) , big_f : (( ( type ) ) -> ( ( prop ) )) , axiom2 : (pi(X:type,pi(Y:type,( ( ( ( big_f ) )-( ( bigX ) ) ) & ( ( ( ( ( big_f ) )-( ( bigY ) ) ) & ( ( ( big_h ) )-( ( bigY ) )-( ( bigX ) ) ) ) -> ( ( ( big_g ) )-( ( bigY ) ) ) ) ) -> ( ( ( big_g ) )-( ( bigX ) ) )))) , axiom1 : (( sigma(X:type,( ( ( big_f ) )-( ( bigX ) ) ) & ( ~( ( ( big_g ) )-( ( bigX ) ) ) )) ) -> ( sigma(X1:type,( ( ( big_f ) )-( ( bigX1 ) ) ) & ( ( ~( ( ( big_g ) )-( ( bigX1 ) ) ) ) & ( pi(Y:type,( ( ( ( big_f ) )-( ( bigY ) ) ) & ( ~( ( ( big_g ) )-( ( bigY ) ) ) ) ) -> ( ( ( big_j ) )-( ( bigX1 ) )-( ( bigY ) ) )) ) )) )) , axiom0 : (pi(X:type,pi(Y:type,( ( ( ( big_f ) )-( ( bigX ) ) ) & ( ( ( ( big_f ) )-( ( bigY ) ) ) & ( ( ( big_h ) )-( ( bigX ) )-( ( bigY ) ) ) ) ) -> ( ~( ( ( big_j ) )-( ( bigY ) )-( ( bigX ) ) ) )))) ] , pi(X:type,( ( ( big_f ) )-( ( bigX ) ) ) -> ( ( ( big_g ) )-( ( bigX ) ) )) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN070+1.p').
checkTheorem(syn , 98 , [ type:set ,c : (( type )) , b : (( type )) , d : (( type )) , a : (( type )) ] , ( ~( ~( ~( ( ( ( a ) ) -> ( ( d ) ) ) & ( ( ( d ) ) -> ( ( a ) ) ) ) ) ) ) & ( ~( ( ( ( b ) ) -> ( ( c ) ) ) & ( ( ( c ) ) -> ( ( b ) ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN071-1.p').


checkTheorem(syn , 99 , [ type:set ,b : (( type )) , big_p : (( ( type ) ) -> ( ( prop ) )) , a : (( type )) , axiom2 : (( ( big_p ) )-( ( a ) )) , axiom1 : (( ( big_p ) )-( ( b ) )) , axiom0 : (~( ( ( ( a ) ) -> ( ( b ) ) ) & ( ( ( b ) ) -> ( ( a ) ) ) )) ] , pi(X:type,( ( big_p ) )-( ( bigX ) )) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN072+1.p').
checkTheorem(syn , 100 , [ type:set ,f : (( ( type ) ) -> ( ( prop ) )) , bigY : (( type )) , big_f : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , bigX : (( type )) , a : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ( ~( ~( ( ( ( big_f ) )-( ( a ) )-( ( bigX ) ) ) \/ ( ( ( big_f ) )-( ( bigX ) )-( ( bigY ) ) ) ) ) ) & ( ~( ( ( big_f ) )-( ( bigX ) )-( ( ( f ) )-( ( bigX ) ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN073-1.p').
checkTheorem(syn , 101 , [ type:set ,big_f : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , a : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ( pi(X:type,( ( ( big_f ) )-( ( a ) )-( ( bigX ) ) ) \/ ( pi(Y:type,( ( big_f ) )-( ( bigX ) )-( ( bigY ) )) )) ) -> ( sigma(X1:type,pi(Y1:type,( ( big_f ) )-( ( bigX1 ) )-( ( bigY1 ) ))) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN073+1.p').
