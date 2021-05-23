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
checkTheorem(syn , 54 , [ type:set ,big_s : (( ( type ) ) -> ( ( prop ) )) , big_r : (( ( type ) ) -> ( ( prop ) )) , big_q : (( ( type ) ) -> ( ( prop ) )) , big_p : (( ( type ) ) -> ( ( prop ) )) ] , ( pi(X:type,pi(Y:type,sigma(Z:type,pi(W:type,( ( ( ( big_p ) )-( ( bigX ) ) ) & ( ( ( big_q ) )-( ( bigY ) ) ) ) -> ( ( ( ( big_r ) )-( ( bigZ ) ) ) & ( ( ( big_s ) )-( ( bigW ) ) ) ))))) ) -> ( sigma(X1:type,sigma(Y1:type,( ( ( ( big_p ) )-( ( bigX1 ) ) ) & ( ( ( big_q ) )-( ( bigY1 ) ) ) ) -> ( sigma(Z1:type,( ( big_r ) )-( ( bigZ1 ) )) ))) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN050+1.p').
checkTheorem(syn , 55 , [ type:set ,bigX : (( type )) , b : (( type )) , big_f : (( ( type ) ) -> ( ( prop ) )) , a : (( type )) , p : (( type )) , axiom1 : (( ~( ( p ) ) ) \/ ( ( ( big_f ) )-( ( a ) ) )) , axiom0 : (( ~( ( ( big_f ) )-( ( b ) ) ) ) \/ ( ( p ) )) ] , ( ~( ~( ( ( p ) ) \/ ( ( ( big_f ) )-( ( bigX ) ) ) ) ) ) & ( ( ~( ( ( big_f ) )-( ( bigX ) ) ) ) \/ ( ~( ( p ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN051-1.p').
checkTheorem(syn , 56 , [ type:set ,big_f : (( ( type ) ) -> ( ( prop ) )) , p : (( type )) , axiom1 : (sigma(X:type,( ( p ) ) -> ( ( ( big_f ) )-( ( bigX ) ) ))) , axiom0 : (sigma(X:type,( ( ( big_f ) )-( ( bigX ) ) ) -> ( ( p ) ))) ] , sigma(X:type,( ( ( p ) ) -> ( ( ( big_f ) )-( ( bigX ) ) ) ) & ( ( ( ( big_f ) )-( ( bigX ) ) ) -> ( ( p ) ) )) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN051+1.p').

checkTheorem(syn , 57 , [ type:set ,a : (( type )) , bigY : (( type )) , big_f : (( ( type ) ) -> ( ( prop ) )) , bigX : (( type )) , p : (( ( type ) ) -> ( ( prop ) )) ] , ( ~( ( ~( ( ~( ( ~( ~( ( ( p ) ) \/ ( ~( ( ( big_f ) )-( ( bigX ) ) ) ) ) ) ) & ( ( ( ( big_f ) )-( ( bigX ) ) ) \/ ( ~( ( p ) ) ) ) ) ) & ( ( ( ( big_f ) )-( ( bigY ) ) ) \/ ( ( p ) ) ) ) ) & ( ( ~( ( p ) ) ) \/ ( ~( ( ( big_f ) )-( ( a ) ) ) ) ) ) ) & ( ( ( ( big_f ) )-( ( bigY ) ) ) \/ ( ~( ( ( big_f ) )-( ( a ) ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN052-1.p').
checkTheorem(syn , 58 , [ type:set ,big_f : (( ( type ) ) -> ( ( prop ) )) , p : (( ( type ) ) -> ( ( prop ) )) ] , ( pi(X:type,( ( ( p ) ) -> ( ( ( big_f ) )-( ( bigX ) ) ) ) & ( ( ( ( big_f ) )-( ( bigX ) ) ) -> ( ( p ) ) )) ) -> ( ( ( ( p ) ) -> ( pi(X1:type,( ( big_f ) )-( ( bigX1 ) )) ) ) & ( ( pi(X1:type,( ( big_f ) )-( ( bigX1 ) )) ) -> ( ( p ) ) ) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN052+1.p').

checkTheorem(syn , 59 , [ type:set ,a : (( type )) , b : (( type )) , bigY : (( type )) , big_f : (( ( type ) ) -> ( ( prop ) )) , bigX : (( type )) , p : (( ( type ) ) -> ( ( prop ) )) ] , ( ~( ( ~( ( ~( ( ~( ~( ( ( p ) ) \/ ( ( ( ( big_f ) )-( ( bigX ) ) ) \/ ( ( ( big_f ) )-( ( bigY ) ) ) ) ) ) ) & ( ( ( p ) ) \/ ( ( ( ( big_f ) )-( ( bigX ) ) ) \/ ( ( ( big_f ) )-( ( b ) ) ) ) ) ) ) & ( ~( ( p ) ) ) ) ) & ( ( ~( ( ( big_f ) )-( ( a ) ) ) ) \/ ( ( ( p ) ) \/ ( ( ( big_f ) )-( ( bigY ) ) ) ) ) ) ) & ( ( ~( ( ( big_f ) )-( ( a ) ) ) ) \/ ( ~( ( ( big_f ) )-( ( b ) ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN053-1.p').
