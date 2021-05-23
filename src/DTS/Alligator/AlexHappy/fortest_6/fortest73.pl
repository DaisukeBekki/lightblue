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
checkTheorem(syn , 432 , [ type:set ,big_g : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , big_f : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , pi(X1:type,pi(X2:type,sigma(Y1:type,sigma(Y2:type,pi(Z:type,( ( ( big_f ) )-( ( bigX1 ) )-( ( bigX2 ) ) ) -> ( ( ( ( big_g ) )-( ( bigX1 ) )-( ( bigX2 ) ) ) -> ( ( ( ( ( ( ( big_g ) )-( ( bigX2 ) )-( ( bigZ ) ) ) -> ( ( ( big_g ) )-( ( bigY2 ) )-( ( bigZ ) ) ) ) & ( ( ( ( big_g ) )-( ( bigY2 ) )-( ( bigZ ) ) ) -> ( ( ( big_g ) )-( ( bigX2 ) )-( ( bigZ ) ) ) ) ) -> ( ( ( ( big_f ) )-( ( bigY1 ) )-( ( bigY2 ) ) ) -> ( ( ( big_f ) )-( ( bigX2 ) )-( ( bigY2 ) ) ) ) ) -> ( ( ( ( ( ( big_g ) )-( ( bigX2 ) )-( ( bigZ ) ) ) -> ( ( ( big_g ) )-( ( bigY1 ) )-( ( bigZ ) ) ) ) & ( ( ( ( big_g ) )-( ( bigY1 ) )-( ( bigZ ) ) ) -> ( ( ( big_g ) )-( ( bigX2 ) )-( ( bigZ ) ) ) ) ) -> ( ( ( ( big_f ) )-( ( bigX1 ) )-( ( bigY1 ) ) ) & ( ( ( ( big_f ) )-( ( bigX2 ) )-( ( bigY1 ) ) ) & ( ( ( big_f ) )-( ( bigY1 ) )-( ( bigY2 ) ) ) ) ) ) ) )))))) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN354+1.p').
checkTheorem(syn , 433 , [ type:set ,big_q : (( ( type ) ) -> ( ( prop ) )) , big_p : (( ( type ) ) -> ( ( prop ) )) , big_r : (( ( type ) ) -> ( ( prop ) )) ] , ( ( pi(X:type,( ( ( big_r ) )-( ( bigX ) ) ) -> ( ( ( big_p ) )-( ( bigX ) ) )) ) & ( pi(X:type,( ~( ( ( big_q ) )-( ( bigX ) ) ) ) -> ( ( ( big_r ) )-( ( bigX ) ) )) ) ) -> ( pi(X:type,( ( ( big_p ) )-( ( bigX ) ) ) \/ ( ( ( big_q ) )-( ( bigX ) ) )) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN355+1.p').

checkTheorem(syn , 434 , [ type:set ,big_q : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , big_r : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , b : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , a : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ( ( ( ( big_r ) )-( ( a ) )-( ( b ) ) ) & ( ( pi(X:type,pi(Y:type,( ( ( big_r ) )-( ( bigX ) )-( ( bigY ) ) ) -> ( ( ( ( big_r ) )-( ( bigY ) )-( ( bigX ) ) ) & ( ( ( big_q ) )-( ( bigX ) )-( ( bigY ) ) ) ))) ) & ( pi(U:type,pi(V:type,( ( ( big_q ) )-( ( bigU ) )-( ( bigV ) ) ) -> ( ( ( big_q ) )-( ( bigU ) )-( ( bigU ) ) ))) ) ) ) -> ( ( ( ( big_q ) )-( ( a ) )-( ( a ) ) ) & ( ( ( big_q ) )-( ( b ) )-( ( b ) ) ) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN356+1.p').

checkTheorem(syn , 435 , [ type:set ,big_p : (( ( type ) ) -> ( ( prop ) )) ] , pi(X:type,sigma(Y:type,( ( ( big_p ) )-( ( bigX ) ) ) -> ( ( ( big_p ) )-( ( bigY ) ) ))) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN357+1.p').


checkTheorem(syn , 436 , [ type:set ,big_q : (( ( type ) ) -> ( ( prop ) )) , p : (( ( type ) ) -> ( ( prop ) )) ] , ( ( sigma(X:type,( ( p ) ) & ( ( ( big_q ) )-( ( bigX ) ) )) ) -> ( ( ( p ) ) & ( sigma(X:type,( ( big_q ) )-( ( bigX ) )) ) ) ) & ( ( ( ( p ) ) & ( sigma(X:type,( ( big_q ) )-( ( bigX ) )) ) ) -> ( sigma(X:type,( ( p ) ) & ( ( ( big_q ) )-( ( bigX ) ) )) ) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN358+1.p').

checkTheorem(syn , 437 , [ type:set ,big_q : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , big_r : (( ( type ) ) -> ( ( prop ) )) ] , ( ( sigma(X:type,( ( big_r ) )-( ( bigX ) )) ) & ( ( pi(Y:type,( ( ( big_r ) )-( ( bigY ) ) ) -> ( sigma(Z:type,( ( big_q ) )-( ( bigY ) )-( ( bigZ ) )) )) ) & ( pi(X:type,pi(Y:type,( ( ( big_q ) )-( ( bigX ) )-( ( bigY ) ) ) -> ( ( ( big_q ) )-( ( bigX ) )-( ( bigX ) ) ))) ) ) ) -> ( sigma(X:type,sigma(Y:type,( ( ( big_q ) )-( ( bigX ) )-( ( bigY ) ) ) & ( ( ( big_r ) )-( ( bigY ) ) ))) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN359+1.p').
