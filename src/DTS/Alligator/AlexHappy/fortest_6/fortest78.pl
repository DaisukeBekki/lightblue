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
checkTheorem(syn , 450 , [ type:set ,big_q : (( ( type ) ) -> ( ( prop ) )) , big_p : (( ( type ) ) -> ( ( prop ) )) ] , ( ( sigma(X:type,( ( ( big_p ) )-( ( bigX ) ) ) -> ( ( ( big_q ) )-( ( bigX ) ) )) ) -> ( ( pi(X:type,( ( big_p ) )-( ( bigX ) )) ) -> ( sigma(X:type,( ( big_q ) )-( ( bigX ) )) ) ) ) & ( ( ( pi(X:type,( ( big_p ) )-( ( bigX ) )) ) -> ( sigma(X:type,( ( big_q ) )-( ( bigX ) )) ) ) -> ( sigma(X:type,( ( ( big_p ) )-( ( bigX ) ) ) -> ( ( ( big_q ) )-( ( bigX ) ) )) ) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN373+1.p').
checkTheorem(syn , 451 , [ type:set ,big_p : (( ( type ) ) -> ( ( prop ) )) ] , ( ( sigma(X:type,pi(Y:type,( ( ( ( big_p ) )-( ( bigX ) ) ) -> ( ( ( big_p ) )-( ( bigY ) ) ) ) & ( ( ( ( big_p ) )-( ( bigY ) ) ) -> ( ( ( big_p ) )-( ( bigX ) ) ) ))) ) -> ( ( ( sigma(X:type,( ( big_p ) )-( ( bigX ) )) ) -> ( pi(Y:type,( ( big_p ) )-( ( bigY ) )) ) ) & ( ( pi(Y:type,( ( big_p ) )-( ( bigY ) )) ) -> ( sigma(X:type,( ( big_p ) )-( ( bigX ) )) ) ) ) ) & ( ( ( ( sigma(X:type,( ( big_p ) )-( ( bigX ) )) ) -> ( pi(Y:type,( ( big_p ) )-( ( bigY ) )) ) ) & ( ( pi(Y:type,( ( big_p ) )-( ( bigY ) )) ) -> ( sigma(X:type,( ( big_p ) )-( ( bigX ) )) ) ) ) -> ( sigma(X:type,pi(Y:type,( ( ( ( big_p ) )-( ( bigX ) ) ) -> ( ( ( big_p ) )-( ( bigY ) ) ) ) & ( ( ( ( big_p ) )-( ( bigY ) ) ) -> ( ( ( big_p ) )-( ( bigX ) ) ) ))) ) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN374+1.p').

checkTheorem(syn , 452 , [ type:set ,big_p : (( ( type ) ) -> ( ( prop ) )) ] , ( ( pi(X:type,( ( ( ( big_p ) )-( ( bigX ) ) ) -> ( sigma(Y:type,( ( big_p ) )-( ( bigY ) )) ) ) & ( ( sigma(Y:type,( ( big_p ) )-( ( bigY ) )) ) -> ( ( ( big_p ) )-( ( bigX ) ) ) )) ) -> ( ( ( pi(X:type,( ( big_p ) )-( ( bigX ) )) ) -> ( sigma(Y:type,( ( big_p ) )-( ( bigY ) )) ) ) & ( ( sigma(Y:type,( ( big_p ) )-( ( bigY ) )) ) -> ( pi(X:type,( ( big_p ) )-( ( bigX ) )) ) ) ) ) & ( ( ( ( pi(X:type,( ( big_p ) )-( ( bigX ) )) ) -> ( sigma(Y:type,( ( big_p ) )-( ( bigY ) )) ) ) & ( ( sigma(Y:type,( ( big_p ) )-( ( bigY ) )) ) -> ( pi(X:type,( ( big_p ) )-( ( bigX ) )) ) ) ) -> ( pi(X:type,( ( ( ( big_p ) )-( ( bigX ) ) ) -> ( sigma(Y:type,( ( big_p ) )-( ( bigY ) )) ) ) & ( ( sigma(Y:type,( ( big_p ) )-( ( bigY ) )) ) -> ( ( ( big_p ) )-( ( bigX ) ) ) )) ) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN375+1.p').

checkTheorem(syn , 453 , [ type:set ,big_p : (( ( type ) ) -> ( ( prop ) )) ] , ( sigma(X:type,pi(Y:type,( ( ( ( big_p ) )-( ( bigY ) ) ) -> ( ( ( big_p ) )-( ( bigX ) ) ) ) & ( ( ( ( big_p ) )-( ( bigX ) ) ) -> ( ( ( big_p ) )-( ( bigY ) ) ) ))) ) -> ( ( pi(X:type,( ( big_p ) )-( ( bigX ) )) ) \/ ( pi(X:type,~( ( ( big_p ) )-( ( bigX ) ) )) ) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN376+1.p').
checkTheorem(syn , 454 , [ type:set ,big_p : (( ( type ) ) -> ( ( prop ) )) ] , ( ( pi(X:type,( ( ( ( big_p ) )-( ( bigX ) ) ) -> ( pi(Y:type,( ( big_p ) )-( ( bigY ) )) ) ) & ( ( pi(Y:type,( ( big_p ) )-( ( bigY ) )) ) -> ( ( ( big_p ) )-( ( bigX ) ) ) )) ) -> ( ( ( sigma(X:type,( ( big_p ) )-( ( bigX ) )) ) -> ( pi(Y:type,( ( big_p ) )-( ( bigY ) )) ) ) & ( ( pi(Y:type,( ( big_p ) )-( ( bigY ) )) ) -> ( sigma(X:type,( ( big_p ) )-( ( bigX ) )) ) ) ) ) & ( ( ( ( sigma(X:type,( ( big_p ) )-( ( bigX ) )) ) -> ( pi(Y:type,( ( big_p ) )-( ( bigY ) )) ) ) & ( ( pi(Y:type,( ( big_p ) )-( ( bigY ) )) ) -> ( sigma(X:type,( ( big_p ) )-( ( bigX ) )) ) ) ) -> ( pi(X:type,( ( ( ( big_p ) )-( ( bigX ) ) ) -> ( pi(Y:type,( ( big_p ) )-( ( bigY ) )) ) ) & ( ( pi(Y:type,( ( big_p ) )-( ( bigY ) )) ) -> ( ( ( big_p ) )-( ( bigX ) ) ) )) ) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN377+1.p').


checkTheorem(syn , 455 , [ type:set ,big_q : (( ( type ) ) -> ( ( prop ) )) , big_p : (( ( type ) ) -> ( ( prop ) )) ] , ( pi(X:type,( ( big_p ) )-( ( bigX ) )) ) -> ( ( ~( sigma(Y:type,( ( big_q ) )-( ( bigY ) )) ) ) \/ ( sigma(Z:type,( ( ( big_p ) )-( ( bigZ ) ) ) -> ( ( ( big_q ) )-( ( bigZ ) ) )) ) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN378+1.p').
