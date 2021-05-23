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
checkTheorem(syn , 456 , [ type:set ,big_q : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) , big_p : (( ( type ) ) -> ( ( prop ) )) ] , ( pi(X:type,( ( big_p ) )-( ( bigX ) )) ) -> ( sigma(Y:type,( pi(X:type,pi(Z:type,( ( big_q ) )-( ( bigX ) )-( ( bigY ) )-( ( bigZ ) ))) ) -> ( ~( pi(Z:type,( ( ( big_p ) )-( ( bigZ ) ) ) & ( ~( ( ( big_q ) )-( ( bigY ) )-( ( bigY ) )-( ( bigZ ) ) ) )) ) )) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN379+1.p').
checkTheorem(syn , 457 , [ type:set ,big_q : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , big_r : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ( pi(W:type,~( ( ( big_r ) )-( ( bigW ) )-( ( bigW ) ) )) ) -> ( sigma(X:type,sigma(Y:type,( ~( ( ( big_r ) )-( ( bigX ) )-( ( bigY ) ) ) ) & ( ( ( ( big_q ) )-( ( bigY ) )-( ( bigX ) ) ) -> ( pi(Z:type,( ( big_q ) )-( ( bigZ ) )-( ( bigZ ) )) ) ))) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN380+1.p').
checkTheorem(syn , 458 , [ type:set ,big_p : (( ( type ) ) -> ( ( prop ) )) , big_q : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ( ( pi(X:type,( sigma(Y:type,( ( big_q ) )-( ( bigX ) )-( ( bigY ) )) ) -> ( ( ( big_p ) )-( ( bigX ) ) )) ) & ( ( pi(V:type,sigma(U:type,( ( big_q ) )-( ( bigU ) )-( ( bigV ) ))) ) & ( pi(W:type,pi(Z:type,( ( ( big_q ) )-( ( bigW ) )-( ( bigZ ) ) ) -> ( ( ( ( big_q ) )-( ( bigZ ) )-( ( bigW ) ) ) \/ ( ( ( big_q ) )-( ( bigZ ) )-( ( bigZ ) ) ) ))) ) ) ) -> ( pi(Z:type,( ( big_p ) )-( ( bigZ ) )) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN381+1.p').

checkTheorem(syn , 459 , [ type:set ,big_q : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , big_p : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ( pi(Z:type,sigma(X:type,( pi(Y:type,( ( big_p ) )-( ( bigX ) )-( ( bigY ) )) ) \/ ( ( ( big_q ) )-( ( bigX ) )-( ( bigZ ) ) ))) ) -> ( pi(Y:type,sigma(X:type,( ( ( big_p ) )-( ( bigX ) )-( ( bigY ) ) ) \/ ( ( ( big_q ) )-( ( bigX ) )-( ( bigY ) ) ))) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN382+1.p').

checkTheorem(syn , 460 , [ type:set ,big_q : (( ( type ) ) -> ( ( prop ) )) , big_p : (( ( type ) ) -> ( ( prop ) )) ] , sigma(X:type,pi(Y:type,( ( ( ( big_p ) )-( ( bigX ) ) ) & ( ( ( big_q ) )-( ( bigY ) ) ) ) -> ( ( ( ( big_q ) )-( ( bigX ) ) ) \/ ( ( ( big_p ) )-( ( bigY ) ) ) ))) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN383+1.p').
checkTheorem(syn , 461 , [ type:set ,big_p : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) ] , pi(Z:type,sigma(X:type,sigma(Y:type,pi(U:type,( ( ( big_p ) )-( ( bigX ) )-( ( bigY ) )-( ( bigZ ) ) ) -> ( ( ( big_p ) )-( ( bigU ) )-( ( bigX ) )-( ( bigX ) ) ))))) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN384+1.p').
