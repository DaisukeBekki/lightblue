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
checkTheorem(syn , 444 , [ type:set ,big_r : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ( ( pi(U:type,pi(V:type,( ( ( ( big_r ) )-( ( bigU ) )-( ( bigU ) ) ) -> ( ( ( big_r ) )-( ( bigU ) )-( ( bigV ) ) ) ) & ( ( ( ( big_r ) )-( ( bigU ) )-( ( bigV ) ) ) -> ( ( ( big_r ) )-( ( bigU ) )-( ( bigU ) ) ) ))) ) & ( pi(W:type,pi(Z:type,( ( ( ( big_r ) )-( ( bigW ) )-( ( bigW ) ) ) -> ( ( ( big_r ) )-( ( bigZ ) )-( ( bigW ) ) ) ) & ( ( ( ( big_r ) )-( ( bigZ ) )-( ( bigW ) ) ) -> ( ( ( big_r ) )-( ( bigW ) )-( ( bigW ) ) ) ))) ) ) -> ( ( sigma(X:type,( ( big_r ) )-( ( bigX ) )-( ( bigX ) )) ) -> ( pi(Y:type,( ( big_r ) )-( ( bigY ) )-( ( bigY ) )) ) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN366+1.p').
checkTheorem(syn , 445 , [ type:set ,big_r : (( ( type ) ) -> ( ( prop ) )) , big_q : (( ( type ) ) -> ( ( prop ) )) , p : (( ( type ) ) -> ( ( prop ) )) ] , ( pi(X:type,( ( ( p ) ) & ( ( ( big_q ) )-( ( bigX ) ) ) ) \/ ( ( ~( ( p ) ) ) & ( ( ( big_r ) )-( ( bigX ) ) ) )) ) -> ( ( pi(X:type,( ( big_q ) )-( ( bigX ) )) ) \/ ( pi(X:type,( ( big_r ) )-( ( bigX ) )) ) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN367+1.p').

checkTheorem(syn , 446 , [ type:set ,big_p : (( ( type ) ) -> ( ( prop ) )) ] , sigma(Y:type,pi(X:type,( ( ( big_p ) )-( ( bigY ) ) ) -> ( ( ( big_p ) )-( ( bigX ) ) ))) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN368+1.p').
checkTheorem(syn , 447 , [ type:set ,big_p : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ( pi(U:type,pi(V:type,pi(W:type,( ( ( big_p ) )-( ( bigU ) )-( ( bigV ) ) ) \/ ( ( ( big_p ) )-( ( bigV ) )-( ( bigW ) ) )))) ) -> ( sigma(X:type,pi(Y:type,( ( big_p ) )-( ( bigX ) )-( ( bigY ) ))) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN369+1.p').
checkTheorem(syn , 448 , [ type:set ,f : (( ( type ) ) -> ( ( prop ) )) , big_p : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) , h : (( ( type ) ) -> ( ( prop ) )) , a : (( ( type ) ) -> ( ( prop ) )) ] , sigma(V:type,pi(Y:type,sigma(Z:type,( ( ( ( big_p ) )-( ( a ) )-( ( bigY ) )-( ( ( h ) )-( ( bigY ) ) ) ) \/ ( ( ( big_p ) )-( ( bigV ) )-( ( bigY ) )-( ( ( f ) )-( ( bigY ) ) ) ) ) -> ( ( ( big_p ) )-( ( bigV ) )-( ( bigY ) )-( ( bigZ ) ) )))) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN370+1.p').
checkTheorem(syn , 449 , [ type:set ,big_r : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ( ( sigma(X:type,( ( big_r ) )-( ( bigX ) )-( ( bigX ) )) ) -> ( pi(Y:type,( ( big_r ) )-( ( bigY ) )-( ( bigY ) )) ) ) -> ( sigma(U:type,pi(V:type,( ( ( big_r ) )-( ( bigU ) )-( ( bigU ) ) ) -> ( ( ( big_r ) )-( ( bigV ) )-( ( bigV ) ) ))) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN371+1.p').
