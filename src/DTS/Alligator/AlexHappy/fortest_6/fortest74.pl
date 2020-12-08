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
checkTheorem(syn , 438 , [ type:set ,big_q : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , big_p : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ( ( pi(X:type,( sigma(Y:type,( ( big_p ) )-( ( bigX ) )-( ( bigY ) )) ) -> ( pi(Y:type,( ( big_q ) )-( ( bigX ) )-( ( bigY ) )) )) ) & ( pi(Z:type,sigma(Y:type,( ( big_p ) )-( ( bigZ ) )-( ( bigY ) ))) ) ) -> ( pi(Y:type,pi(X:type,( ( big_q ) )-( ( bigX ) )-( ( bigY ) ))) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN360+1.p').

checkTheorem(syn , 439 , [ type:set ,big_q : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , big_s : (( ( type ) ) -> ( ( prop ) )) , big_p : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ( ( sigma(V:type,pi(X:type,( ( big_p ) )-( ( bigX ) )-( ( bigV ) ))) ) & ( ( pi(X:type,( ( ( big_s ) )-( ( bigX ) ) ) -> ( sigma(Y:type,( ( big_q ) )-( ( bigY ) )-( ( bigX ) )) )) ) & ( pi(X:type,pi(Y:type,( ( ( big_p ) )-( ( bigX ) )-( ( bigY ) ) ) -> ( ~( ( ( big_q ) )-( ( bigX ) )-( ( bigY ) ) ) ))) ) ) ) -> ( sigma(U:type,~( ( ( big_s ) )-( ( bigU ) ) )) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN361+1.p').

checkTheorem(syn , 440 , [ type:set ,big_p : (( ( type ) ) -> ( ( prop ) )) , big_r : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ( ( pi(Y:type,sigma(W:type,( ( big_r ) )-( ( bigY ) )-( ( bigW ) ))) ) & ( sigma(Z:type,pi(X:type,( ( ( big_p ) )-( ( bigX ) ) ) -> ( ~( ( ( big_r ) )-( ( bigZ ) )-( ( bigX ) ) ) ))) ) ) -> ( sigma(X:type,~( ( ( big_p ) )-( ( bigX ) ) )) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN362+1.p').
checkTheorem(syn , 441 , [ type:set ,a : (( type )) , big_r : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , b : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ( ( pi(X:type,( ( big_r ) )-( ( bigX ) )-( ( b ) )) ) & ( pi(Y:type,( sigma(Z:type,( ( big_r ) )-( ( bigY ) )-( ( bigZ ) )) ) -> ( ( ( big_r ) )-( ( a ) )-( ( bigY ) ) )) ) ) -> ( sigma(U:type,pi(V:type,( ( big_r ) )-( ( bigU ) )-( ( bigV ) ))) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN363+1.p').
checkTheorem(syn , 442 , [ type:set ,g : (( ( type ) ) -> ( ( prop ) )) , big_q : (( ( type ) ) -> ( ( prop ) )) , f : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , big_m : (( ( type ) ) -> ( ( prop ) )) , big_p : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ( ( pi(X:type,( sigma(Y:type,( ( big_p ) )-( ( bigX ) )-( ( bigY ) )) ) -> ( pi(Z:type,( ( big_p ) )-( ( bigZ ) )-( ( bigZ ) )) )) ) & ( ( pi(U:type,sigma(V:type,( ( ( big_p ) )-( ( bigU ) )-( ( bigV ) ) ) \/ ( ( ( ( big_m ) )-( ( bigU ) ) ) & ( ( ( big_q ) )-( ( ( f ) )-( ( bigU ) )-( ( bigV ) ) ) ) ))) ) & ( pi(W:type,( ( ( big_q ) )-( ( bigW ) ) ) -> ( ~( ( ( big_m ) )-( ( ( g ) )-( ( bigW ) ) ) ) )) ) ) ) -> ( pi(U:type,sigma(V:type,( ( ( big_p ) )-( ( ( g ) )-( ( bigU ) ) )-( ( bigV ) ) ) & ( ( ( big_p ) )-( ( bigU ) )-( ( bigU ) ) ))) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN364+1.p').

checkTheorem(syn , 443 , [ type:set ,big_r : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , g : (( ( type ) ) -> ( ( prop ) )) , h : (( ( type ) ) -> ( ( prop ) )) , big_p : (( ( type ) ) -> ( ( prop ) )) ] , ( ( pi(X:type,sigma(Y:type,( ( ( big_p ) )-( ( bigX ) ) ) -> ( ( ( ( big_r ) )-( ( bigX ) )-( ( ( g ) )-( ( ( h ) )-( ( bigY ) ) ) ) ) & ( ( ( big_p ) )-( ( bigY ) ) ) ))) ) & ( pi(W:type,( ( ( big_p ) )-( ( bigW ) ) ) -> ( ( ( ( big_p ) )-( ( ( g ) )-( ( bigW ) ) ) ) & ( ( ( big_p ) )-( ( ( h ) )-( ( bigW ) ) ) ) )) ) ) -> ( pi(X:type,( ( ( big_p ) )-( ( bigX ) ) ) -> ( sigma(Y:type,( ( ( big_r ) )-( ( bigX ) )-( ( bigY ) ) ) & ( ( ( big_p ) )-( ( bigY ) ) )) )) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN365+1.p').
