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
checkTheorem(syn , 486 , [ type:set ,f : (( ( type ) ) -> ( ( prop ) )) ] , ( ( pi(X:type,( ( f ) )-( ( bigX ) )) ) -> ( pi(Y:type,pi(Z:type,( ( ( f ) )-( ( bigY ) ) ) & ( ( ( f ) )-( ( bigZ ) ) ))) ) ) & ( ( pi(Y:type,pi(Z:type,( ( ( f ) )-( ( bigY ) ) ) & ( ( ( f ) )-( ( bigZ ) ) ))) ) -> ( pi(X:type,( ( f ) )-( ( bigX ) )) ) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN409+1.p').
checkTheorem(syn , 487 , [ type:set ,f : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ( pi(X:type,pi(Y:type,( ( f ) )-( ( bigX ) )-( ( bigY ) ))) ) -> ( sigma(U:type,sigma(V:type,( ( f ) )-( ( bigU ) )-( ( bigV ) ))) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN410+1.p').
checkTheorem(syn , 488 , [ type:set ,f : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) ] , ( ( pi(X:type,pi(Y:type,pi(Z:type,( ( f ) )-( ( bigX ) )-( ( bigY ) )-( ( bigZ ) )))) ) -> ( ~( sigma(U:type,sigma(V:type,sigma(W:type,~( ( ( f ) )-( ( bigU ) )-( ( bigV ) )-( ( bigW ) ) )))) ) ) ) & ( ( ~( sigma(U:type,sigma(V:type,sigma(W:type,~( ( ( f ) )-( ( bigU ) )-( ( bigV ) )-( ( bigW ) ) )))) ) ) -> ( pi(X:type,pi(Y:type,pi(Z:type,( ( f ) )-( ( bigX ) )-( ( bigY ) )-( ( bigZ ) )))) ) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN411+1.p').
checkTheorem(syn , 489 , [ type:set ,f : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( sigma(X:type,pi(Y:type,( ( ( ( f ) )-( ( bigX ) )-( ( bigY ) ) ) -> ( ~( ( ( f ) )-( ( bigX ) )-( ( bigX ) ) ) ) ) & ( ( ~( ( ( f ) )-( ( bigX ) )-( ( bigX ) ) ) ) -> ( ( ( f ) )-( ( bigX ) )-( ( bigY ) ) ) ))) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN412+1.p').
checkTheorem(syn , 490 , [ type:set ,f : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ( pi(Z:type,sigma(Y:type,pi(X:type,( ( ( ( f ) )-( ( bigX ) )-( ( bigY ) ) ) -> ( ( ( ( f ) )-( ( bigX ) )-( ( bigZ ) ) ) & ( ~( ( ( f ) )-( ( bigX ) )-( ( bigX ) ) ) ) ) ) & ( ( ( ( ( f ) )-( ( bigX ) )-( ( bigZ ) ) ) & ( ~( ( ( f ) )-( ( bigX ) )-( ( bigX ) ) ) ) ) -> ( ( ( f ) )-( ( bigX ) )-( ( bigY ) ) ) )))) ) -> ( ~( sigma(V:type,pi(U:type,( ( f ) )-( ( bigU ) )-( ( bigV ) ))) ) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN413+1.p').
checkTheorem(syn , 491 , [ type:set ,g : (( ( type ) ) -> ( ( prop ) )) , f : (( ( type ) ) -> ( ( prop ) )) , h : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ( ( pi(X:type,( sigma(Y:type,( ( ( h ) )-( ( bigX ) )-( ( bigY ) ) ) & ( ( ( f ) )-( ( bigY ) ) )) ) -> ( sigma(Z:type,( ( ( h ) )-( ( bigX ) )-( ( bigZ ) ) ) & ( ( ( g ) )-( ( bigZ ) ) )) )) ) -> ( pi(U:type,pi(V:type,pi(W:type,( ( ( ( h ) )-( ( bigU ) )-( ( bigV ) ) ) & ( ( ( f ) )-( ( bigV ) ) ) ) -> ( ( ( ( h ) )-( ( bigU ) )-( ( bigW ) ) ) & ( ( ( g ) )-( ( bigW ) ) ) )))) ) ) & ( ( pi(U:type,pi(V:type,pi(W:type,( ( ( ( h ) )-( ( bigU ) )-( ( bigV ) ) ) & ( ( ( f ) )-( ( bigV ) ) ) ) -> ( ( ( ( h ) )-( ( bigU ) )-( ( bigW ) ) ) & ( ( ( g ) )-( ( bigW ) ) ) )))) ) -> ( pi(X:type,( sigma(Y:type,( ( ( h ) )-( ( bigX ) )-( ( bigY ) ) ) & ( ( ( f ) )-( ( bigY ) ) )) ) -> ( sigma(Z:type,( ( ( h ) )-( ( bigX ) )-( ( bigZ ) ) ) & ( ( ( g ) )-( ( bigZ ) ) )) )) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN414+1.p').
