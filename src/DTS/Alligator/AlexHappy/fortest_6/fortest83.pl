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
checkTheorem(syn , 474 , [ type:set ,f : (( ( type ) ) -> ( ( prop ) )) ] , ( ( ~( sigma(X:type,( ( f ) )-( ( bigX ) )) ) ) -> ( pi(Y:type,~( ( ( f ) )-( ( bigY ) ) )) ) ) & ( ( pi(Y:type,~( ( ( f ) )-( ( bigY ) ) )) ) -> ( ~( sigma(X:type,( ( f ) )-( ( bigX ) )) ) ) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN397+1.p').

checkTheorem(syn , 475 , [ type:set ,f : (( ( type ) ) -> ( ( prop ) )) , p : (( ( type ) ) -> ( ( prop ) )) ] , ( ( pi(X:type,( ( p ) ) & ( ( ( f ) )-( ( bigX ) ) )) ) -> ( ( ( p ) ) & ( pi(Y:type,( ( f ) )-( ( bigY ) )) ) ) ) & ( ( ( ( p ) ) & ( pi(Y:type,( ( f ) )-( ( bigY ) )) ) ) -> ( pi(X:type,( ( p ) ) & ( ( ( f ) )-( ( bigX ) ) )) ) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN398+1.p').
checkTheorem(syn , 476 , [ type:set ,p : (( type )) , f : (( ( type ) ) -> ( ( prop ) )) ] , ( pi(X:type,( ( ( ( f ) )-( ( bigX ) ) ) -> ( ( p ) ) ) & ( ( ( p ) ) -> ( ( ( f ) )-( ( bigX ) ) ) )) ) -> ( ( ( pi(Y:type,( ( f ) )-( ( bigY ) )) ) -> ( ( p ) ) ) & ( ( ( p ) ) -> ( pi(Y:type,( ( f ) )-( ( bigY ) )) ) ) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN399+1.p').
checkTheorem(syn , 477 , [ type:set ,p : (( type )) ] , ( ( pi(X:type,( p )) ) -> ( ( p ) ) ) & ( ( ( p ) ) -> ( pi(X:type,( p )) ) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN400+1.p').
checkTheorem(syn , 478 , [ type:set ,f : (( ( type ) ) -> ( ( prop ) )) ] , pi(X:type,( pi(Y:type,( ( f ) )-( ( bigY ) )) ) -> ( ( ( f ) )-( ( bigX ) ) )) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN401+1.p').
checkTheorem(syn , 479 , [ type:set ,f : (( ( type ) ) -> ( ( prop ) )) ] , pi(X:type,( ( ( f ) )-( ( bigX ) ) ) -> ( sigma(Y:type,( ( f ) )-( ( bigY ) )) )) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN402+1.p').
