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
checkTheorem(syn , 480 , [ type:set ,h : (( ( type ) ) -> ( ( prop ) )) , g : (( ( type ) ) -> ( ( prop ) )) , f : (( ( type ) ) -> ( ( prop ) )) ] , pi(X:type,( ( ( ( ( f ) )-( ( bigX ) ) ) -> ( ( ( g ) )-( ( bigX ) ) ) ) & ( ( ( ( g ) )-( ( bigX ) ) ) -> ( ( ( h ) )-( ( bigX ) ) ) ) ) -> ( ( ( ( f ) )-( ( bigX ) ) ) -> ( ( ( h ) )-( ( bigX ) ) ) )) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN403+1.p').
checkTheorem(syn , 481 , [ type:set ,f : (( ( type ) ) -> ( ( prop ) )) ] , ( pi(X:type,( ( f ) )-( ( bigX ) )) ) -> ( sigma(Y:type,( ( f ) )-( ( bigY ) )) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN404+1.p').
checkTheorem(syn , 482 , [ type:set ,g : (( ( type ) ) -> ( ( prop ) )) , f : (( ( type ) ) -> ( ( prop ) )) ] , ( ( pi(X:type,( ( f ) )-( ( bigX ) )) ) & ( sigma(Y:type,( ( g ) )-( ( bigY ) )) ) ) -> ( sigma(Z:type,( ( ( f ) )-( ( bigZ ) ) ) & ( ( ( g ) )-( ( bigZ ) ) )) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN405+1.p').
checkTheorem(syn , 483 , [ type:set ,h : (( ( type ) ) -> ( ( prop ) )) , g : (( ( type ) ) -> ( ( prop ) )) , f : (( ( type ) ) -> ( ( prop ) )) ] , ( ( pi(X:type,( ( ( f ) )-( ( bigX ) ) ) -> ( ( ( g ) )-( ( bigX ) ) )) ) & ( sigma(Y:type,( ( ( f ) )-( ( bigY ) ) ) & ( ( ( h ) )-( ( bigY ) ) )) ) ) -> ( sigma(Z:type,( ( ( g ) )-( ( bigZ ) ) ) & ( ( ( h ) )-( ( bigZ ) ) )) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN406+1.p').
checkTheorem(syn , 484 , [ type:set ,h : (( ( type ) ) -> ( ( prop ) )) , g : (( ( type ) ) -> ( ( prop ) )) , f : (( ( type ) ) -> ( ( prop ) )) ] , ( pi(X:type,( ( ( f ) )-( ( bigX ) ) ) -> ( ( ( ( g ) )-( ( bigX ) ) ) \/ ( ( ( h ) )-( ( bigX ) ) ) )) ) -> ( ( pi(Y:type,( ( ( f ) )-( ( bigY ) ) ) -> ( ( ( g ) )-( ( bigY ) ) )) ) \/ ( sigma(Z:type,( ( ( f ) )-( ( bigZ ) ) ) & ( ( ( h ) )-( ( bigZ ) ) )) ) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN407+1.p').

checkTheorem(syn , 485 , [ type:set ,g : (( ( type ) ) -> ( ( prop ) )) , f : (( ( type ) ) -> ( ( prop ) )) ] , ( ~( sigma(X:type,( ( f ) )-( ( bigX ) )) ) ) -> ( pi(Y:type,( ( ( f ) )-( ( bigY ) ) ) -> ( ( ( g ) )-( ( bigY ) ) )) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN408+1.p').
