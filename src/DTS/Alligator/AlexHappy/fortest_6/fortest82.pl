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
checkTheorem(syn , 468 , [ type:set ,p2 : (( type )) , p1 : (( type )) ] , ( ( ( ( p1 ) ) \/ ( ( p2 ) ) ) & ( ( ( ~( ( p1 ) ) ) \/ ( ( p2 ) ) ) & ( ( ( p1 ) ) \/ ( ~( ( p2 ) ) ) ) ) ) -> ( ~( ( ~( ( p1 ) ) ) \/ ( ~( ( p2 ) ) ) ) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN391+1.p').

checkTheorem(syn , 469 , [ type:set ,p2 : (( type )) , p1 : (( type )) ] , ( ( ( ( ( p1 ) ) -> ( ( p2 ) ) ) & ( ( ( p2 ) ) -> ( ( p1 ) ) ) ) -> ( ( ( ( p2 ) ) \/ ( ~( ( p1 ) ) ) ) & ( ( ~( ( p2 ) ) ) \/ ( ( p1 ) ) ) ) ) & ( ( ( ( ( p2 ) ) \/ ( ~( ( p1 ) ) ) ) & ( ( ~( ( p2 ) ) ) \/ ( ( p1 ) ) ) ) -> ( ( ( ( p1 ) ) -> ( ( p2 ) ) ) & ( ( ( p2 ) ) -> ( ( p1 ) ) ) ) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN392+1.p').

checkTheorem(syn , 470 , [ type:set ,p3 : (( type )) , p2 : (( type )) , p1 : (( type )) ] , ( ( ( ( ( ( ( p1 ) ) -> ( ( p2 ) ) ) & ( ( ( p2 ) ) -> ( ( p1 ) ) ) ) -> ( ( p3 ) ) ) & ( ( ( p3 ) ) -> ( ( ( ( p1 ) ) -> ( ( p2 ) ) ) & ( ( ( p2 ) ) -> ( ( p1 ) ) ) ) ) ) -> ( ( ( ( p1 ) ) -> ( ( ( ( p2 ) ) -> ( ( p3 ) ) ) & ( ( ( p3 ) ) -> ( ( p2 ) ) ) ) ) & ( ( ( ( ( p2 ) ) -> ( ( p3 ) ) ) & ( ( ( p3 ) ) -> ( ( p2 ) ) ) ) -> ( ( p1 ) ) ) ) ) & ( ( ( ( ( p1 ) ) -> ( ( ( ( p2 ) ) -> ( ( p3 ) ) ) & ( ( ( p3 ) ) -> ( ( p2 ) ) ) ) ) & ( ( ( ( ( p2 ) ) -> ( ( p3 ) ) ) & ( ( ( p3 ) ) -> ( ( p2 ) ) ) ) -> ( ( p1 ) ) ) ) -> ( ( ( ( ( ( p1 ) ) -> ( ( p2 ) ) ) & ( ( ( p2 ) ) -> ( ( p1 ) ) ) ) -> ( ( p3 ) ) ) & ( ( ( p3 ) ) -> ( ( ( ( p1 ) ) -> ( ( p2 ) ) ) & ( ( ( p2 ) ) -> ( ( p1 ) ) ) ) ) ) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN393+1.003.p').



checkTheorem(syn , 471 , [ type:set ,g : (( ( type ) ) -> ( ( prop ) )) , f : (( ( type ) ) -> ( ( prop ) )) ] , ( pi(X:type,( ( ( f ) )-( ( bigX ) ) ) -> ( ( ( g ) )-( ( bigX ) ) )) ) -> ( ( pi(Y:type,( ( f ) )-( ( bigY ) )) ) -> ( pi(Z:type,( ( g ) )-( ( bigZ ) )) ) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN394+1.p').
checkTheorem(syn , 472 , [ type:set ,g : (( ( type ) ) -> ( ( prop ) )) , f : (( ( type ) ) -> ( ( prop ) )) ] , ( pi(X:type,( ( ( f ) )-( ( bigX ) ) ) -> ( ( ( g ) )-( ( bigX ) ) )) ) -> ( ( sigma(Y:type,( ( f ) )-( ( bigY ) )) ) -> ( sigma(Z:type,( ( g ) )-( ( bigZ ) )) ) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN395+1.p').
checkTheorem(syn , 473 , [ type:set ,f : (( ( type ) ) -> ( ( prop ) )) ] , ( ( ~( pi(X:type,( ( f ) )-( ( bigX ) )) ) ) -> ( sigma(Y:type,~( ( ( f ) )-( ( bigY ) ) )) ) ) & ( ( sigma(Y:type,~( ( ( f ) )-( ( bigY ) ) )) ) -> ( ~( pi(X:type,( ( f ) )-( ( bigX ) )) ) ) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN396+1.p').
