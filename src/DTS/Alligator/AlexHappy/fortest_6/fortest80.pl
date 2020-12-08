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
checkTheorem(syn , 462 , [ type:set ,big_q : (( ( type ) ) -> ( ( prop ) )) , big_p : (( ( type ) ) -> ( ( prop ) )) ] , sigma(X:type,pi(Y:type,( ( ( big_p ) )-( ( bigX ) ) ) -> ( ( ( ( big_q ) )-( ( bigX ) ) ) \/ ( ( ( big_p ) )-( ( bigY ) ) ) ))) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN385+1.p').
checkTheorem(syn , 463 , [ type:set ,big_d : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) , big_s : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , big_f : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ( ( pi(X:type,sigma(Y:type,( ( big_f ) )-( ( bigX ) )-( ( bigY ) ))) ) & ( ( sigma(X:type,pi(E:type,sigma(N:type,pi(W:type,( ( ( big_s ) )-( ( bigN ) )-( ( bigW ) ) ) -> ( ( ( big_d ) )-( ( bigW ) )-( ( bigX ) )-( ( bigE ) ) ))))) ) & ( pi(E:type,sigma(D:type,pi(A:type,pi(B:type,( ( ( big_d ) )-( ( bigA ) )-( ( bigB ) )-( ( bigD ) ) ) -> ( pi(Y:type,pi(Z:type,( ( ( ( big_f ) )-( ( bigA ) )-( ( bigY ) ) ) & ( ( ( big_f ) )-( ( bigB ) )-( ( bigZ ) ) ) ) -> ( ( ( big_d ) )-( ( bigY ) )-( ( bigZ ) )-( ( bigE ) ) ))) ))))) ) ) ) -> ( sigma(Y:type,pi(E:type,sigma(M:type,pi(W:type,( ( ( big_s ) )-( ( bigM ) )-( ( bigW ) ) ) -> ( pi(Z:type,( ( ( big_f ) )-( ( bigW ) )-( ( bigZ ) ) ) -> ( ( ( big_d ) )-( ( bigZ ) )-( ( bigY ) )-( ( bigE ) ) )) ))))) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN386+1.p').

checkTheorem(syn , 464 , [ type:set ,p : (( type )) ] , ( ( p ) ) \/ ( ~( ( p ) ) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN387+1.p').


checkTheorem(syn , 465 , [ type:set ,p : (( type )) ] , ( ( p ) ) \/ ( ~( ~( ~( ( p ) ) ) ) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN388+1.p').

checkTheorem(syn , 466 , [ type:set ,q : (( type )) , p : (( type )) ] , ( ( ( ( p ) ) -> ( ( q ) ) ) -> ( ( p ) ) ) -> ( ( p ) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN389+1.p').

checkTheorem(syn , 467 , [ type:set ,p : (( type )) ] , ( ( ( p ) ) -> ( ( p ) ) ) & ( ( ( p ) ) -> ( ( p ) ) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN390+1.p').
