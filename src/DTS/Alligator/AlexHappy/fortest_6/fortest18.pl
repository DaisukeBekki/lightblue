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
checkTheorem(syn , 102 , [] , (~(false)) , unknown , '../../TPTP-v7.3.0/Problems/SYN/SYN074-1.p').
checkTheorem(syn , 103 , [] , (~(false)) , unknown , '../../TPTP-v7.3.0/Problems/SYN/SYN074+1.p').
checkTheorem(syn , 104 , [] , (~(false)) , unknown , '../../TPTP-v7.3.0/Problems/SYN/SYN075-1.p').

checkTheorem(syn , 105 , [] , (~(false)) , unknown , '../../TPTP-v7.3.0/Problems/SYN/SYN076-1.p').


checkTheorem(syn , 106 , [ type:set ,big_f : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ~( sigma(W:type,pi(X:type,( ( ( ( big_f ) )-( ( bigX ) )-( ( bigW ) ) ) -> ( pi(U:type,( ( ( big_f ) )-( ( bigX ) )-( ( bigU ) ) ) -> ( sigma(Y:type,( ( ( big_f ) )-( ( bigY ) )-( ( bigU ) ) ) & ( ~( sigma(Z:type,( ( ( big_f ) )-( ( bigZ ) )-( ( bigU ) ) ) & ( ( ( big_f ) )-( ( bigZ ) )-( ( bigY ) ) )) ) )) )) ) ) & ( ( pi(U:type,( ( ( big_f ) )-( ( bigX ) )-( ( bigU ) ) ) -> ( sigma(Y:type,( ( ( big_f ) )-( ( bigY ) )-( ( bigU ) ) ) & ( ~( sigma(Z:type,( ( ( big_f ) )-( ( bigZ ) )-( ( bigU ) ) ) & ( ( ( big_f ) )-( ( bigZ ) )-( ( bigY ) ) )) ) )) )) ) -> ( ( ( big_f ) )-( ( bigX ) )-( ( bigW ) ) ) ))) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN077+1.p').


checkTheorem(syn , 107 , [ type:set ,bigZ : (( type )) , bigY : (( type )) , bigX : (( type )) , big_f : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , c : (( type )) , f : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , b : (( type )) , a : (( type )) , axiom2 : (( ( big_f ) )-( ( ( f ) )-( ( a ) )-( ( b ) ) )-( ( ( f ) )-( ( b ) )-( ( c ) ) )) , axiom1 : (( ( big_f ) )-( ( ( f ) )-( ( b ) )-( ( c ) ) )-( ( ( f ) )-( ( a ) )-( ( c ) ) )) , axiom0 : (( ~( ( ( big_f ) )-( ( bigX ) )-( ( bigY ) ) ) ) \/ ( ( ~( ( ( big_f ) )-( ( bigY ) )-( ( bigZ ) ) ) ) \/ ( ( ( big_f ) )-( ( bigX ) )-( ( bigZ ) ) ) )) ] , ~( ~( ( ( big_f ) )-( ( ( f ) )-( ( a ) )-( ( b ) ) )-( ( ( f ) )-( ( a ) )-( ( c ) ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN079-1.p').
