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
checkTheorem(syn , 366 , [ type:set ,f : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , a : (( type )) , bigX : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ( ~( ~( ( ~( ( ( f ) )-( ( bigX ) )-( ( a ) ) ) ) \/ ( ( ( f ) )-( ( a ) )-( ( bigX ) ) ) ) ) ) & ( ( ~( ( ( f ) )-( ( bigX ) )-( ( a ) ) ) ) \/ ( ~( ( ( f ) )-( ( a ) )-( ( bigX ) ) ) ) ) , unknown , '../../TPTP-v7.3.0/Problems/SYN/SYN322-1.p').
checkTheorem(syn , 367 , [ type:set ,big_f : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , sigma(Y:type,( pi(X:type,( ( ( big_f ) )-( ( bigX ) )-( ( bigY ) ) ) -> ( ( ( big_f ) )-( ( bigY ) )-( ( bigX ) ) )) ) -> ( ~( pi(X:type,( ( ( big_f ) )-( ( bigX ) )-( ( bigY ) ) ) -> ( ~( ( ( big_f ) )-( ( bigY ) )-( ( bigX ) ) ) )) ) )) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN322+1.p').
checkTheorem(syn , 368 , [ type:set ,g : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , f : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , a : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , bigX : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ( ~( ( ~( ( ~( ~( ( ( ( f ) )-( ( bigX ) )-( ( a ) ) ) \/ ( ( ( g ) )-( ( bigX ) )-( ( a ) ) ) ) ) ) & ( ( ~( ( ( f ) )-( ( a ) )-( ( bigX ) ) ) ) \/ ( ( ( g ) )-( ( bigX ) )-( ( a ) ) ) ) ) ) & ( ( ( ( f ) )-( ( bigX ) )-( ( a ) ) ) \/ ( ~( ( ( g ) )-( ( bigX ) )-( ( a ) ) ) ) ) ) ) & ( ( ~( ( ( f ) )-( ( a ) )-( ( bigX ) ) ) ) \/ ( ~( ( ( g ) )-( ( bigX ) )-( ( a ) ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN323-1.p').
checkTheorem(syn , 369 , [ type:set ,big_g : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , big_f : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , sigma(Y:type,( pi(X:type,( ( ( ( big_f ) )-( ( bigX ) )-( ( bigY ) ) ) -> ( ( ( big_f ) )-( ( bigY ) )-( ( bigX ) ) ) ) -> ( ( ( big_g ) )-( ( bigX ) )-( ( bigY ) ) )) ) -> ( ~( pi(X:type,( ( ( ( big_f ) )-( ( bigX ) )-( ( bigY ) ) ) -> ( ( ( big_f ) )-( ( bigY ) )-( ( bigX ) ) ) ) -> ( ~( ( ( big_g ) )-( ( bigX ) )-( ( bigY ) ) ) )) ) )) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN323+1.p').
checkTheorem(syn , 370 , [ type:set ,f : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , y : (( ( type ) ) -> ( ( prop ) )) , bigX : (( ( type ) ) -> ( ( prop ) )) ] , ( ~( ( ~( ( ~( ~( ( ( ( f ) )-( ( bigX ) )-( ( ( y ) )-( ( bigX ) ) ) ) \/ ( ~( ( ( f ) )-( ( bigX ) )-( ( bigX ) ) ) ) ) ) ) & ( ( ~( ( ( f ) )-( ( bigX ) )-( ( ( y ) )-( ( bigX ) ) ) ) ) \/ ( ( ( f ) )-( ( bigX ) )-( ( bigX ) ) ) ) ) ) & ( ( ( ( f ) )-( ( bigX ) )-( ( ( y ) )-( ( bigX ) ) ) ) \/ ( ( ( f ) )-( ( ( y ) )-( ( bigX ) ) )-( ( ( y ) )-( ( bigX ) ) ) ) ) ) ) & ( ( ~( ( ( f ) )-( ( bigX ) )-( ( ( y ) )-( ( bigX ) ) ) ) ) \/ ( ~( ( ( f ) )-( ( ( y ) )-( ( bigX ) ) )-( ( ( y ) )-( ( bigX ) ) ) ) ) ) , unknown , '../../TPTP-v7.3.0/Problems/SYN/SYN324-1.p').
checkTheorem(syn , 371 , [ type:set ,big_f : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , sigma(X:type,pi(Y:type,( ( ( ( ( big_f ) )-( ( bigX ) )-( ( bigY ) ) ) -> ( ( ( big_f ) )-( ( bigX ) )-( ( bigX ) ) ) ) & ( ( ( ( big_f ) )-( ( bigX ) )-( ( bigX ) ) ) -> ( ( ( big_f ) )-( ( bigX ) )-( ( bigY ) ) ) ) ) -> ( ( ( ( ( big_f ) )-( ( bigX ) )-( ( bigY ) ) ) -> ( ( ( big_f ) )-( ( bigY ) )-( ( bigY ) ) ) ) & ( ( ( ( big_f ) )-( ( bigY ) )-( ( bigY ) ) ) -> ( ( ( big_f ) )-( ( bigX ) )-( ( bigY ) ) ) ) ))) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN324+1.p').
