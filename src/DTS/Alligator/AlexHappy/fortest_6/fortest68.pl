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
checkTheorem(syn , 402 , [ type:set ,big_f : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) ] , sigma(X:type,pi(Y:type,sigma(Z:type,( ( ( big_f ) )-( ( bigX ) )-( ( bigY ) )-( ( bigZ ) ) ) -> ( ( ( big_f ) )-( ( bigY ) )-( ( bigZ ) )-( ( bigZ ) ) )))) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN339+1.p').
checkTheorem(syn , 403 , [ type:set ,f : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) ) ) )) , bigZ2 : (( type )) , bigZ1 : (( type )) , y : (( ( type ) ) -> ( ( prop ) )) , bigX : (( ( type ) ) -> ( ( prop ) )) ] , ( ~( ~( ( ( f ) )-( ( bigX ) )-( ( ( y ) )-( ( bigX ) ) )-( ( bigZ1 ) )-( ( bigZ2 ) )-( ( bigZ1 ) ) ) ) ) & ( ~( ( ( f ) )-( ( bigZ1 ) )-( ( bigX ) )-( ( ( y ) )-( ( bigX ) ) )-( ( bigZ1 ) )-( ( bigZ2 ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN340-1.p').
checkTheorem(syn , 404 , [ type:set ,big_f : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) ) ) )) ] , sigma(X:type,pi(Y:type,sigma(Z1:type,sigma(Z2:type,( ( ( big_f ) )-( ( bigX ) )-( ( bigY ) )-( ( bigZ1 ) )-( ( bigZ2 ) )-( ( bigZ1 ) ) ) -> ( ( ( big_f ) )-( ( bigZ1 ) )-( ( bigX ) )-( ( bigY ) )-( ( bigZ1 ) )-( ( bigZ2 ) ) ))))) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN340+1.p').
checkTheorem(syn , 405 , [ type:set ,y4 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , f : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) , bigY3 : (( type )) , y2 : (( ( type ) ) -> ( ( prop ) )) , bigY1 : (( ( type ) ) -> ( ( prop ) )) ] , ( ~( ~( ( ( f ) )-( ( bigY1 ) )-( ( ( y2 ) )-( ( bigY1 ) ) )-( ( bigY3 ) ) ) ) ) & ( ~( ( ( f ) )-( ( ( y2 ) )-( ( bigY1 ) ) )-( ( bigY3 ) )-( ( ( y4 ) )-( ( bigY1 ) )-( ( bigY3 ) ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN341-1.p').
checkTheorem(syn , 406 , [ type:set ,big_f : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) ] , sigma(X1:type,pi(X2:type,sigma(X3:type,pi(X4:type,( ( ( big_f ) )-( ( bigX1 ) )-( ( bigX2 ) )-( ( bigX3 ) ) ) -> ( ( ( big_f ) )-( ( bigX2 ) )-( ( bigX3 ) )-( ( bigX4 ) ) ))))) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN341+1.p').
checkTheorem(syn , 407 , [ type:set ,bigY4 : (( type )) , f : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) , y3 : (( ( type ) ) -> ( ( prop ) )) , bigY2 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) , a : (( ( type ) ) -> ( ( prop ) )) ] , ( ~( ~( ( ( f ) )-( ( a ) )-( ( bigY2 ) )-( ( ( y3 ) )-( ( bigY2 ) ) ) ) ) ) & ( ~( ( ( f ) )-( ( bigY4 ) )-( ( bigY4 ) )-( ( a ) ) ) ) , unknown , '../../TPTP-v7.3.0/Problems/SYN/SYN342-1.p').
