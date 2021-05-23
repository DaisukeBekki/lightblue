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
checkTheorem(syn , 396 , [ type:set ,b2 : (( type )) , a2 : (( type )) , b : (( type )) , z : (( ( type ) ) -> ( ( prop ) )) , true : (( type )) , f : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , bigY : (( type )) , a : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , ifeq : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) ) )) , bigC : (( type )) , bigB : (( type )) , bigA : (( type )) , axiom0 : (( ( ( ( ifeq ) )-( ( bigA ) )-( ( bigA ) )-( ( bigB ) )-( ( bigC ) ) ) -> ( ( bigB ) ) ) & ( ( ( bigB ) ) -> ( ( ( ifeq ) )-( ( bigA ) )-( ( bigA ) )-( ( bigB ) )-( ( bigC ) ) ) )) ] , ( ~( ( ~( ( ~( ( ~( ( ~( ~( ( ( ( ( f ) )-( ( a ) )-( ( bigY ) ) ) -> ( ( true ) ) ) & ( ( ( true ) ) -> ( ( ( f ) )-( ( a ) )-( ( bigY ) ) ) ) ) ) ) & ( ( ( ( ( f ) )-( ( ( z ) )-( ( bigY ) ) )-( ( a ) ) ) -> ( ( true ) ) ) & ( ( ( true ) ) -> ( ( ( f ) )-( ( ( z ) )-( ( bigY ) ) )-( ( a ) ) ) ) ) ) ) & ( ( ( ( ( f ) )-( ( ( z ) )-( ( bigY ) ) )-( ( bigY ) ) ) -> ( ( true ) ) ) & ( ( ( true ) ) -> ( ( ( f ) )-( ( ( z ) )-( ( bigY ) ) )-( ( bigY ) ) ) ) ) ) ) & ( ( ( ( ( ifeq ) )-( ( ( f ) )-( ( b ) )-( ( bigY ) ) )-( ( true ) )-( ( a2 ) )-( ( b2 ) ) ) -> ( ( b2 ) ) ) & ( ( ( b2 ) ) -> ( ( ( ifeq ) )-( ( ( f ) )-( ( b ) )-( ( bigY ) ) )-( ( true ) )-( ( a2 ) )-( ( b2 ) ) ) ) ) ) ) & ( ( ( ( ( ifeq ) )-( ( ( f ) )-( ( b ) )-( ( ( z ) )-( ( bigY ) ) ) )-( ( true ) )-( ( a2 ) )-( ( b2 ) ) ) -> ( ( b2 ) ) ) & ( ( ( b2 ) ) -> ( ( ( ifeq ) )-( ( ( f ) )-( ( b ) )-( ( ( z ) )-( ( bigY ) ) ) )-( ( true ) )-( ( a2 ) )-( ( b2 ) ) ) ) ) ) ) & ( ~( ( ( ( a2 ) ) -> ( ( b2 ) ) ) & ( ( ( b2 ) ) -> ( ( a2 ) ) ) ) ) , unknown , '../../TPTP-v7.3.0/Problems/SYN/SYN337-10.p').
checkTheorem(syn , 397 , [ type:set ,b : (( type )) , z : (( ( type ) ) -> ( ( prop ) )) , f : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , bigY : (( type )) , a : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ( ~( ( ~( ( ~( ( ~( ~( ( ( f ) )-( ( a ) )-( ( bigY ) ) ) ) ) & ( ( ( f ) )-( ( ( z ) )-( ( bigY ) ) )-( ( a ) ) ) ) ) & ( ( ( f ) )-( ( ( z ) )-( ( bigY ) ) )-( ( bigY ) ) ) ) ) & ( ~( ( ( f ) )-( ( b ) )-( ( bigY ) ) ) ) ) ) & ( ~( ( ( f ) )-( ( b ) )-( ( ( z ) )-( ( bigY ) ) ) ) ) , unknown , '../../TPTP-v7.3.0/Problems/SYN/SYN337-1.p').
checkTheorem(syn , 398 , [ type:set ,big_f : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , pi(X1:type,pi(X2:type,sigma(Y:type,pi(Z:type,( ( ( big_f ) )-( ( bigX1 ) )-( ( bigY ) ) ) -> ( ( ( ( big_f ) )-( ( bigZ ) )-( ( bigX1 ) ) ) -> ( ( ( ( big_f ) )-( ( bigZ ) )-( ( bigY ) ) ) -> ( ( ( ( big_f ) )-( ( bigX2 ) )-( ( bigY ) ) ) \/ ( ( ( big_f ) )-( ( bigX2 ) )-( ( bigZ ) ) ) ) ) ))))) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN337+1.p').
checkTheorem(syn , 399 , [ type:set ,bigZ : (( type )) , f : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , y : (( ( type ) ) -> ( ( prop ) )) , bigX : (( ( type ) ) -> ( ( prop ) )) ] , ( ~( ( ~( ~( ( ( f ) )-( ( bigX ) )-( ( ( y ) )-( ( bigX ) ) ) ) ) ) & ( ( ( f ) )-( ( bigZ ) )-( ( bigX ) ) ) ) ) & ( ~( ( ( f ) )-( ( ( y ) )-( ( bigX ) ) )-( ( ( y ) )-( ( bigX ) ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN338-1.p').
checkTheorem(syn , 400 , [ type:set ,big_f : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , sigma(X:type,pi(Y:type,sigma(Z:type,( ( ( big_f ) )-( ( bigX ) )-( ( bigY ) ) ) -> ( ( ( ( big_f ) )-( ( bigZ ) )-( ( bigX ) ) ) -> ( ( ( big_f ) )-( ( bigY ) )-( ( bigY ) ) ) )))) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN338+1.p').
checkTheorem(syn , 401 , [ type:set ,f : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) , bigZ : (( type )) , y : (( ( type ) ) -> ( ( prop ) )) , bigX : (( ( type ) ) -> ( ( prop ) )) ] , ( ~( ~( ( ( f ) )-( ( bigX ) )-( ( ( y ) )-( ( bigX ) ) )-( ( bigZ ) ) ) ) ) & ( ~( ( ( f ) )-( ( ( y ) )-( ( bigX ) ) )-( ( bigZ ) )-( ( bigZ ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN339-1.p').
