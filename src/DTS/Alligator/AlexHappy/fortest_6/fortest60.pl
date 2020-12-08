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
checkTheorem(syn , 354 , [ type:set ,y : (( ( type ) ) -> ( ( prop ) )) , f : (( ( type ) ) -> ( ( prop ) )) , bigX : (( ( type ) ) -> ( ( prop ) )) ] , ( ~( ~( ( ~( ( ( f ) )-( ( bigX ) ) ) ) \/ ( ~( ( ( f ) )-( ( ( y ) )-( ( bigX ) ) ) ) ) ) ) ) & ( ( ( ( f ) )-( ( bigX ) ) ) \/ ( ( ( f ) )-( ( ( y ) )-( ( bigX ) ) ) ) ) , unknown , '../../TPTP-v7.3.0/Problems/SYN/SYN316-1.p').
checkTheorem(syn , 355 , [ type:set ,big_f : (( ( type ) ) -> ( ( prop ) )) ] , sigma(X:type,pi(Y:type,( ( ( ( big_f ) )-( ( bigX ) ) ) -> ( ( ( big_f ) )-( ( bigY ) ) ) ) & ( ( ( ( big_f ) )-( ( bigY ) ) ) -> ( ( ( big_f ) )-( ( bigX ) ) ) ))) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN316+1.p').
checkTheorem(syn , 356 , [ type:set ,b : (( type )) , a : (( type )) , c : (( type )) , g : (( ( type ) ) -> ( ( prop ) )) , f : (( ( type ) ) -> ( ( prop ) )) , bigZ : (( ( type ) ) -> ( ( prop ) )) ] , ( ~( ( ~( ~( ( ( f ) )-( ( bigZ ) ) ) ) ) & ( ~( ( ( g ) )-( ( bigZ ) ) ) ) ) ) & ( ( ~( ( ( f ) )-( ( c ) ) ) ) \/ ( ( ( ( g ) )-( ( c ) ) ) \/ ( ( ( ( f ) )-( ( a ) ) ) \/ ( ( ( g ) )-( ( b ) ) ) ) ) ) , unknown , '../../TPTP-v7.3.0/Problems/SYN/SYN317-1.p').
checkTheorem(syn , 357 , [ type:set ,big_g : (( ( type ) ) -> ( ( prop ) )) , big_f : (( ( type ) ) -> ( ( prop ) )) ] , ( ( sigma(X:type,( ( ( big_f ) )-( ( bigX ) ) ) -> ( ( ( big_g ) )-( ( bigX ) ) )) ) -> ( sigma(X:type,sigma(Y:type,( ( ( big_f ) )-( ( bigX ) ) ) -> ( ( ( big_g ) )-( ( bigY ) ) ))) ) ) & ( ( sigma(X:type,sigma(Y:type,( ( ( big_f ) )-( ( bigX ) ) ) -> ( ( ( big_g ) )-( ( bigY ) ) ))) ) -> ( sigma(X:type,( ( ( big_f ) )-( ( bigX ) ) ) -> ( ( ( big_g ) )-( ( bigX ) ) )) ) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN317+1.p').
checkTheorem(syn , 358 , [ type:set ,p : (( ( type ) ) -> ( ( prop ) )) , a : (( ( type ) ) -> ( ( prop ) )) , g : (( ( type ) ) -> ( ( prop ) )) , b : (( type )) , f : (( ( type ) ) -> ( ( prop ) )) , bigX : (( ( type ) ) -> ( ( prop ) )) ] , ( ~( ( ~( ( ~( ~( ( ~( ( ( f ) )-( ( bigX ) ) ) ) \/ ( ( ~( ( ( f ) )-( ( b ) ) ) ) \/ ( ( ( g ) )-( ( bigX ) ) ) ) ) ) ) & ( ( ( p ) )-( ( a ) ) ) ) ) & ( ( ( f ) )-( ( bigX ) ) ) ) ) & ( ~( ( ( g ) )-( ( b ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN318-1.p').
checkTheorem(syn , 359 , [ type:set ,p : (( type )) , big_g : (( ( type ) ) -> ( ( prop ) )) , big_f : (( ( type ) ) -> ( ( prop ) )) ] , sigma(Y:type,( pi(X:type,( ( ( big_f ) )-( ( bigX ) ) ) -> ( ( ( ( big_f ) )-( ( bigY ) ) ) -> ( ( ( big_g ) )-( ( bigX ) ) ) )) ) -> ( ( ( p ) ) -> ( pi(X:type,( ( ( big_f ) )-( ( bigX ) ) ) -> ( ( ( big_g ) )-( ( bigY ) ) )) ) )) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN318+1.p').
