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
checkTheorem(syn , 48 , [ type:set ,s : (( type )) , r : (( type )) , q : (( type )) , p : (( type )) ] , ( ( ( ( ( p ) ) & ( ( ( q ) ) -> ( ( r ) ) ) ) -> ( ( s ) ) ) -> ( ( ( ~( ( p ) ) ) \/ ( ( ( q ) ) \/ ( ( s ) ) ) ) & ( ( ~( ( p ) ) ) \/ ( ( ~( ( r ) ) ) \/ ( ( s ) ) ) ) ) ) & ( ( ( ( ~( ( p ) ) ) \/ ( ( ( q ) ) \/ ( ( s ) ) ) ) & ( ( ~( ( p ) ) ) \/ ( ( ~( ( r ) ) ) \/ ( ( s ) ) ) ) ) -> ( ( ( ( p ) ) & ( ( ( q ) ) -> ( ( r ) ) ) ) -> ( ( s ) ) ) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN047+1.p').

checkTheorem(syn , 49 , [ type:set ,f : (( ( type ) ) -> ( ( prop ) )) , big_f : (( ( type ) ) -> ( ( prop ) )) , bigX : (( ( type ) ) -> ( ( prop ) )) ] , ( ~( ~( ( ( big_f ) )-( ( bigX ) ) ) ) ) & ( ~( ( ( big_f ) )-( ( ( f ) )-( ( bigX ) ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN048-1.p').
checkTheorem(syn , 50 , [ type:set ,big_f : (( ( type ) ) -> ( ( prop ) )) ] , sigma(Y:type,pi(X:type,( ( ( big_f ) )-( ( bigY ) ) ) -> ( ( ( big_f ) )-( ( bigX ) ) ))) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN048+1.p').
checkTheorem(syn , 51 , [ type:set ,big_q : (( ( type ) ) -> ( ( prop ) )) , g : (( ( type ) ) -> ( ( prop ) )) , big_p : (( ( type ) ) -> ( ( prop ) )) , f : (( ( type ) ) -> ( ( prop ) )) , bigX : (( ( type ) ) -> ( ( prop ) )) ] , ( ~( ( ~( ~( ( ~( ( ( big_p ) )-( ( ( f ) )-( ( bigX ) ) ) ) ) \/ ( ( ( big_q ) )-( ( ( g ) )-( ( bigX ) ) ) ) ) ) ) & ( ( ( big_p ) )-( ( bigX ) ) ) ) ) & ( ~( ( ( big_q ) )-( ( bigX ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN049-1.p').
checkTheorem(syn , 52 , [ type:set ,big_q : (( ( type ) ) -> ( ( prop ) )) , big_p : (( ( type ) ) -> ( ( prop ) )) ] , sigma(X:type,pi(Y:type,pi(Z:type,( ( ( ( big_p ) )-( ( bigY ) ) ) -> ( ( ( big_q ) )-( ( bigZ ) ) ) ) -> ( ( ( ( big_p ) )-( ( bigX ) ) ) -> ( ( ( big_q ) )-( ( bigX ) ) ) )))) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN049+1.p').

checkTheorem(syn , 53 , [ type:set ,bigW : (( type )) , b : (( type )) , a : (( type )) , big_s : (( ( type ) ) -> ( ( prop ) )) , bigX : (( ( type ) ) -> ( ( prop ) )) , big_r : (( ( type ) ) -> ( ( prop ) )) , f : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , big_q : (( ( type ) ) -> ( ( prop ) )) , bigZ : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , big_p : (( ( type ) ) -> ( ( prop ) )) , bigY : (( ( type ) ) -> ( ( prop ) )) ] , ( ~( ( ~( ( ~( ( ~( ~( ( ~( ( ( big_p ) )-( ( bigY ) ) ) ) \/ ( ( ~( ( ( big_q ) )-( ( bigZ ) ) ) ) \/ ( ( ( big_r ) )-( ( ( f ) )-( ( bigY ) )-( ( bigZ ) ) ) ) ) ) ) ) & ( ( ~( ( ( big_p ) )-( ( bigY ) ) ) ) \/ ( ( ~( ( ( big_q ) )-( ( bigZ ) ) ) ) \/ ( ( ( big_s ) )-( ( bigX ) ) ) ) ) ) ) & ( ( ( big_p ) )-( ( a ) ) ) ) ) & ( ( ( big_q ) )-( ( b ) ) ) ) ) & ( ~( ( ( big_r ) )-( ( bigW ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN050-1.p').
