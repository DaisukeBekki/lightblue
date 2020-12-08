
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
checkTheorem(syn , 936 , [ type:set ,sk2 : (( type )) , p : (( ( type ) ) -> ( ( prop ) )) , l : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , g : (( ( type ) ) -> ( ( prop ) )) , h : (( ( type ) ) -> ( ( prop ) )) , sk1 : (( ( type ) ) -> ( ( prop ) )) , bigA : (( ( type ) ) -> ( ( prop ) )) ] , ( ~( ( ~( ( ~( ( ~( ( ~( ~( ( ( ( l ) )-( ( bigA ) )-( ( ( g ) )-( ( ( h ) )-( ( ( sk1 ) )-( ( bigA ) ) ) ) ) ) \/ ( ~( ( ( p ) )-( ( bigA ) ) ) ) ) ) ) & ( ( ( ( p ) )-( ( ( sk1 ) )-( ( bigA ) ) ) ) \/ ( ~( ( ( p ) )-( ( bigA ) ) ) ) ) ) ) & ( ( ( ( p ) )-( ( ( g ) )-( ( bigA ) ) ) ) \/ ( ~( ( ( p ) )-( ( bigA ) ) ) ) ) ) ) & ( ( ( ( p ) )-( ( ( h ) )-( ( bigA ) ) ) ) \/ ( ~( ( ( p ) )-( ( bigA ) ) ) ) ) ) ) & ( ( ( p ) )-( ( sk2 ) ) ) ) ) & ( ( ~( ( ( p ) )-( ( bigA ) ) ) ) \/ ( ~( ( ( l ) )-( ( sk2 ) )-( ( bigA ) ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN729-1.p').
checkTheorem(syn , 937 , [ type:set ,l : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , g : (( ( type ) ) -> ( ( prop ) )) , h : (( ( type ) ) -> ( ( prop ) )) , p : (( ( type ) ) -> ( ( prop ) )) ] , ( ( pi(X:type,sigma(Y:type,( ( ( p ) )-( ( bigX ) ) ) -> ( ( ( ( l ) )-( ( bigX ) )-( ( ( g ) )-( ( ( h ) )-( ( bigY ) ) ) ) ) & ( ( ( p ) )-( ( bigY ) ) ) ))) ) & ( pi(W:type,( ( ( p ) )-( ( bigW ) ) ) -> ( ( ( ( p ) )-( ( ( g ) )-( ( bigW ) ) ) ) & ( ( ( p ) )-( ( ( h ) )-( ( bigW ) ) ) ) )) ) ) -> ( pi(X:type,( ( ( p ) )-( ( bigX ) ) ) -> ( sigma(Y:type,( ( ( l ) )-( ( bigX ) )-( ( bigY ) ) ) & ( ( ( p ) )-( ( bigY ) ) )) )) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN729+1.p').
checkTheorem(syn , 938 , [ type:set ,k : (( ( type ) ) -> ( ( prop ) )) , p : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) , h : (( ( type ) ) -> ( ( prop ) )) , a : (( ( type ) ) -> ( ( prop ) )) ] , sigma(V:type,pi(J:type,sigma(Q:type,( ( ( ( p ) )-( ( a ) )-( ( bigJ ) )-( ( ( h ) )-( ( bigJ ) ) ) ) \/ ( ( ( p ) )-( ( bigV ) )-( ( bigJ ) )-( ( ( k ) )-( ( bigJ ) ) ) ) ) -> ( ( ( p ) )-( ( bigV ) )-( ( bigJ ) )-( ( bigQ ) ) )))) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN730+1.p').
checkTheorem(syn , 939 , [ type:set ,p : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) , sk1 : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , bigB : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) , bigA : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ( ~( ~( ( ( p ) )-( ( bigA ) )-( ( bigB ) )-( ( ( sk1 ) )-( ( bigB ) )-( ( bigA ) ) ) ) ) ) & ( ~( ( ( p ) )-( ( bigA ) )-( ( bigA ) )-( ( bigB ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN731-1.p').
checkTheorem(syn , 940 , [ type:set ,p : (( ( type ) ) -> ( ( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) ) )) ] , sigma(W:type,( pi(X:type,sigma(Y:type,( ( p ) )-( ( bigW ) )-( ( bigX ) )-( ( bigY ) ))) ) -> ( sigma(Z:type,( ( p ) )-( ( bigZ ) )-( ( bigZ ) )-( ( bigW ) )) )) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN731+1.p').

checkTheorem(syn , 941 , [ type:set ,q : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , p : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ( pi(Y:type,( pi(X:type,( ( p ) )-( ( bigX ) )-( ( bigY ) )) ) -> ( pi(U:type,( ( q ) )-( ( bigU ) )-( ( bigY ) )) )) ) -> ( sigma(Z:type,( sigma(V:type,( ( p ) )-( ( bigV ) )-( ( bigZ ) )) ) -> ( sigma(W:type,( ( ( p ) )-( ( bigZ ) )-( ( bigW ) ) ) \/ ( ( ( q ) )-( ( bigW ) )-( ( bigZ ) ) )) )) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN732+1.p').
