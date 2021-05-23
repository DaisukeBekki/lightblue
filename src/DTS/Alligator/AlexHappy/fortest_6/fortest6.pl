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
checkTheorem(syn , 36 , [ type:set ,q : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , p : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , f : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , bigY : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) , bigX : (( ( type ) ) -> ( ( ( type ) ) -> ( ( prop ) ) )) ] , ( ~( ( ~( ( ~( ( ~( ( ~( ( ~( ~( ( ~( ( ( p ) )-( ( bigX ) )-( ( ( f ) )-( ( bigX ) )-( ( bigY ) ) ) ) ) \/ ( ( ~( ( ( p ) )-( ( bigX ) )-( ( bigY ) ) ) ) \/ ( ( ( q ) )-( ( bigY ) )-( ( ( f ) )-( ( bigX ) )-( ( bigY ) ) ) ) ) ) ) ) & ( ( ~( ( ( p ) )-( ( bigX ) )-( ( bigY ) ) ) ) \/ ( ( ~( ( ( q ) )-( ( bigY ) )-( ( ( f ) )-( ( bigX ) )-( ( bigY ) ) ) ) ) \/ ( ( ( p ) )-( ( bigX ) )-( ( ( f ) )-( ( bigX ) )-( ( bigY ) ) ) ) ) ) ) ) & ( ( ~( ( ( p ) )-( ( bigX ) )-( ( bigY ) ) ) ) \/ ( ( ~( ( ( p ) )-( ( ( f ) )-( ( bigX ) )-( ( bigY ) ) )-( ( ( f ) )-( ( bigX ) )-( ( bigY ) ) ) ) ) \/ ( ( ( q ) )-( ( ( f ) )-( ( bigX ) )-( ( bigY ) ) )-( ( ( f ) )-( ( bigX ) )-( ( bigY ) ) ) ) ) ) ) ) & ( ( ( ( p ) )-( ( bigX ) )-( ( bigY ) ) ) \/ ( ( ( p ) )-( ( ( f ) )-( ( bigX ) )-( ( bigY ) ) )-( ( ( f ) )-( ( bigX ) )-( ( bigY ) ) ) ) ) ) ) & ( ( ( ( p ) )-( ( bigX ) )-( ( bigY ) ) ) \/ ( ~( ( ( q ) )-( ( ( f ) )-( ( bigX ) )-( ( bigY ) ) )-( ( ( f ) )-( ( bigX ) )-( ( bigY ) ) ) ) ) ) ) ) & ( ( ( ( q ) )-( ( bigX ) )-( ( bigY ) ) ) \/ ( ( ( q ) )-( ( ( f ) )-( ( bigX ) )-( ( bigY ) ) )-( ( ( f ) )-( ( bigX ) )-( ( bigY ) ) ) ) ) ) ) & ( ( ~( ( ( q ) )-( ( bigX ) )-( ( bigY ) ) ) ) \/ ( ~( ( ( q ) )-( ( ( f ) )-( ( bigX ) )-( ( bigY ) ) )-( ( ( f ) )-( ( bigX ) )-( ( bigY ) ) ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN038-1.p').

checkTheorem(syn , 37 , [ type:set ,q : (( type )) , p : (( type )) ] , ( ~( ( ~( ( ~( ~( ( ~( ( p ) ) ) \/ ( ( q ) ) ) ) ) & ( ( ~( ( q ) ) ) \/ ( ( p ) ) ) ) ) & ( ~( ( q ) ) ) ) ) & ( ( p ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN040-1.p').
checkTheorem(syn , 38 , [ type:set ,q : (( type )) , p : (( type )) ] , ( ( ( ( p ) ) -> ( ( q ) ) ) -> ( ( ~( ( q ) ) ) -> ( ~( ( p ) ) ) ) ) & ( ( ( ~( ( q ) ) ) -> ( ~( ( p ) ) ) ) -> ( ( ( p ) ) -> ( ( q ) ) ) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN040+1.p').

checkTheorem(syn , 39 , [ type:set ,q : (( type )) , p : (( type )) ] , ( ~( ( ~( ( ~( ~( ( p ) ) ) ) & ( ~( ( q ) ) ) ) ) & ( ( q ) ) ) ) & ( ~( ( p ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN041-1.p').
checkTheorem(syn , 40 , [ type:set ,q : (( type )) , p : (( type )) ] , ( ~( ( ( p ) ) -> ( ( q ) ) ) ) -> ( ( ( q ) ) -> ( ( p ) ) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN041+1.p').

checkTheorem(syn , 41 , [ type:set ,p : (( type )) , r : (( type )) , q : (( type )) , axiom3 : (( ~( ( q ) ) ) \/ ( ( r ) )) , axiom2 : (( ~( ( r ) ) ) \/ ( ( p ) )) , axiom1 : (( ~( ( r ) ) ) \/ ( ( q ) )) , axiom0 : (( ~( ( p ) ) ) \/ ( ( ( q ) ) \/ ( ( r ) ) )) ] , ( ~( ~( ( ~( ( p ) ) ) \/ ( ~( ( q ) ) ) ) ) ) & ( ( ( p ) ) \/ ( ( q ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN044-1.p').
