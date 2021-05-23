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
checkTheorem(syn , 42 , [ type:set ,p : (( type )) , r : (( type )) , q : (( type )) , axiom2 : (( ( q ) ) -> ( ( r ) )) , axiom1 : (( ( r ) ) -> ( ( ( p ) ) & ( ( q ) ) )) , axiom0 : (( ( p ) ) -> ( ( ( q ) ) \/ ( ( r ) ) )) ] , ( ( ( p ) ) -> ( ( q ) ) ) & ( ( ( q ) ) -> ( ( p ) ) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN044+1.p').

checkTheorem(syn , 43 , [ type:set ,r : (( type )) , q : (( type )) , p : (( type )) ] , ( ~( ( ~( ( ~( ~( ( ( p ) ) \/ ( ( q ) ) ) ) ) & ( ( ( p ) ) \/ ( ( r ) ) ) ) ) & ( ~( ( p ) ) ) ) ) & ( ( ~( ( q ) ) ) \/ ( ~( ( r ) ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN045-1.p').
checkTheorem(syn , 44 , [ type:set ,r : (( type )) , q : (( type )) , p : (( type )) ] , ( ( ( ( p ) ) \/ ( ( ( q ) ) & ( ( r ) ) ) ) -> ( ( ( ( p ) ) \/ ( ( q ) ) ) & ( ( ( p ) ) \/ ( ( r ) ) ) ) ) & ( ( ( ( ( p ) ) \/ ( ( q ) ) ) & ( ( ( p ) ) \/ ( ( r ) ) ) ) -> ( ( ( p ) ) \/ ( ( ( q ) ) & ( ( r ) ) ) ) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN045+1.p').


checkTheorem(syn , 45 , [ type:set ,q : (( type )) , p : (( type )) ] , ( ~( ( ~( ~( ( ~( ( p ) ) ) \/ ( ( q ) ) ) ) ) & ( ( p ) ) ) ) & ( ~( ( q ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN046-1.p').
checkTheorem(syn , 46 , [ type:set ,q : (( type )) , p : (( type )) ] , ( ( ( ( p ) ) -> ( ( q ) ) ) -> ( ( ~( ( p ) ) ) \/ ( ( q ) ) ) ) & ( ( ( ~( ( p ) ) ) \/ ( ( q ) ) ) -> ( ( ( p ) ) -> ( ( q ) ) ) ) , yes , '../../TPTP-v7.3.0/Problems/SYN/SYN046+1.p').

checkTheorem(syn , 47 , [ type:set ,r : (( type )) , s : (( type )) , q : (( type )) , p : (( type )) ] , ( ~( ( ~( ( ~( ( ~( ~( ( p ) ) ) ) & ( ( ~( ( p ) ) ) \/ ( ( ( q ) ) \/ ( ( s ) ) ) ) ) ) & ( ( ~( ( p ) ) ) \/ ( ( ~( ( r ) ) ) \/ ( ( s ) ) ) ) ) ) & ( ~( ( s ) ) ) ) ) & ( ( ~( ( q ) ) ) \/ ( ( r ) ) ) , no , '../../TPTP-v7.3.0/Problems/SYN/SYN047-1.p').
