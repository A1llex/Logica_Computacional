%Simples%
%2.1%
pedro.
estudiar(pedro).
duerme(pedro).
trampa(pedro).

aprobar(X,examen):- estudiar(X),duerme(X);trampa(X).

%2.2%

pedir_matrimonio(daniel,sofia).

casara(X,Y):-pedir_matrimonio(X,Y).

%2.3%
quieren_comer(gatitos).
estan_alegres(gatitos).
estan_en_celo(gatitos).

maullan(X,maullan):-
    quieren_comer(X);
    estan_alegres(X);
    estan_en_celo(X).

%2Reglas Recursivas%
%2.1 Naturales%
natural(0).
natural(X) :- 
    X > 0,
    X1 is X-1,
    natural(X1).

%2.2 Fibonacci%
fibonacci(0,1).
fibonacci(1,1).
fibonacci(N,X) :-
    N > 1,
    N1 is N-1,
    N2 is N-2,
    fibonacci(N1,Y),
    fibonacci(N2,Z),
    X is Y+Z. 

%2.3 Suma naturales%
suma(0,X,X).
suma(X,0,X).
suma(X,Y,Z) :-
    natural(X),
    natural(Y),
    X1 is X-1,
    Y1 is Y+1,
    suma(X1,Y1,Z).

%2.4 potencia%
%Multiplicacion%
multiplicacion(1,X,X).
multiplicacion(X,Y,Z) :-
    natural(X),
    natural(Y),
    X1 is X-1,
    multiplicacion(X1, Y, Z1),
    suma(Z1,Y,Z).
%Potencia%
potencia(_,0,1).
potencia(X,1,X).
potencia(X,Y,Z) :-
    natural(X),
    natural(Y),
    Y1 is Y-1,
    potencia(X,Y1,Z1),
    multiplicacion(X,Z1,Z).
