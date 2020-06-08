%Operador de corte%
%4.1%
pot(_,0,1).
pot(X,1,X).
pot(X,s(Y),Z) :-
    pot(X,Y,Z1),
    Z is X *Y1.

%4.2 Factorial%
fac(0,1).
fac(X,s(Y)):-
    X1 is X*Y,
    fac(X1,Y).