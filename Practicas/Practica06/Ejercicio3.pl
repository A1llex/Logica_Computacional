%Ejercicios 3 Listas%

%3.1 Pertenece%
pertenece(X,[X|_],1).
pertenece(X,[_|XS],N) :-
    N1 is N-1,
    pertenece(X,XS,N1).


%3.2 Prefijo%
prefijo([],_).
prefijo([X|XS],[X|YS]) :-
    prefijo(XS,YS).

%3.3 Suma Acumulada%

sumaAcumulada([],_).
sumaAcumulada([X|XS],[]) :-
    suml().

%auxiliar% 
suml([],_,_).
suml([X|XS],[YS],S) :-
    S1 is X+S,
    suml([XS],[YS,S1],S1).