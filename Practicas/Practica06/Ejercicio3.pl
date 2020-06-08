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



%3.4 Nueva Lista%
nuevaLista([],[]).
nuevaLista([X,X],[c(X,2)]).
nuevaLista([X],[c(X,1)]).
nuevaLista([X|XS],[(c(L,N))|YS]) :-
    L = X ,N \= 1 ->
    N1 is N-1,
    nuevaLista(XS,[(c(L,N1))|YS]);
    nuevaLista(XS,YS).