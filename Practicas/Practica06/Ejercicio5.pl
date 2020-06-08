%Automatas%
%5.1%
inicial(q0).
final(q3).

delta(q0,a,q1).
delta(q1,b,q2).
delta(q2,c,q1).
delta(q2,d,q3).
delta(q3,c,q0).

afd(X) :-
    inicial(q0),
    trancision(q0,XS).

trancision(q3,[]) :-
    final(q3).
trancision(Q,[X|XS]) :- 
    delta(Q,X,Qi),
    trancision(Qi,XS).

%5.2 afnd%
inicial1(1).
final1(3).

delta1(1,b,2).
delta1(2,a,2).
delta1(2,b,3).
delta1(3,a,2).

afnd(X) :-
    inicial1(1),
    trancision1(1,XS).

trancision1(3,[]) :-
    final1(q3).
trancision1(Q,[X|XS]) :- 
    delta1(Q,X,Qi),
    trancision1(Qi,XS).
