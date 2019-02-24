%% member(X, [X|Tail]).
%% member(X, [Head|Tail]) :- member(X, Tail).
conc([], L, L).
conc(L, [], L).
conc([X|L1], L2, [X|L3]) :- conc(L1, L2, L3).
add([], L, L).
add(L, [], L).
add(X, L, [X|L]).

del(X, [X|Tail], Tail).
del(_, [], []).
del(X, [Y|Tail], [Y|NewTail]) :- del(X, Tail, NewTail).

insert(X, L, BL) :- del(X, BL, L), BL \== L.
sublist(S, L) :- conc(_, L2, L), cnc(S, _, L2).


perm([], []).
perm([X|L], P) :- perm(L, L1), insert(X, L1, P).
