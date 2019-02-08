member(X, [X|Tail]).
member(X, [Head|Tail]) :- member(X, Tail).
conc([], L, L).
conc(L, [], L).
conc([X|L1], L2, [X|L3]) :- conc(L1, L2, L3).
add([], L, L).
add(L, [], L).
add(X, L, [X|L]).

del(X, [X|Tail], Tail).
del(X, [], []).
del(X, [Y|Tail], [Y|NewTail]) :- del(X, Tail, NewTail).


sublist(S, L) :- conc(_, L2, L), conc(S, _, L2).
