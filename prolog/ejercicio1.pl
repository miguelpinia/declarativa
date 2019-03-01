enc([],[]) :- !.
enc([], _).
enc([X], [[X]]) :- X \== [], !.
enc([X, Y | R], [[X|Xs] | R1]) :-
    X = Y,
    enc([Y|R], [Xs | R1]).
enc([X, Y], [[X],[Y]]) :-
    X \== Y, !.
enc([X, Y | R], [[X],[Y|Ys]|R1]) :-
    X \== Y,
    enc(R, [Ys|R1]).
