f(X, 0) :- X < 3.
f(X, 2) :- 3 =< X, X < 6.
f(X, 4) :- 6 =< X.

f1(X, 0) :- X < 3, !.
f1(X, 2) :- 3 =< X, X < 6, !.
f1(X, 4) :- 6 =< X.

f2(X, 0) :- X < 3, !.
f2(X, 2) :- X < 6, !.
f2(_, 4).

max(X, Y, X) :- X >= Y, !.
max(_, Y, Y).

%% member(X, [X|_]) :- !.
%% member(X, [_|L]) :- member(X, L).

%% member2(X, [X|_]).
%% member2(X, [_|L]) :- member(X, L).

snake(lucifer).
animal(lucifer).
animal(dog).
animal(cat).

likes(mary, X) :- snake(X), !, fail.
likes(mary, X) :- animal(X).

likes2(mary, X) :- snake(X), !, fail;
                   animal(X).

different(X, X) :- !, fail.
different(_, _).

different2(X, Y) :- X = Y, !, fail;
                    true.

not(P) :- P, !, fail; true.

likes3(mary, X) :- animal(X), not(snake(X)).
different3(X, Y) :- not(X = Y).

family(person(tom, fox, date(7, may, 1990), work(bbc, 152)),
       person(ann, fox, date(9, may, 1991), unemployed),
      [person(pat, fox, date(5, may, 2013), unemployed),
       person(jim, fox, date(5, may, 2013), unemployed)]).

family(person(andrew, fox, date(12, jul, 1990), work(bbc, 500)),
       person(mary, fox, date(16, oct, 1993), unemployed),
       [person(john, fox, date(12, apr, 2015), unemployed),
        person(patrick, fox, date(5, may, 2013), unemployed)]).

family(person(mike, armstrong, date(10, jul, 1990), work(cnn, 152)),
       person(olga, armstrong, date(9, oct, 1992), work(cnn, 152)),
       [person(jane, armstrong, date(4, dec, 2013), unemployed)]).


lfam([X, Y| Z]) :- family(X, Y, Z).

hassurname(person(_, Surname, _, _), Surname).
lfam2([X, Y| Z], Surname) :- family(X, Y, Z), hassurname(X, Surname).

husband(X) :- family(X, _, _).
wife(X) :- family(_, X, _).
child(X) :- family(_, _, Children), member2(X, Children).
exists(Person) :- husband(Person); wife(Person); child(Person).

promedio(N, Prom, Surname) :- lfam2(L, Surname),
                              total(L, S),
                              length(L, N),
                              Prom is S / N,
                              Prom < 200.

salary(person(_, _, _, work(_, S)), S).
salary(person(_, _, _, unemployed), 0).
total([], 0).
total([P|R], Sum) :- salary(P, S), total(R, Rest), Sum is S + Rest.


persons(Name, LastName) :- family(person(Name, LastName, _, _), _, _).
persons(Name, LastName) :- family(_, person(Name, LastName, _, _), _).
persons(Name, LastName) :- family(_, _, Children), member(person(Name, LastName, _, _), Children).



dateofbirth( person(_, _, Date, _), Date).
dateandsurname(person(_, Surname, Date, _), Surname, Date).

hola :- writeln('¿Cuál es tu nombre? '), read(X), write('Hola'), tab(1), write(X).
cube :- write('Siguiente elemento por favor: '), read(X), process(X).
process(stop) :- !.
process(N) :- C is N * N * N, write('El cubo de '), write(N), write(' es '), write(C), nl, cube.


fast(ann).
slow(tom).
slow(pat).
%% assert((faster(X, Y) :- fast(X), slow(Y))).
%% assert(p(a)), assertz(p(b)), asserta(p(c)).
%% retract(p(a)).

maketable :- L = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
             member(X, L), member(Y, L),
             Z is X * Y,
             assert(product(X, Y, Z)),
             false.
