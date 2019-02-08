parent(pam, bob).
parent(tom, bob).
parent(tom, liz).
parent(bob, ann).
parent(bob, pat).
parent(pat, jim).

male(tom).
male(bob).
male(jim).
female(liz).
female(ann).



female(pat).
female(pam).

offspring(Y, X) :- parent( X, Y).

mother(X, Y) :- parent(X, Y),
                female(X).


grandparent(X, Z) :- parent(X, Y),
                     parent(Y, Z).

sister( X, Y) :- parent(Z, X),
                 parent(Z, Y),
                 female(X),
                 different(X, Y).

predecessor(X, Z) :- parent( X, Z).
predecessor( X, Z) :- parent( X, Y),
                      predecessor(Y, Z).

hasachild(X) :- parent(X, _).
