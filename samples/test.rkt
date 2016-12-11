#lang racketlog

id.

father(p, o).
mother(a, o).

father(p, f).
mother(a, f).

father(pp, d).
mother(a, d).

father(p, oo).
mother(aa, oo).

couple(X, Y) :-
  father(X, Z),
  mother(Y, Z).

?couple(X, Y).
