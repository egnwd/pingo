% Background
role(x).
role(o).

next(cell(M, N, P)) :- does(P, mark(M, N)), init(cell(M, N, b)).
next(cell(M, N, P)) :- init(cell(M, N, P)), P != b.
next(cell(M1, N1, b)) :- does(P, mark(M2, N2)), init(cell(M1, N1, b)), M1 != M2.
next(cell(M1, N1, b)) :- does(P, mark(M2, N2)), init(cell(M1, N1, b)), N1 != N2.

next(control(x)) :- init(control(o)).
next(control(o)) :- init(control(x)).

row(M, P) :- role(P), next(cell(M, 1, P)), next(cell(M, 2, P)), next(cell(M, 3, P)).
column(N, P) :- role(P), next(cell(1, N, P)), next(cell(2, N, P)), next(cell(3, N, P)).
diagonal(P) :- role(P), next(cell(1, 1, P)), next(cell(2, 2, P)), next(cell(3, 3, P)).
diagonal(P) :- role(P), next(cell(1, 3, P)), next(cell(2, 2, P)), next(cell(3, 1, P)).

legal(V2, mark(V0, V1)) :- init(cell(V0, V1, b)), init(control(V2)), not terminal.
legal(V0, noop) :- not init(control(V0)), role(V0).

line(P) :- row(_, P).
line(P) :- column(_, P).
line(P) :- diagonal(P).

open :- init(cell(_, _, b)).

goal(P, 100) :- line(P).
goal(P, 50) :- not line(x), not line(o), not open, role(P).
goal(P1, 0) :- role(P1), role(P2), line(P2), P1 != P2.

terminal :- line(_).
terminal :- not open.
