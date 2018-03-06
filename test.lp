#const a = 1.

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

0 { does(P, A) : p(A) } 1 :- legal(P, A).
:- role(P), not does(P, _), not terminal.
:- does(P, A1), does(P, A2), A1 < A2.

:~ goal(W, 100).[1@2, 1]

#show line/1.

#pos(ex_2_4_3,
  {}, {},
  {
    does(x, mark(2,3)).
    init(cell(1,1,x)).
    init(cell(1,2,x)).
    init(cell(1,3,b)).
    init(cell(2,1,o)).
    init(cell(2,2,b)).
    init(cell(2,3,b)).
    init(cell(3,1,x)).
    init(cell(3,2,o)).
    init(cell(3,3,o)).
    init(control(x)).
  }
).

#neg(ex_bad_1,
  {
    does(x, noop)
  }, {},
  {
    init(cell(1,1,x)).
    init(cell(1,2,b)).
    init(cell(1,3,b)).
    init(cell(2,1,o)).
    init(cell(2,2,b)).
    init(cell(2,3,b)).
    init(cell(3,1,x)).
    init(cell(3,2,o)).
    init(cell(3,3,b)).
    init(control(x)).
  }
).

#brave_ordering(ord_4_3,ex_1_4_2,ex_2_2_3).

#deep_ordering(d_ord_1, ex_1, ex_2, 3).

#child(ex_1, ex_1_1).

#modeh(legal(var(p), mark(var(row), var(column)))).
#modeh(legal(var(p), noop)).
#modeb(1, init(cell(var(row), var(column), const(state)))).
#modeb(1, init(control(var(p)))).
#modeb(terminal).
#modeb(1, role(var(p))).

#modeo(1, goal(const(role), const(reward)), (positive)).
#constant(role, x).
% #constant(role, o).
% #constant(state, x).
% #constant(state, o).
% #constant(state, b).
#constant(reward, 100).
% #constant(reward, 50).
% #constant(reward, 0).
#weight(-1).
% #weight(1).

#maxp(1).

#maxv(4).
