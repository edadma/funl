% Read by tests_prolog_calls.sysl: a Prolog program and the FunL program importing it calling each
% other in both directions.
%
% The FunL program defines the relation edge/2, the generating function upto/1 (upto/2 here, each
% value it generates one solution), halve/1 (halve/2 here, which raises on zero) and the relation
% through/1, which calls boom/1 below.

% Calls a FunL relation twice, backtracking into it for every path of two edges.
reach(X, Z) :- edge(X, Y), edge(Y, Z).

% Backtracks into a FunL generator: one solution for each value upto/1 gives.
multiples(N, M) :- upto(N, K), M is K * 10.

% A cut that commits this predicate and nothing of whoever called it.
first_multiple(N, M) :- multiples(N, M), !.

% Called from a FunL relation, which backtracks into it.
small(X) :- member(X, [1, 2, 3]).

% An error the FunL function raises, caught here.
guarded(X, R) :- catch(halve(X, R), error(E, _), R = caught(E)).

% A ball thrown here, crossing the FunL relation through/1 on its way back to the catch.
boom(_) :- throw(oops(deep)).
relay(R) :- catch(through(R), oops(X), R = got(X)).
