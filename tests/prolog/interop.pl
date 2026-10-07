% Read by tests_prolog_calls.sysl: Prolog clauses calling FunL, and called from FunL.
%
% par/2 is a FunL relation and double/1 a FunL function -- double/2 here, its last argument the
% function's value -- both defined by the FunL program that imports this file.

grand(X, Z) :- par(X, Y), par(Y, Z).

twice_of(X, Y) :- double(X, Y).

either(X) :- X = left ; X = right.

first(X) :- either(X), !.

% Indexed on its first argument, so each step leaves no choice point and the walk runs in constant
% control space however long the list.
walk([], done).
walk([_|T], R) :- walk(T, R).

% Writes whatever FunL hands it, for tests_prolog_write.sysl to see how a FunL value is written.
show_term(X) :- writeq(X), nl.

:- write(loaded), nl.
