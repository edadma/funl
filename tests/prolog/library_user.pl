% Read by tests_prolog_namespace.sysl: clauses calling a library predicate and a builtin, which the
% FunL program importing this file defines functions of the same names beside.

rev(L, R) :- reverse(L, R).

srt(L, S) :- sort(L, S).
