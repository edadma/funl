% Read by tests_prolog_import.sysl: a Prolog file importing a FunL file and calling what it defines.
%
% geometry.fl defines the relation side/2, the generating function upto/1 (upto/2 here), and the
% functions scaled/1 and halve/1 (scaled/2 and halve/2 here); halve/1 raises on zero.

:- import("geometry.fl").

sides(S, N) :- side(S, N).
counted(N, L) :- findall(K, upto(N, K), L).
area(N, A) :- scaled(N, A).
guarded(X, R) :- catch(halve(X, R), error(E, _), R = caught(E)).
