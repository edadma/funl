% A file the standalone Prolog's top level consults in `tests_prolog_toplevel.sysl`.

parent(tom, bob).
parent(tom, liz).
parent(bob, ann).

grandparent(X, Z) :- parent(X, Y), parent(Y, Z).

:- write(loaded), nl.
