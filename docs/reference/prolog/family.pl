% The family the Prolog reference page loads from FunL.

parent(tom, bob).
parent(tom, liz).
parent(bob, ann).
parent(bob, pat).

grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
