% Read by tests_prolog_calls.sysl, consulted from FunL while the program runs.

greeting(hello).
greeting(world).

:- forall(greeting(G), (write(G), nl)).
