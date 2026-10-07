% Read by tests_prolog_import.sysl: a Prolog file whose own clauses define scaled/2, which the FunL
% file it imports already defines as the function scaled/1.

:- import("geometry.fl").

scaled(_, 0).
