% Read by tests_prolog_consult.sysl: imported by a FunL program that defines shared/1, which
% clash_a.pl defines too, so consulting clash_a.pl is refused.

:- catch(consult("tests/prolog/clash_a.pl"), error(E, _), (write(E), nl)).
