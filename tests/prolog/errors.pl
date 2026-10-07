% Read by tests_errors.sysl: Prolog catching what FunL code raises, and a Prolog error crossing FunL
% frames.
%
% The FunL program that imports this file defines the functions named here -- each one a predicate
% one argument longer, its last argument the function's value -- and through/1, a FunL function that
% calls boom/1.

% Run Goal, writing the formal term of the error it raises, or `none`.
try(Goal) :- catch((Goal, Formal = none), error(Formal, _), true), write(Formal), nl.

% Call each FunL function with arguments that make it raise.
divide_by_zero :- try(divide(1, 0, _)).
floor_divide_by_zero :- try(floor_divide(1, 0, _)).
overflow :- try(add(9223372036854775807, 1, _)).
inexact :- try(divide(7, 2, _)).
not_a_number :- try(subtract(1, a, _)).
unbound :- try(add(_, 1, _)).
pattern_unbound :- try(head(_, _)).
no_clause :- try(head([], _)).
index_out_of_range :- try(element([1, 2], 5, _)).
missing_key :- try(lookup(b, _)).
raised :- try(raise(boom, _)).
not_a_collection :- try(contains(3, 5, _)).
immutable :- try(store(x, _)).
wrong_count :- try(call_with_two(x, _)).
negative_size :- try(make_array(1, _)).
no_field :- try(field_z(point(3, 4), _)).
no_scan :- try(tab_outside(1, _)).

% A Prolog error thrown inside a FunL function and caught out here, the FunL frames between unwound.
boom(_) :- throw(deep).
crossing(R) :- catch(through(1, R), E, R = caught(E)).

% A ball nothing in this file catches, for the FunL program to see uncaught.
escape(X) :- through(X, _).
