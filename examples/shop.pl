% FunL and Prolog sharing predicates, the Prolog half: run it with `funl examples/shop.pl`.
%
% `:- import` loads shop.funl, whose path is relative to this file. Its top level runs as it
% loads, printing the catalogue, and its relations and functions then answer as predicates here:
% price/2 and unit/2 are FunL relations, euros/2 is the one-parameter FunL function `euros`, and
% sizes/1 is a FunL generator, with one solution per value it produces.

:- import("shop.funl").
:- initialization(main).

% Prolog rules over the FunL facts.
affordable(Item, Budget) :- price(Item, P), P =< Budget.

basket_total([], 0).
basket_total([Item|Items], Total) :-
    price(Item, P),
    basket_total(Items, Rest),
    Total is P + Rest.

main :-
    findall(I, affordable(I, 250), Cheap),
    write(Cheap), nl,                         % [apple,bread,grapes]
    basket_total([apple, bread, grapes], T),
    euros(T, E),
    write(E), nl,                             % 11r2 -- FunL's exact 11/2, in Prolog's spelling
    findall(S, sizes(S), Sizes),
    write(Sizes), nl,                         % [small,large]
    forall((price(X, _), unit(X, kilo)),      % unit/2 commits in its `if`, so X is bound first
           (write(X), nl)).                   % apple, grapes
