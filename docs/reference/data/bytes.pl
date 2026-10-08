kinds(X, Kinds) :-
    findall(K, ( member(K, [atom, number, compound, callable, atomic]), G =.. [K, X], call(G) ), Kinds).

same(X, X).

order(X, Y, Order) :- compare(Order, X, Y).

sorted(List, Sorted) :- msort(List, Sorted).
