% Standard Prolog with no FunL in it: run it with `prolog examples/calculator.pl`.
%
% A small calculator. A DCG turns a list of character codes into an expression tree, honouring
% precedence and left associativity; eval/2 walks the tree; and catch/3 turns a division by zero,
% raised as an ISO error term, into a message. initialization(main) runs once the file is loaded.

:- initialization(main).

% expr --> term, { + or - term }*, built left to right through an accumulator.
expr(E) --> term(T), expr_rest(T, E).
expr_rest(Acc, E) --> "+", !, term(T), expr_rest(Acc + T, E).
expr_rest(Acc, E) --> "-", !, term(T), expr_rest(Acc - T, E).
expr_rest(E, E) --> [].

term(E) --> factor(F), term_rest(F, E).
term_rest(Acc, E) --> "*", !, factor(F), term_rest(Acc * F, E).
term_rest(Acc, E) --> "/", !, factor(F), term_rest(Acc / F, E).
term_rest(E, E) --> [].

factor(E) --> "(", !, expr(E), ")".
factor(N) --> digits(Ds), { Ds \== [], number_codes(N, Ds) }.

digits([D|Ds]) --> [D], { D >= 0'0, D =< 0'9 }, !, digits(Ds).
digits([]) --> [].

eval(N, N) :- number(N).
eval(A + B, V) :- eval(A, X), eval(B, Y), V is X + Y.
eval(A - B, V) :- eval(A, X), eval(B, Y), V is X - Y.
eval(A * B, V) :- eval(A, X), eval(B, Y), V is X * Y.
eval(A / B, V) :- eval(A, X), eval(B, Y), V is X // Y.

calc(Text) :-
    atom_codes(Text, Codes),
    (   phrase(expr(Tree), Codes)
    ->  catch(( eval(Tree, V), write(Text = V), nl ),
              error(evaluation_error(zero_divisor), _),
              ( write(Text), write(': division by zero'), nl ))
    ;   write(Text), write(': not an expression'), nl
    ).

main :-
    calc('1+2*3'),            % 1+2*3=7
    calc('(1+2)*3'),          % (1+2)*3=9
    calc('20-5-3'),           % 20-5-3=12 -- left associative
    calc('100/7'),            % 100/7=14 -- integer division
    calc('2*(3+4)/0'),        % 2*(3+4)/0: division by zero
    calc('2+*3'),             % 2+*3: not an expression
    calc('2^100'),            % 2^100: not an expression
    X is 2 ^ 100,
    write(X), nl.             % 1267650600228229401496703205376 -- integers are unbounded
