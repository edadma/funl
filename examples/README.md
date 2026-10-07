# FunL examples

Short programs for people to read. Build the interpreters with `sysl build -p funl` and
`sysl build -p prolog` at the repository root, then run `./funl/funl examples/<file>.funl`; the
comments in each file say what it prints.

| file | shows |
|---|---|
| `generators.funl` | goal-directed evaluation: success and failure, alternation, `every`, generator functions, backtracking into an expression |
| `string_scanning.funl` | `s ? e` with `tab`, `move`, `upto`, `many`, `find`, and a word generator built from a scan |
| `regex.funl` | regex literals as matchers in a scan: forward matching, lookahead, unbounded lookbehind, combinators |
| `data_and_patterns.funl` | `data` records and sum types, clauses with patterns and guards, a binary search tree, partial function literals |
| `numbers.funl` | the numeric tower: big integers, exact rationals (`1/3 + 1/6`), reals, Newton's method in fractions |
| `functions.funl` | guards and `where`, lambdas and closures, operator sections, currying |
| `word_count.funl` | a scanning generator feeding a mutable map |
| `relations.funl` | logic programming in FunL: facts, rules, `free`, `~`, `findall`, negation, functions inside rules |
| `map_colouring.funl` | a map-colouring puzzle stated as a relation and solved by its backtracking |
| `n_queens.funl` | N-queens with a generator, chained comparisons and reversible assignment `<-` |
| `send_more_money.funl` | SEND + MORE = MONEY by backtracking through a digit generator |
| `shop.funl` | the FunL half of a shared program: relations and functions that Prolog uses as predicates |
| `shop.pl` | the Prolog half: `:- import("shop.funl")`, run with `./funl/funl examples/shop.pl` |
| `calculator.pl` | standard Prolog with no FunL in it (a DCG, `catch/3`), run with `./prolog/prolog examples/calculator.pl` |
| `family_tree.funl`, `family_tree.pl` | the family tree in FunL and in Prolog |
