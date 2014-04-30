## Laziness static analysis tool

Examples from ESOP2013 paper, in `examples-esop2013` directory:

### Run these files to see effects of different amounts of laziness:

* `nqueens-strict.rkt`: slow

n-queens with no laziness

* `nqueens-lazy-naive.rkt`: slower

n-queens with naive laziness, ie eager cons replaced with lcons, where:

    (lcons x y) = (cons x (lazy y))

* `nqueens-lazy-proper.rkt`: fast

n-queens with lazy lists + lazy foldr

### Analyze this file:

`nqueens-lazy-naive-analyzable`:

* same as `nqueens-lazy-naive.rkt`, but for analyzing (ie requires and macros commented-out)
* result is equivalent to `nqueens-lazy-proper.rkt`