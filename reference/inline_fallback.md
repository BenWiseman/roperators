# Inline fallback for expressions that might error

Evaluate the left-hand side; if it raises an error, return the
right-hand side instead. The right-hand side is only evaluated when
needed (lazily), so an expensive or side-effecting fallback is safe to
use.

## Usage

``` r
expr %else% alternative
```

## Arguments

- expr:

  an expression to try

- alternative:

  value (or expression) to return if `expr` errors

## Value

The value of `expr`, or of `alternative` if `expr` raised an error.

## Author

Ben Wiseman, <benjamin.h.wiseman@gmail.com>

## Examples

``` r
sqrt(4)         %else% NA_real_     # 2
#> [1] 2
sqrt("a")       %else% NA_real_     # NA, instead of an error
#> [1] NA
(1:3)[[99]]     %else% "out of range"
#> [1] "out of range"
stop("boom")    %else% "recovered"
#> [1] "recovered"
```
