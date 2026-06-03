# Vector content checks

Quick checks for what a vector contains. `is.constant()` is `TRUE` when
`x` holds at most one unique value (ignoring `NA`), and `is.binary()` is
`TRUE` when it holds at most two.

## Usage

``` r
is.constant(x)

is.binary(x)
```

## Arguments

- x:

  object to be tested

## Value

A logical value.

## Examples

``` r
is.constant(c(1, 1, 1))     # TRUE
#> [1] TRUE
is.constant(c(1, 2, 1))     # FALSE
#> [1] FALSE
is.binary(c("a", "b", NA))  # TRUE
#> [1] TRUE
is.binary(c("a", "b", "c")) # FALSE
#> [1] FALSE
```
