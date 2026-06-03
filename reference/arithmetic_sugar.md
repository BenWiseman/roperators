# Arithmetic convenience operators

`%+-%` builds a tolerance interval: `x %+-% y` returns
`c(x - y, x + y)`, which slots straight into the "between" operators in
[`comparisons`](https://benwiseman.github.io/roperators/reference/comparisons.md).
`%/0%` is a safe division that returns `NA` instead of `Inf`/`NaN` when
dividing by zero, so a stray zero will not poison a downstream sum or
mean.

## Usage

``` r
x %+-% y

x %/0% y
```

## Arguments

- x:

  a numeric value (or vector)

- y:

  a numeric value (or vector)

## Value

`%+-%` returns a length-2 numeric vector `c(lower, upper)`; `%/0%`
returns `x / y` with any divide-by-zero results replaced by `NA`.

## Author

Ben Wiseman, <benjamin.h.wiseman@gmail.com>

## Examples

``` r
5 %+-% 0.5             # 4.5 5.5
#> [1] 4.5 5.5
4.9 %><% (5 %+-% 0.5)  # TRUE  - composes with the 'between' operator
#> [1] TRUE

10 %/0% 2                    # 5
#> [1] 5
10 %/0% 0                    # NA (not Inf)
#> [1] NA
c(1, 2, 3) %/0% c(1, 0, 3)   # 1 NA 1
#> [1]  1 NA  1
```
