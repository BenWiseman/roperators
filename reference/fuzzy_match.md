# Fuzzy (case- and whitespace-insensitive) string equality

The string counterpart to the numeric operator `%~=%` (see
[`floating_point_comparisons`](https://benwiseman.github.io/roperators/reference/floating_point_comparisons.md)).
`x %~% y` is `TRUE` when `x` and `y` are equal ignoring case,
leading/trailing whitespace, and runs of internal whitespace - handy for
joining or matching messy, hand-entered data.

## Usage

``` r
x %~% y
```

## Arguments

- x:

  a character vector

- y:

  a character vector

## Value

A logical vector, with `NA` where either side is `NA`.

## Author

Ben Wiseman, <benjamin.h.wiseman@gmail.com>

## Examples

``` r
"Foo " %~% "foo"                   # TRUE
#> [1] TRUE
"a  b" %~% "a b"                    # TRUE
#> [1] TRUE
c("Yes", "NO") %~% c("yes", "no")  # TRUE TRUE
#> [1] TRUE TRUE
```
