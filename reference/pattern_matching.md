# SQL-style pattern-matching operators

Convenience operators for regular-expression matching, inspired by SQL's
`LIKE`. Each takes a character vector `x` and a single pattern, and
returns a logical vector the same length as `x`.

`%rlike%` matches case-insensitively, equivalent to
`grepl(pattern, x, ignore.case = TRUE)`.

`%perl%` matches case-sensitively using Perl-compatible regular
expressions, equivalent to `grepl(pattern, x, perl = TRUE)`.

## Usage

``` r
x %rlike% pattern

x %perl% pattern
```

## Arguments

- x:

  a character vector

- pattern:

  a single character expression (regular expression)

## Value

A logical vector the same length as `x`.

## Note

If you are working with `data.table`, prefer its own (faster) `%like%`
operator.

## Author

Ben Wiseman, <benjamin.h.wiseman@gmail.com>

## Examples

``` r
x <- c("foo", "bar", "dOe", "rei", "mei", "obo")

# case-insensitive: where x contains an "o" (any case)
x[x %rlike% "O"]
#> [1] "foo" "dOe" "obo"
# [1] "foo" "dOe" "obo"

# case-sensitive Perl matching: middle letter is an upper-case "O"
x[x %perl% "[a-z]O[a-z]"]
#> [1] "dOe"
# [1] "dOe"
```
