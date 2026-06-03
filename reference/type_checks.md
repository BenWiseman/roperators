# Type checks

A collection of convenience checks for the type or contents of an
object. Each returns a logical value (or vector), which keeps guard
clauses and `if` statements short and readable. The `*_or_null` variants
are handy for validating optional arguments, and the `*_for_calcs` /
`*_for_indexing` helpers flag values that would break arithmetic or
subsetting (such as `NA`, `NaN`, or `Inf`).

## Usage

``` r
is.scalar(x)

is.scalar_or_null(x)

is.numeric_or_null(x)

is.character_or_null(x)

is.logical_or_null(x)

is.df_or_null(x)

is.list_or_null(x)

is.atomic_nan(x)

is.irregular_list(x)

any_bad_for_calcs(x, ..., na.rm = FALSE)

all_good_for_calcs(x, ..., na.rm = FALSE)

is.bad_for_indexing(x)

is.good_for_indexing(x)

is.bad_and_equal(x, y)

is.bad_for_calcs(x, na.rm = FALSE)

is.good_for_calcs(x, na.rm = FALSE)

is.null_or_na(x)
```

## Arguments

- x:

  object to be tested

- ...:

  values to be tested

- na.rm:

  if `TRUE`, `NA` values are not considered bad for calculations

- y:

  object to be tested

## Value

A logical value (or vector) indicating whether `x` meets the test.

## Author

Steven Nydick, <steven.nydick@kornferry.com>

## Examples

``` r
is.scalar(1)              # TRUE
#> [1] TRUE
is.scalar(c(1, 2))        # FALSE
#> [1] FALSE
is.numeric_or_null(NULL)  # TRUE
#> [1] TRUE
is.bad_for_calcs(NA)      # TRUE
#> [1] TRUE
is.good_for_calcs(1)      # TRUE
#> [1] TRUE
is.bad_and_equal(NA, NA)  # TRUE
#> [1] TRUE
```
