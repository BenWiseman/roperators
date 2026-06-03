# Comparison operators with better missing-value handling

A set of comparison operators that improve on base R by treating missing
values as comparable (so two `NA`s are considered equal) and by adding
convenient interval ("between") tests. The operators are:

- `%==%` - equality that treats `NA == NA` as `TRUE`.

- `%===%` - strict equality of both value *and* class, for those
  familiar with 'JavaScript' `===`.

- `%>=%`, `%<=%` - greater/less than or equal to, with missing-value
  equality.

- `%><%`, `%>=<%` - between, with the ends excluded or included
  respectively.

For approximate (floating-point) comparisons, see
[`floating_point_comparisons`](https://benwiseman.github.io/roperators/reference/floating_point_comparisons.md).

## Usage

``` r
x %==% y

x %===% y

x %>=% y

x %<=% y

x %><% y

x %>=<% y
```

## Arguments

- x:

  a vector

- y:

  a vector (for the "between" operators, a length-2 vector of the form
  `c(lower, upper)`)

## Value

A logical vector.

## See also

Other comparisons:
[`floating_point_comparisons`](https://benwiseman.github.io/roperators/reference/floating_point_comparisons.md)

## Author

Ben Wiseman, <benjamin.h.wiseman@gmail.com>

## Examples

``` r
 ## Equality and ordering, with missing-value equality
 c(1, NA, 3, 4)  ==  c(1, NA, 4, 3)
#> [1]  TRUE    NA FALSE FALSE
 #  TRUE    NA  FALSE FALSE

 c(1, NA, 3, 4) %==% c(1, NA, 4, 3)
#> [1]  TRUE  TRUE FALSE FALSE
 #  TRUE  TRUE  FALSE FALSE

 c(1, NA, 3, 4) %>=% c(1, NA, 4, 3)
#> [1]  TRUE  TRUE FALSE  TRUE
 #  TRUE  TRUE FALSE  TRUE

 c(1, NA, 3, 4) %<=% c(1, NA, 4, 3)
#> [1]  TRUE  TRUE  TRUE FALSE
 #  TRUE  TRUE  TRUE FALSE

 ## Strict equality - a la 'JavaScript' ===
 # Only TRUE if the class AND value of x and y are the same
 x <- int(2)
 y <- 2
 x == y         # TRUE
#> [1] TRUE
 x %===% y      # FALSE
#> [1] FALSE
 x %===% int(y) # TRUE
#> [1] TRUE

 ## Between
 # ends excluded
 2 %><% c(1, 3)  # TRUE
#> [1] TRUE
 3 %><% c(1, 3)  # FALSE
#> [1] FALSE

 # ends included
 2 %>=<% c(1, 3) # TRUE
#> [1] TRUE
 3 %>=<% c(1, 3) # TRUE
#> [1] TRUE
```
