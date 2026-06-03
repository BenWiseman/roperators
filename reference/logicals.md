# Logical operators

A few convenience logical operators: "not in" (`%ni%`), exclusive or
(`%xor%`), and all-or-nothing (`%aon%`, which is `TRUE` when `x` and `y`
are both `TRUE` or both `FALSE`).

## Usage

``` r
x %ni% y

x %xor% y

x %aon% y
```

## Arguments

- x:

  a vector

- y:

  a vector

## Value

A logical vector.

## Author

Ben Wiseman, <benjamin.h.wiseman@gmail.com>

## Examples

``` r
 #### Not in ####
 "z" %ni% c("a", "b", "c") # TRUE
#> [1] TRUE

 #### Exclusive or ####
 TRUE  %xor% TRUE  # FALSE
#> [1] FALSE
 FALSE %xor% FALSE # FALSE
#> [1] FALSE
 FALSE %xor% TRUE  # TRUE
#> [1] TRUE

 #### All-or-nothing ####
 TRUE  %aon% TRUE  # TRUE
#> [1] TRUE
 FALSE %aon% FALSE # TRUE
#> [1] TRUE
 FALSE %aon% TRUE  # FALSE
#> [1] FALSE
```
