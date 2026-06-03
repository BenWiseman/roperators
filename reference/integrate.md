# Inline integration operator

An inline call to [`integrate`](https://rdrr.io/r/stats/integrate.html)
that returns the value of the integral directly, rather than the usual
list.

## Usage

``` r
f %integrate% range
```

## Arguments

- f:

  a function with a numeric return value

- range:

  a length-2 numeric vector, `c(lower, upper)`

## Value

A single numeric value: the value of the integral.

## Author

Ben Wiseman, <benjamin.h.wiseman@gmail.com>

## Examples

``` r
f <- function(x) x^2
f %integrate% c(0, 1) # 0.3333333
#> [1] 0.3333333

# compared with base R, which returns a list:
str(integrate(f, 0, 1))
#> List of 5
#>  $ value       : num 0.333
#>  $ abs.error   : num 3.7e-15
#>  $ subdivisions: int 1
#>  $ message     : chr "OK"
#>  $ call        : language integrate(f = f, lower = 0, upper = 1)
#>  - attr(*, "class")= chr "integrate"
```
