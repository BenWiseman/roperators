# Format a proportion as a percentage string

Turn a proportion (such as `0.75`) into a human-friendly percentage
string (such as `"75%"`).

## Usage

``` r
as.percent(x, digits = 1, ...)
```

## Arguments

- x:

  a numeric proportion, or vector of proportions, where `1` represents
  one hundred percent

- digits:

  number of decimal places to show

- ...:

  further arguments passed to
  [`formatC`](https://rdrr.io/r/base/formatc.html)

## Value

A character vector of percentage strings.

## Author

Ben Wiseman, <benjamin.h.wiseman@gmail.com>

## Examples

``` r
as.percent(0.75)              # "75.0\%"
#> [1] "75.0%"
as.percent(c(0.1, 0.005))     # "10.0\%" "0.5\%"
#> [1] "10.0%" "0.5%" 
as.percent(2 / 3, digits = 0) # "67\%"
#> [1] "67%"
```
