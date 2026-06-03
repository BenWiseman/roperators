# Convert a factor with numeric labels into a numeric vector

Converting a factor with
[`as.numeric()`](https://rdrr.io/r/base/numeric.html) returns the
underlying integer codes, not the labels, which is rarely what you want
when the labels are themselves numbers. `f.as.numeric()` returns the
labels as numbers instead.

## Usage

``` r
f.as.numeric(x)
```

## Arguments

- x:

  a factor with numeric labels

## Value

A numeric vector of the factor's labels.

## Author

Ulrike Grömping, <groemping@beuth-hochschule.de>

Ben Wiseman, <benjamin.h.wiseman@gmail.com>

## Examples

``` r
x <- factor(c(11, 22, 33, 99))

as.numeric(x)
#> [1] 1 2 3 4
# 1 2 3 4      # the integer codes - NOT usually what you want

f.as.numeric(x)
#> [1] 11 22 33 99
# 11 22 33 99  # the labels as numbers - usually what you want

# equivalent to the clunkier base idiom:
as.numeric(as.character(x))
#> [1] 11 22 33 99
```
