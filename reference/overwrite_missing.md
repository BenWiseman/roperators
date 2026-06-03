# Assign a value to a vector's missing values

`%na<-%` is a shortcut to assign a value to all `NA` elements of `x`.
The replacement may be a single value (recycled to every `NA`), a vector
with one entry per missing element (filled in order), or a vector the
same length as `x` (its values at the missing positions are used).

## Usage

``` r
x %na<-% value
```

## Arguments

- x:

  a vector

- value:

  a single value, a vector with one entry per `NA`, or a vector the same
  length as `x`

## Value

Used for the side effect of reassigning `x` in the calling environment;
returns the modified `x` invisibly.

## Author

Ben Wiseman, <benjamin.h.wiseman@gmail.com>

## Examples

``` r
x <- c("a", NA, "c")
x %na<-% "b"
print(x)
#> [1] "a" "b" "c"
# "a" "b" "c"

x <- c(1, NA, 3, NA)
x %na<-% c(2, 4)   # one replacement per NA, in order
print(x)
#> [1] 1 2 3 4
# 1 2 3 4
```
