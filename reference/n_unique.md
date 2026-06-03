# Count the number of unique values

Returns the number of unique elements in `x`, optionally ignoring `NA`s.

## Usage

``` r
n_unique(x, na.rm = FALSE)
```

## Arguments

- x:

  a vector

- na.rm:

  logical; if `TRUE`, `NA`s are ignored when counting unique values

## Value

An integer count of unique values.

## Author

Ben Wiseman, <benjamin.h.wiseman@gmail.com>

## Examples

``` r
n_unique(c(1, 2, 2, 3, NA))               # 4
#> [1] 4
n_unique(c(1, 2, 2, 3, NA), na.rm = TRUE) # 3
#> [1] 3
```
