# Choose and permute operators

Shorthand infix operators for common combinatorics: `n %C% k` gives the
number of combinations ("n choose k"), and `n %P% k` gives the number of
permutations ("n permute k").

## Usage

``` r
n %C% k

n %P% k
```

## Arguments

- n:

  whole number (the `n` in "n choose/permute k")

- k:

  whole number (the `k` in "n choose/permute k")

## Value

A numeric value.

## Author

Ben Wiseman, <benjamin.h.wiseman@gmail.com>

## Examples

``` r
5 %C% 3 # 10  (5 choose 3)
#> [1] 10
5 %P% 3 # 60  (5 permute 3)
#> [1] 60
```
