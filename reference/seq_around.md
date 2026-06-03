# Sequence of evenly spaced points around an origin

Returns a vector of `n` points evenly spaced around `origin`, with the
given `spacing` between neighbours.

## Usage

``` r
seq_around(origin = 1, n = 1, spacing = 0.25)
```

## Arguments

- origin:

  number to centre the sequence on

- n:

  number of points to create (a single whole number)

- spacing:

  distance between any two neighbouring points

## Value

A numeric vector. Defaults to `1` when called with no arguments, to
mirror the default behaviour of
[`seq`](https://rdrr.io/r/base/seq.html).

## Author

Ben Wiseman, <benjamin.h.wiseman@gmail.com>

## Examples

``` r
seq_around(0, n = 5, spacing = 1) # -2 -1  0  1  2
#> [1] -2 -1  0  1  2
seq_around(10, n = 3)             # 9.75 10.00 10.25
#> [1]  9.75 10.00 10.25
```
