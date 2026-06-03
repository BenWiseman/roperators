# Floating-point comparison operators

An important set of operators missing from base R. Using `==` on two
non-integer numbers can give unexpected results (see examples), because
of the way floating-point numbers are represented. These operators
instead test equality up to a small tolerance, via
[`all.equal`](https://rdrr.io/r/base/all.equal.html).

For a fuller explanation, see
<https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html>.

## Usage

``` r
x %~=% y

x %>~% y

x %<~% y
```

## Arguments

- x:

  numeric

- y:

  numeric

## Value

A logical value.

## See also

Other comparisons:
[`comparisons`](https://benwiseman.github.io/roperators/reference/comparisons.md)

## Author

Ben Wiseman, <benjamin.h.wiseman@gmail.com>

## Examples

``` r
 ## Floating-point test of equality
 # base R:
 (0.1 + 0.1 + 0.1) == 0.3   # FALSE
#> [1] FALSE
 # with roperators:
 (0.1 + 0.1 + 0.1) %~=% 0.3 # TRUE
#> [1] TRUE

 # Note how the base >= and <= behave here:
 (0.1 + 0.1 + 0.1) %>=% 0.3 # TRUE
#> [1] TRUE
 (0.1 + 0.1 + 0.1) %<=% 0.3 # FALSE
#> [1] FALSE

 # Use %>~% and %<~% for greater/less than OR approximately equal
 (0.1 + 0.1 + 0.1) %>~% 0.3 # TRUE
#> [1] TRUE
 (0.1 + 0.1 + 0.1) %<~% 0.3 # TRUE
#> [1] TRUE
```
