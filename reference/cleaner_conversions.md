# Cleaner type-conversion functions

Short aliases for the most common `as.*` conversions. There is nothing
magical here, but the shorter names can make data-wrangling code much
easier to read, especially for users coming from other languages.
`as.class()` additionally converts to a class chosen by name at run
time.

## Usage

``` r
chr(x, ...)

int(x, ...)

dbl(x, ...)

num(x, ...)

bool(x, ...)

as.class(x, class)
```

## Arguments

- x:

  value to be converted

- ...:

  further arguments passed to the underlying `as.*` function

- class:

  character; the name of the class to convert `x` to (used by
  `as.class`)

## Value

The value of `x` coerced to the requested type.

## Author

Steven Nydick, <steven.nydick@kornferry.com>

Ben Wiseman, <benjamin.h.wiseman@gmail.com>

## Examples

``` r
chr(42)    # "42" = as.character()
#> [1] "42"
int(42.1)  # 42L  = as.integer()
#> [1] 42
dbl("42")  # 42   = as.double()
#> [1] 42
num("42")  # 42   = as.numeric()
#> [1] 42
bool(42)   # TRUE = as.logical()
#> [1] TRUE


# as.class() converts to an arbitrary class chosen by name:
as.class(255, "roman") # CCLV
#> [1] CCLV
```
