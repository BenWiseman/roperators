# Modify an object in place by regular expression

This takes two arguments, just like `gsub`: a pattern and a replacement.
It overwrites only the matched portion of each element of `x`. If you
want to overwrite whole elements that match (rather than just the
matched portion), use `%regex<-%` instead.

## Usage

``` r
x %regex=% value
```

## Arguments

- x:

  a character vector

- value:

  a length-2 character vector of the form `c(pattern, replacement)`

## Value

Used for the side effect of reassigning `x` in the calling environment;
returns the modified `x` invisibly.

## Author

Ben Wiseman, <benjamin.h.wiseman@gmail.com>

## Examples

``` r
x <- c("a1b", "b1", "c", "d0")

# change any digit to "x"
x %regex=% c("\\d+", "x")
print(x)
#> [1] "axb" "bx"  "c"   "dx" 
# "axb" "b" "c" "dx"
```
