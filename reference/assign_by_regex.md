# Assign to a vector only where a regular expression matches

This takes two arguments, just like `gsub`: a pattern and a replacement.
It overwrites the *entire* element wherever the pattern matches. If you
want to substitute only the matched portion, use `%regex=%` instead; to
replace matches with nothing (`""`), use `%-%` or `%-=%`.

## Usage

``` r
x %regex<-% value
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

# overwrite any element containing a digit
x %regex<-% c("\\d+", "x")
print(x)
#> [1] "x" "x" "c" "x"
# "x" "b" "c" "x"
```
