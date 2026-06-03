# Assignment (in-place modifier) operators

Modify the stored value of the left-hand-side object in place. These are
the equivalent of operators such as `+=`, `-=`, `*=`, and `/=` in
languages like C++ or 'Python'. `%+=%` and `%-=%` also work with
strings, and `%-=%` accepts regular expressions.

## Usage

``` r
x %+=% y

x %-=% y

x %*=% y

x %/=% y

x %^=% y

x %log=% y

x %root=% y
```

## Arguments

- x:

  a stored value

- y:

  value to modify the stored value by

## Value

Used for the side effect of reassigning `x` in the calling environment;
returns the new value of `x` invisibly.

## Author

Ben Wiseman, <benjamin.h.wiseman@gmail.com>

## Examples

``` r
x <- 1
x %+=% 2
x == 3 # TRUE
#> [1] TRUE

x %-=% 3
x == 0 # TRUE
#> [1] TRUE

# Or with data frames...
test <- iris

# Simply modify in place
test$Sepal.Length[test$Species == 'setosa' & test$Petal.Length < 1.5] %+=% 1

# Which is much nicer than typing:
test$Sepal.Length[test$Species == 'setosa' & test$Petal.Length < 1.5] <-
test$Sepal.Length[test$Species == 'setosa' & test$Petal.Length < 1.5] + 1
# ...which is over the 100 character limit for R documentation!

# %+=% and %-=% also work with strings
x <- "ab"
x %+=% "c"
x %-=% "b"
x == "ac" # TRUE
#> [1] TRUE

# %-=% can also take regular expressions
x <- "foobar"
x %-=% "[fb]"
print(x)
#> [1] "ooar"
# "ooar"
```
