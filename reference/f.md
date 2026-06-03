# String interpolation (f-strings for R)

Interpolate R expressions into a string, like 'Python' f-strings.
Anything inside curly braces is evaluated in the calling environment and
inserted into the string. Doubled braces are treated as a single literal
brace.

## Usage

``` r
f(..., .envir = parent.frame())
```

## Arguments

- ...:

  one or more strings containing `{expr}` placeholders

- .envir:

  the environment in which to evaluate the placeholders (defaults to the
  calling environment)

## Value

A character vector the same length as the input, with every `{expr}`
replaced by its evaluated, comma-collapsed value.

## Note

`f` is also a popular name for throwaway functions, so be aware it may
mask (or be masked by) a local `f` of your own.

## Author

Ben Wiseman, <benjamin.h.wiseman@gmail.com>

## Examples

``` r
name <- "Ben"
n    <- 2
f("Hi {name}, you have {n} new messages")
#> [1] "Hi Ben, you have 2 new messages"
f("{n} + {n} = {n + n}")
#> [1] "2 + 2 = 4"

# vectors are collapsed with ", "
f("today's letters: {head(LETTERS, n)}")
#> [1] "today's letters: A, B"
```
