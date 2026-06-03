# String arithmetic operators

Perform string concatenation and arithmetic in a similar way to other
languages. String addition (`%+%`) glues strings together, string
subtraction (`%-%`) removes a pattern, string multiplication (`%s*%`)
repeats a string, and string division (`%s/%`) counts how many times a
pattern occurs. String division has no equivalent in languages like
'Python', yet is arguably more useful than string multiplication, and it
accepts regular expressions.

## Usage

``` r
x %+% y

x %-% y

x %s*% y

x %s/% y
```

## Arguments

- x:

  a string (character vector)

- y:

  a string (character vector or regular expression)

## Value

A character vector for `%+%`, `%-%`, and `%s*%`, and an integer count
for `%s/%`.

## Author

Ben Wiseman, <benjamin.h.wiseman@gmail.com>

## Examples

``` r
("ab" %+% "c") == "abc" # TRUE
#> [1] TRUE
("abc" %-% "b") == "ac" # TRUE
#> [1] TRUE
("ac" %s*% 2) == "acac" # TRUE
#>   ac 
#> TRUE 
("acac" %s/% "c") == 2  # TRUE
#>    c 
#> TRUE 
# String division with a regular expression:
"an apple a day keeps the malignant spirit of Steve Jobs at bay" %s/% "Steve Jobs|apple"
#> Steve Jobs|apple 
#>                2 
```
