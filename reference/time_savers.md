# Get the first, last, n-th, or most frequent element or word

Small helpers for pulling pieces out of vectors and strings - handy
inside `apply`-style calls. `get_1st()`, `get_last()`, and `get_nth()`
extract elements of a vector; the `*_word` variants split strings into
words first; and `get_most_frequent()` / `get_most_frequent_word()`
return the most common value(s).

## Usage

``` r
get_1st(x, type = "v")

get_last(x, type = "v")

get_nth(x, n = 1, type = "v")

get_1st_word(x, type = "v", split = " ")

get_last_word(x, type = "v", split = " ")

get_nth_word(x, n = 1, type = "v", split = " ")

get_most_frequent(x, collapse = NULL)

get_most_frequent_word(
  x,
  ignore.punct = TRUE,
  ignore.case = TRUE,
  split = " ",
  collapse = NULL,
  punct.regex = "[[:punct:]]",
  punct.replace = ""
)
```

## Arguments

- x:

  an R object, usually a vector or character string

- type:

  `'v'` (default) to index as a vector (`x[1]`), or `'l'` to index as a
  list (`x[[1]]`)

- n:

  integer; the n-th element or word to select

- split:

  character used to separate words (default `' '`)

- collapse:

  optional character; if supplied, the result is pasted into a single
  string using this separator

- ignore.punct:

  logical; ignore punctuation marks

- ignore.case:

  logical; ignore case (if `TRUE`, the result is lower-case)

- punct.regex:

  character; regex used to strip punctuation (default `[[:punct:]]`)

- punct.replace:

  character; what to replace punctuation with (default `""`)

## Value

The selected element(s) or word(s). `get_most_frequent*()` return the
most common value(s), as a character vector unless `x` is numeric and
`collapse` is not used.

## Author

Ben Wiseman, <benjamin.h.wiseman@gmail.com>

## Examples

``` r
# a list of split-up car names
car_names <- strsplit(row.names(mtcars)[1:5], " ")

sapply(car_names, get_1st)
#> [1] "Mazda"  "Mazda"  "Datsun" "Hornet" "Hornet"
# [1] "Mazda" "Mazda" "Datsun" "Hornet" "Hornet"

sapply(car_names, get_nth, 2)
#> [1] "RX4"        "RX4"        "710"        "4"          "Sportabout"
# [1] "RX4" "RX4" "710" "4" "Sportabout"

# Or pull a simple string apart (e.g. someone's full name):
get_1st_word(rownames(mtcars)[1:5])
#> [1] "Mazda"  "Mazda"  "Datsun" "Hornet" "Hornet"
# [1] "Mazda" "Mazda" "Datsun" "Hornet" "Hornet"

get_last_word(rownames(mtcars)[1:5])
#> [1] "RX4"        "Wag"        "710"        "Drive"      "Sportabout"
# [1] "RX4" "Wag" "710" "Drive" "Sportabout"

get_nth_word(rownames(mtcars)[1:5], 2)
#> [1] "RX4"        "RX4"        "710"        "4"          "Sportabout"
# [1] "RX4" "RX4" "710" "4" "Sportabout"


# get_most_frequent() returns the mode(s)
my_stuff <- c(1:10, 10, 5)
get_1st(my_stuff)           # 1
#> [1] 1
get_nth(my_stuff, 3)        # 3
#> [1] 3
get_last(my_stuff)          # 5
#> [1] 5
get_most_frequent(my_stuff) # the modes (5 and 10), as a numeric vector
#> [1] 10  5

my_chars <- c("a", "b", "b", "a", "g", "o", "l", "d")
get_most_frequent(my_chars)                    # "a" "b"
#> [1] "a" "b"
get_most_frequent(my_chars, collapse = " & ")  # "a & b"
#> [1] "a & b"

# the *_word helpers split a string into words first
generic_string <- "Who's A good boy? Winston's a good boy!"
get_1st_word(generic_string)    # "Who's"
#> [1] "Who's"
get_nth_word(generic_string, 3) # "good"
#> [1] "good"
get_last_word(generic_string)   # "boy!"
#> [1] "boy!"

# default ignores case and punctuation
get_most_frequent_word(generic_string)
#> [1] "a"    "boy"  "good"
# keep case and punctuation:
get_most_frequent_word(generic_string, ignore.case = FALSE, ignore.punct = FALSE)
#> [1] "good"
```
