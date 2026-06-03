# Paste and cat helpers

A small family of `paste`/`cat` conveniences:

- `paste_()` - like [`paste0()`](https://rdrr.io/r/base/paste.html), but
  separates with an underscore.

- `cat0()` - like [`paste0()`](https://rdrr.io/r/base/paste.html), but
  for `cat` (no separator).

- `catN()` - like `cat0()`, but appends a new line.

- `paste_series()` - paste a series of items together with a
  conjunction, e.g. `"a, b, and c"`.

- `paste_oxford()` - a shortcut for `paste_series()` using an Oxford
  comma.

## Usage

``` r
paste_(..., collapse = NULL)

cat0(..., file = "", fill = FALSE, labels = NULL, append = FALSE)

catN(..., file = "", fill = FALSE, labels = NULL, append = FALSE)

paste_series(
  ...,
  sep = c(",", ";"),
  conjunction = c("and", "or", "&"),
  use_oxford_comma = TRUE
)

paste_oxford(...)
```

## Arguments

- ...:

  one or more R objects, to be converted to character vectors.

- collapse:

  an optional character string to separate the results. Not
  [`NA_character_`](https://rdrr.io/r/base/NA.html). When `collapse` is
  a string, the result is always a string
  ([`character`](https://rdrr.io/r/base/character.html) of length 1).

- file:

  character - A connection, or a character string naming the file to
  print to. If "" (the default), cat prints to the standard output
  connection, the console unless redirected by sink.

- fill:

  a logical or (positive) numeric controlling how the output is broken
  into successive lines. see \`?cat\`

- labels:

  character vector of labels for the lines printed. Ignored if fill is
  FALSE.

- append:

  logical. Only used if the argument `file` is the name of file (and not
  a connection or `"|cmd"`). If `TRUE` output will be appended to
  `file`; otherwise, it will overwrite the contents of `file`.

- sep:

  a character vector of strings to append after each element

- conjunction:

  the conjunction used to join the final elements of a series, such as
  `"and"`, `"or"`, or `"&"`

- use_oxford_comma:

  logical; whether to use the Oxford comma (standard in American
  English) before the conjunction

## Value

`paste_()`, `paste_series()`, and `paste_oxford()` return a character
vector. `cat0()` and `catN()` are called for their side effect
(printing) and return `NULL` invisibly.

## Author

Steven Nydick, <steven.nydick@kornferry.com>

## Examples

``` r
paste_("a", "b", "c")          # "a_b_c"  (paste0 with an underscore)
#> [1] "a_b_c"
cat0("no", "spaces", "here")   # prints: nospaceshere
#> nospaceshere
catN("...and this one", " ends with a newline")
#> ...and this one ends with a newline


paste_series("a")
#> [1] "a"
paste_series("a", "b")
#> [1] "a and b"
paste_series("a", "b", "c")
#> [1] "a, b, and c"
# works if putting entries into c function
paste_series(c("a", "b", "c"), "d")
#> [1] "a, b, c, and d"
# can use oxford comma or not
paste_series("a", "b", "c",
             use_oxford_comma = TRUE)
#> [1] "a, b, and c"
paste_series("a", "b", "c",
             use_oxford_comma = FALSE)
#> [1] "a, b and c"
# makes no difference if fewer than 3 items
paste_series("a", "b",
             use_oxford_comma = TRUE)
#> [1] "a and b"
```
