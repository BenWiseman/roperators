# roperators

> Additional operators and helpers to make R code easier to **read,
> write, and maintain** — and a friendlier landing for people arriving
> from Python and other languages.

`roperators` gives you the small things you keep wishing R had: string
arithmetic (`'foo' + 'bar'`), in-place modifiers (`x += 1`), comparisons
that don’t fall over on `NA` or floating point, and a pile of tiny
quality-of-life helpers. Pure base R, no heavy dependencies.

(It’s pronounced *rop-er-ators*, not *r-operators*.)

## Installation

``` r

# Released version from CRAN:
install.packages("roperators")

# Development version from GitHub:
# install.packages("remotes")
remotes::install_github("BenWiseman/roperators")
```

``` r

library(roperators)
```

## String arithmetic

``` r

"using infix (%) operators " %+% "lets R do string addition"
#> [1] "using infix (%) operators lets R do string addition"

"abcabc" %-% "c"            # subtract a pattern
#> [1] "abab"
"ha" %s*% 3                 # multiply
#>       ha 
#> "hahaha"
"an apple a day" %s/% "a"   # divide: how many times does the pattern occur?
#> a 
#> 4
```

## In-place modifiers (*à la* `+=`)

No more `df$x[long$condition] <- df$x[long$condition] + 1`:

``` r

x <- 1
x %+=% 2
x
#> [1] 3

d <- iris
d$Sepal.Length[d$Species == "setosa"] %+=% 1   # add 1, in place
```

You also get `%-=%`, `%*=%`, `%/=%`, `%^=%`, `%root=%`, `%log=%`,
`%regex=%` (apply a regex in place), `%regex<-%` (overwrite matching
elements), and `%na<-%` (fill in the `NA`s):

``` r

x <- c(1, NA, 3, NA)
x %na<-% 0
x
#> [1] 1 0 3 0
```

## Comparisons that behave

`NA`-aware equality, real floating-point equality, and `between`:

``` r

c(1, NA, 3) == c(1, NA, 4)    # base R: NA leaks through
#> [1]  TRUE    NA FALSE
c(1, NA, 3) %==% c(1, NA, 4)  # roperators: NA == NA is TRUE
#> [1]  TRUE  TRUE FALSE

(0.1 + 0.1 + 0.1) == 0.3      # base R: FALSE (floating point)
#> [1] FALSE
(0.1 + 0.1 + 0.1) %~=% 0.3    # roperators: TRUE
#> [1] TRUE

5 %><% c(1, 10)   # strictly between
#> [1] TRUE
1 %>=<% c(1, 10)  # between, inclusive
#> [1] TRUE
```

Plus `%===%` (strict value-and-class equality, like JavaScript’s `===`),
`%>=%` / `%<=%` (with `NA` equality), and `%>~%` / `%<~%` (greater/less
than *or* approximately equal).

## Logical & SQL-style pattern matching

``` r

"z" %ni% c("a", "b", "c")          # not in
#> [1] TRUE
TRUE %xor% FALSE                   # exclusive or
#> [1] TRUE
c("FOO", "bar") %rlike% "foo"      # case-insensitive LIKE
#> [1]  TRUE FALSE
```

## ✨ New in 1.4

``` r

# f-strings: interpolate expressions straight into a string
name <- "Ben"; n <- 2
f("Hi {name}, you have {n} message{if (n != 1) 's'}")
#> [1] "Hi Ben, you have 2 messages"

# %else%: inline fallback if an expression errors
sqrt("not a number") %else% NA_real_
#> [1] NA

# %/0%: safe division (no Inf/NaN sneaking into your means)
10 %/0% 0
#> [1] NA

# %+-%: tolerance interval that composes with the 'between' operators
4.9 %><% (5 %+-% 0.5)
#> [1] TRUE

# %~%: fuzzy, case/whitespace-insensitive string equality
"  Yes " %~% "yes"
#> [1] TRUE

# as.percent(): proportions -> tidy percentage strings
as.percent(c(0.1, 0.005, 2/3))
#> [1] "10.0%" "0.5%"  "66.7%"
```

## Shorter conversions & safety checks

``` r

int("42"); chr(42); num("4.2"); bool("TRUE")   # as.integer(), as.character(), ...
#> [1] 42
#> [1] "42"
#> [1] 4.2
#> [1] TRUE

# stop writing as.numeric(as.character(factor_var)):
f.as.numeric(factor(c(11, 22, 33)))
#> [1] 11 22 33

# one guard instead of five:
is.bad_for_calcs(c(1, NA, Inf, NaN))
#> [1] FALSE  TRUE  TRUE  TRUE
```

There are matching `is.*_or_null()` predicates,
[`is.scalar()`](https://benwiseman.github.io/roperators/reference/type_checks.md),
[`is.constant()`](https://benwiseman.github.io/roperators/reference/content_checks.md),
[`is.binary()`](https://benwiseman.github.io/roperators/reference/content_checks.md),
and friends.

## Everyday helpers

``` r

get_1st_word("Ada Lovelace")           # and get_last_word / get_nth_word
#> [1] "Ada"
get_most_frequent(c("a", "b", "b"))    # mode(s)
#> [1] "b"
paste_oxford("Tom", "Dick", "Harry")   # Oxford-comma joining
#> [1] "Tom, Dick, and Harry"

mean_cc(c(1, 2, NA))                   # *_cc = the base function with na.rm = TRUE
#> [1] 1.5
```

…plus OS checks
([`get_os()`](https://benwiseman.github.io/roperators/reference/os.md),
[`is.os_mac()`](https://benwiseman.github.io/roperators/reference/os.md),
…), file-extension checks
([`is_csv_file()`](https://benwiseman.github.io/roperators/reference/file_checks.md),
…), and
[`read.tsv()`](https://benwiseman.github.io/roperators/reference/read.tsv.md)
/
[`read.psv()`](https://benwiseman.github.io/roperators/reference/read.tsv.md).

## Learn more

- [`vignette("nicer_roperators")`](https://benwiseman.github.io/roperators/articles/nicer_roperators.md)
  — the guided tour.
- Full reference: <https://benwiseman.github.io/roperators/>

A quick note on collisions: a few names are deliberately shared with the
wider ecosystem (`%+%` with ggplot2, `%like%`-style matching with
data.table). If you load those packages too, reach for the namespaced
form (e.g. `roperators::%+%`) where it matters.
