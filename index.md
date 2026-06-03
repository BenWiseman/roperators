# roperators

> Small, friendly operators and helpers that let your R code read the
> way you already think it.

R is a wonderful language to work in — but here and there it asks you to
write something a little clunkier than the thought in your head.
`roperators` quietly smooths those moments over. There’s nothing magic
going on underneath: just a handful of small, well-behaved operators and
helpers, all in base R, that make everyday code calmer to read and write
— and a gentle welcome for folks arriving from Python and elsewhere.

(It’s pronounced *rop-er-ators*, by the way — not *r-operators*.)

## Find the right tool

*This interactive finder works best on the [package
website](https://benwiseman.github.io/roperators/) — search by what
you’re trying to do (“join two strings”, “missing values”, “read a
file”…) and jump straight to it.*

## Installation

``` r

# the released version, from CRAN:
install.packages("roperators")

# or the development version, from GitHub:
# install.packages("remotes")
remotes::install_github("BenWiseman/roperators")
```

``` r

library(roperators)
```

## String arithmetic

Let’s start with the one nearly everyone misses coming from other
languages — adding strings together:

``` r

"using infix (%) operators " %+% "lets R do string addition"
#> [1] "using infix (%) operators lets R do string addition"

"abcabc" %-% "c"            # subtract a pattern
#> [1] "abab"
"ha" %s*% 3                 # multiply
#>       ha 
#> "hahaha"
"an apple a day" %s/% "a"   # divide: how many times does the pattern appear?
#> a 
#> 4
```

## In-place modifiers (*à la* `+=`)

We’ve all written the same long line twice just to nudge a column up by
one. You needn’t:

``` r

x <- 1
x %+=% 2
x
#> [1] 3

d <- iris
d$Sepal.Length[d$Species == "setosa"] %+=% 1   # add 1, in place
```

There’s a whole little family — `%-=%`, `%*=%`, `%/=%`, `%^=%`,
`%root=%`, `%log=%` — plus `%regex=%` (apply a regex in place),
`%regex<-%` (overwrite matching elements), and `%na<-%` (gently fill in
the `NA`s):

``` r

x <- c(1, NA, 3, NA)
x %na<-% 0
x
#> [1] 1 0 3 0
```

## Comparisons that behave

`NA` and floating point trip up almost everyone at some point — and
honestly, it’s not your fault, it’s just the defaults. Here’s a softer
landing:

``` r

c(1, NA, 3) == c(1, NA, 4)    # base R: the NA leaks through
#> [1]  TRUE    NA FALSE
c(1, NA, 3) %==% c(1, NA, 4)  # roperators: NA == NA is treated as TRUE
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

You also get `%===%` (strict value-and-class equality, like JavaScript’s
`===`), `%>=%` / `%<=%` (with `NA` equality), and `%>~%` / `%<~%`
(greater/less than *or* approximately equal).

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

A few new friends arrived in this release:

``` r

# f-strings: interpolate expressions straight into a string
name <- "Ben"; n <- 2
f("Hi {name}, you have {n} message{if (n != 1) 's'}")
#> [1] "Hi Ben, you have 2 messages"

# %else%: a calm fallback if an expression decides to error
sqrt("not a number") %else% NA_real_
#> [1] NA

# %/0%: safe division, so a stray zero never poisons your means
10 %/0% 0
#> [1] NA

# %+-%: a tolerance interval that drops right into the 'between' operators
4.9 %><% (5 %+-% 0.5)
#> [1] TRUE

# %~%: forgiving, case/whitespace-insensitive string equality
"  Yes " %~% "yes"
#> [1] TRUE

# as.percent(): proportions, dressed up as tidy percentages
as.percent(c(0.1, 0.005, 2/3))
#> [1] "10.0%" "0.5%"  "66.7%"
```

## Shorter conversions & gentle checks

Shorter names for the conversions you reach for all the time:

``` r

int("42"); chr(42); num("4.2"); bool("TRUE")   # as.integer(), as.character(), ...
#> [1] 42
#> [1] "42"
#> [1] 4.2
#> [1] TRUE

# no more as.numeric(as.character(factor_var)):
f.as.numeric(factor(c(11, 22, 33)))
#> [1] 11 22 33

# one tidy question instead of five:
is.bad_for_calcs(c(1, NA, Inf, NaN))
#> [1] FALSE  TRUE  TRUE  TRUE
```

There’s a matching set of `is.*_or_null()` predicates too, along with
[`is.scalar()`](https://benwiseman.github.io/roperators/reference/type_checks.md),
[`is.constant()`](https://benwiseman.github.io/roperators/reference/content_checks.md),
[`is.binary()`](https://benwiseman.github.io/roperators/reference/content_checks.md),
and friends — handy for keeping your guard clauses short.

## A little drawer of everyday helpers

``` r

get_1st_word("Ada Lovelace")           # and get_last_word / get_nth_word
#> [1] "Ada"
get_most_frequent(c("a", "b", "b"))    # the mode(s)
#> [1] "b"
paste_oxford("Tom", "Dick", "Harry")   # Oxford-comma joining, done for you
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
  — the unhurried, guided tour.
- Full reference: <https://benwiseman.github.io/roperators/>

A gentle heads-up on names: a few are shared on purpose with the wider
world (`%+%` with ggplot2, `%like%`-style matching with data.table). If
you’ve got those loaded too, just reach for the namespaced form
(`roperators::%+%`) where it matters, and everyone gets along fine.
