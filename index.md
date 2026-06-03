# roperators

> Small, friendly operators and helpers that let your R code read the
> way you already think it.

R is a wonderful language to work in — but here and there it asks you to
write something clunkier than the thought in your head. `roperators`
smooths those moments over. Nothing magic underneath: just a handful of
small, well-behaved operators and helpers, all in base R — and a
friendly welcome for folks arriving from Python and elsewhere.

``` r

"foo" %+% "bar"               # add strings
#> [1] "foobar"
(0.1 + 0.1 + 0.1) %~=% 0.3    # floating-point equality that works
#> [1] TRUE
c(1, NA) %==% c(1, NA)        # NA == NA treated as TRUE
#> [1] TRUE TRUE
```

(It’s pronounced *rop-er-ators*, not *r-operators*.)

## Installation

``` r

# released version, from CRAN:
install.packages("roperators")

# or the development version, from GitHub:
# install.packages("remotes")
remotes::install_github("BenWiseman/roperators")
```

``` r

library(roperators)

"hello " %+% "world"   # your first win
#> [1] "hello world"
```

## Find the right tool

*On the [package website](https://benwiseman.github.io/roperators/) this
is an interactive finder — search by what you’re trying to do (“join two
strings”, “missing values”, “read a file”…) and jump straight to the
docs. Either way, the [full
reference](https://benwiseman.github.io/roperators/reference/index.html)
lists everything.*

*New here? Take the [5-minute guided
tour](https://benwiseman.github.io/roperators/articles/nicer_roperators.html),
or skim the tour of features below.*

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

There’s a whole family — `%-=%`, `%*=%`, `%/=%`, `%^=%`, `%root=%`,
`%log=%` — plus `%regex=%` (apply a regex in place), `%regex<-%`
(overwrite matching elements), and `%na<-%` (fill in the `NA`s):

``` r

x <- c(1, NA, 3, NA)
x %na<-% 0
x
#> [1] 1 0 3 0
```

## Comparisons that behave

`NA` and floating point trip up almost everyone — it’s not your fault,
just the defaults:

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

A few logical conveniences, plus SQL-style pattern matching:

``` r

"z" %ni% c("a", "b", "c")          # not in
#> [1] TRUE
TRUE %xor% FALSE                   # exclusive or
#> [1] TRUE
c("FOO", "bar") %rlike% "foo"      # case-insensitive LIKE
#> [1]  TRUE FALSE
```

## f-strings & safe everyday operators

A few of these arrived in 1.4 (see the
[changelog](https://benwiseman.github.io/roperators/news/index.html)):

``` r

# f-strings: interpolate expressions straight into a string
name <- "Ben"; n <- 2
f("Hi {name}, you have {n} message{if (n != 1) 's'}")
#> [1] "Hi Ben, you have 2 messages"

# %else%: a fallback when an expression errors
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

## Shorter conversions & quick checks

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

# TRUE where a value is NA, NaN, or Inf:
is.bad_for_calcs(c(1, NA, Inf, NaN))
#> [1] FALSE  TRUE  TRUE  TRUE
```

There’s a matching set of `is.*_or_null()` predicates too, along with
[`is.scalar()`](https://benwiseman.github.io/roperators/reference/type_checks.md),
[`is.constant()`](https://benwiseman.github.io/roperators/reference/content_checks.md),
[`is.binary()`](https://benwiseman.github.io/roperators/reference/content_checks.md),
and friends — handy for keeping your guard clauses short.

## Everyday helpers

``` r

get_1st_word("Ada Lovelace")           # and get_last_word / get_nth_word
#> [1] "Ada"
get_most_frequent(c("a", "b", "b"))    # the mode(s)
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

## At a glance

| You want… | Reach for |
|----|----|
| String concat / subtract | `%+%` / `%-%` |
| String repeat / count | `%s*%` / `%s/%` |
| In-place maths | `%+=%` `%-=%` `%*=%` `%/=%` `%^=%` |
| Fill NAs / regex edit in place | `%na<-%` / `%regex=%` / `%regex<-%` |
| NA-aware (in)equality | `%==%` `%>=%` `%<=%` |
| Floating-point equality | `%~=%` `%>~%` `%<~%` |
| Strict (value + class) equality | `%===%` |
| Between (excl / incl) | `%><%` / `%>=<%` |
| Not-in / xor / all-or-nothing | `%ni%` / `%xor%` / `%aon%` |
| String interpolation | [`f()`](https://benwiseman.github.io/roperators/reference/f.md) |
| Inline error fallback | `%else%` |
| Safe divide / tolerance | `%/0%` / `%+-%` |
| Fuzzy string match | `%~%` |

## Next steps

- **[Take the 5-minute guided tour
  →](https://benwiseman.github.io/roperators/articles/nicer_roperators.html)**
- **[Browse all functions
  →](https://benwiseman.github.io/roperators/reference/index.html)**

A quick note on shared names: `%+%` is also ggplot2’s, and
`%like%`-style matching is data.table’s. If you load those too, use the
namespaced form (`roperators::%+%`) where it matters.
