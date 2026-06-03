# roperators: cleaner, friendlier R

`roperators` adds the small things you keep wishing base R had: string
arithmetic, in-place modifiers, comparisons that survive contact with
`NA` and floating point, and a pile of tiny quality-of-life helpers.
It’s pure base R with no heavy dependencies, and it’s designed to be
especially welcoming to people arriving from Python and other languages.

This vignette is a guided tour. If you just want the highlights, here
they are:

``` r

"foo" %+% "bar"               # string addition
#> [1] "foobar"
(0.1 + 0.1 + 0.1) %~=% 0.3    # floating-point equality that works
#> [1] TRUE
c(1, NA) %==% c(1, NA)        # NA == NA is TRUE here
#> [1] TRUE TRUE

name <- "you"
f("hello {name}, 2 + 2 = {2 + 2}")   # f-strings!
#> [1] "hello you, 2 + 2 = 4"
```

## String arithmetic

One of the most common gripes from people coming to R is the lack of
string arithmetic. So we added it.

``` r

my_string <- "using infix (%) operators " %+% "lets R do string addition"
my_string
#> [1] "using infix (%) operators lets R do string addition"

# subtraction removes a pattern
my_string %-% "lets R do string addition"
#> [1] "using infix (%) operators "

# multiplication repeats (%*% was already taken, so it's %s*%)
"ha" %s*% 3
#>       ha 
#> "hahaha"
```

And something you *can’t* do in Python — **string division**, which
counts how many times a pattern appears (regular expressions welcome):

``` r

"an apple a day keeps the malignant spirit of Steve Jobs at bay" %s/% "a"
#> a 
#> 8

# with a regular expression
"an apple a day keeps the malignant spirit of Steve Jobs at bay" %s/% "Steve Jobs|apple"
#> Steve Jobs|apple 
#>                2
```

## In-place modifiers (*à la* `+=`)

How many times have you written something like
`df$x[long$condition] <- df$x[long$condition] + 1`? The line doesn’t
even fit on the page. With `roperators`:

``` r

x <- 1
x %+=% 2
x
#> [1] 3

d <- iris
# add 1 to setosa sepal lengths, in place
d$Sepal.Length[d$Species == "setosa"] %+=% 1
```

The full set is `%+=%`, `%-=%`, `%*=%`, `%/=%`, `%^=%`, `%root=%`, and
`%log=%`. `%+=%` and `%-=%` also work on strings:

``` r

x <- "ab"
x %+=% "c"
x
#> [1] "abc"
```

### Replacing missing values and regex matches

`%na<-%` fills in the `NA`s, and `%regex=%` / `%regex<-%` edit in place:

``` r

x <- c(NA, 1, 2, 3)
x %na<-% 0
x
#> [1] 0 1 2 3

x <- c("a1b", "b1", "c", "d0")
x %regex=% c("\\d+", "#")   # replace the matched part
x
#> [1] "a#b" "b#"  "c"   "d#"
```

## Comparisons that behave

### `NA`-aware equality

An `NA` doesn’t technically equal another `NA` — but most of the time,
for your purposes, it should. How many `if` statements have broken on
this?

``` r

a <- c(NA, "foo", "foo", NA)
b <- c(NA, "foo", "bar", "bar")

a == b      # base R: NA leaks through
#> [1]    NA  TRUE FALSE    NA
a %==% b    # roperators: NA == NA is TRUE
#> [1]  TRUE  TRUE FALSE FALSE
```

`%>=%` and `%<=%` carry the same NA-aware behaviour.

### Floating-point equality

The classic trap. Innocent statistics students are seldom warned:

``` r

(0.1 + 0.1 + 0.1) == 0.3    # FALSE (!)
#> [1] FALSE
(0.1 + 0.1 + 0.1) %~=% 0.3  # TRUE
#> [1] TRUE

# greater/less-than-or-approximately-equal
(0.1 + 0.1 + 0.1) %>~% 0.3
#> [1] TRUE
(0.1 + 0.1 + 0.1) %<~% 0.3
#> [1] TRUE
```

### Between, and strict equality

``` r

5 %><% c(1, 10)    # strictly between
#> [1] TRUE
1 %>=<% c(1, 10)   # inclusive
#> [1] TRUE
5 %><% c(10, 1)    # reversed bounds are fine too
#> [1] TRUE

# %===% is strict value-AND-class equality, like JavaScript's ===
x <- int(2)
x == 2       # TRUE
#> [1] TRUE
x %===% 2    # FALSE (different class)
#> [1] FALSE
x %===% int(2)
#> [1] TRUE
```

## Logical and SQL-style operators

``` r

"z" %ni% c("a", "b", "c")    # not in
#> [1] TRUE
TRUE %xor% FALSE             # exclusive or
#> [1] TRUE
TRUE %aon% TRUE              # all-or-nothing: both TRUE or both FALSE
#> [1] TRUE

# SQL-style LIKE
c("FOO", "bar", "fizz") %rlike% "foo"   # case-insensitive
#> [1]  TRUE FALSE FALSE
c("dOe", "doe")         %perl% "[a-z]O" # case-sensitive, Perl regex
#> [1]  TRUE FALSE
```

## ✨ New in 1.4

A small, opinionated set of additions.

**[`f()`](https://benwiseman.github.io/roperators/reference/f.md) —
string interpolation (R’s f-strings).** Anything in
[`{ }`](https://rdrr.io/r/base/Paren.html) is evaluated in the calling
environment:

``` r

who <- "Ben"; n <- 2
f("Hi {who}, you have {n} new message{if (n != 1) 's'}")
#> [1] "Hi Ben, you have 2 new messages"
f("today's first letters: {head(LETTERS, n)}")   # vectors are collapsed
#> [1] "today's first letters: A, B"
```

**`%else%` — inline fallback** when an expression might error (the
fallback is only evaluated if needed):

``` r

sqrt("not a number") %else% NA_real_
#> [1] NA
(1:3)[[99]]          %else% "out of range"
#> [1] "out of range"
```

**`%/0%` — safe division** that returns `NA` instead of letting
`Inf`/`NaN` poison a downstream
[`sum()`](https://rdrr.io/r/base/sum.html) or
[`mean()`](https://rdrr.io/r/base/mean.html):

``` r

c(10, 20, 30) %/0% c(2, 0, 5)
#> [1]  5 NA  6
```

**`%+-%` — a tolerance interval** that drops straight into the `between`
operators:

``` r

5 %+-% 0.5
#> [1] 4.5 5.5
4.9 %><% (5 %+-% 0.5)
#> [1] TRUE
```

**`%~%` — fuzzy string equality** (ignores case and whitespace), the
string sibling of `%~=%`:

``` r

"  Yes " %~% "yes"
#> [1] TRUE
c("Apple", "PEAR") %~% c("apple", "pear")
#> [1] TRUE TRUE
```

**[`as.percent()`](https://benwiseman.github.io/roperators/reference/as.percent.md)
— proportions as tidy strings:**

``` r

as.percent(c(0.1, 0.005, 2 / 3))
#> [1] "10.0%" "0.5%"  "66.7%"
as.percent(2 / 3, digits = 0)
#> [1] "67%"
```

## Shorter type conversions

R’s conversion syntax is wordy. These help:

``` r

chr(42)      # as.character()
#> [1] "42"
int(42.9)    # as.integer()
#> [1] 42
num("4.2")   # as.numeric()
#> [1] 4.2
bool("TRUE") # as.logical()
#> [1] TRUE

# the infamous factor-to-number gotcha, solved:
fac <- factor(c(11, 22, 33))
as.numeric(fac)     # 1 2 3  -- almost never what you want
#> [1] 1 2 3
f.as.numeric(fac)   # 11 22 33
#> [1] 11 22 33

# convert to a class chosen at run time
as.class(255, "roman")
#> [1] CCLV
```

## Safety and type checks

Rather than chaining five checks, ask one question:

``` r

# would these break a calculation?
is.bad_for_calcs(c(1, NA, Inf, NaN, 5))
#> [1] FALSE  TRUE  TRUE  TRUE FALSE

is.scalar(1)
#> [1] TRUE
is.constant(c(1, 1, 1))
#> [1] TRUE
is.binary(c("a", "b", "a"))
#> [1] TRUE
```

There’s a full family of `is.*_or_null()` predicates too, handy for
validating optional function arguments.

## Everyday helpers

``` r

# pull pieces out of vectors and strings
get_1st_word("Ada Lovelace")
#> [1] "Ada"
get_last_word("Ada Lovelace")
#> [1] "Lovelace"
get_most_frequent(c("a", "b", "b", "c", "b"))
#> [1] "b"

# Oxford-comma joining
paste_oxford("Tom", "Dick", "Harry")
#> [1] "Tom, Dick, and Harry"

# complete-cases stats: just add _cc for na.rm = TRUE
mean_cc(c(1, 2, NA))
#> [1] 1.5
sd_cc(c(1, 2, 3, NA))
#> [1] 1

# environment checks
get_os()
#> [1] "linux"
get_R_version()
#> [1] "4.6.0"

# file-extension checks
is_csv_file(c("a.csv", "b.txt"))
#> [1]  TRUE FALSE
```

## Cheat sheet

| You want… | Use |
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
| SQL-style LIKE | `%rlike%` / `%perl%` |
| String interpolation | [`f()`](https://benwiseman.github.io/roperators/reference/f.md) |
| Inline error fallback | `%else%` |
| Safe divide / tolerance | `%/0%` / `%+-%` |
| Fuzzy string match | `%~%` |

## A note on collisions

A few names are deliberately shared with the wider ecosystem — `%+%`
with ggplot2, and `%like%`-style matching with data.table. If you load
those packages as well, reach for the namespaced form
(`roperators::%+%`) where it matters.
