# roperators

> Small, friendly operators and helpers that let your R code read the
> way you already think it.

### Which would you rather write?

| base R                                    | roperators  ← cleaner          |
|-------------------------------------------|--------------------------------|
| `c(1, NA, 3) == c(1, NA, 4)`              | `c(1, NA, 3) %==% c(1, NA, 4)` |
| `isTRUE(all.equal(0.1 + 0.1 + 0.1, 0.3))` | `(0.1 + 0.1 + 0.1) %~=% 0.3`   |
| `paste0(“Hi”, name, “, you have”, n)`     | `f(“Hi {name}, you have {n}”)` |
| `x[is.na(x)] <- 0`                        | `x %na<-% 0`                   |
| `ifelse(b == 0, NA, a / b)`               | `a %/0% b`                     |

*On the [package site](https://benwiseman.github.io/roperators/) this is
an interactive taste test — you pick the line you’d rather write, and it
tells you if you’ve got good taste. →*

All base R, zero dependencies — just operators and helpers that say what
you mean. (Pronounced *rop-er-ators*, not *r-operators*.)

## Installation

``` r

install.packages("roperators")                       # released, from CRAN
remotes::install_github("BenWiseman/roperators")      # or the dev version
```

## Find the right tool

*On the [package website](https://benwiseman.github.io/roperators/) this
is an interactive finder — search by what you’re trying to do (“join two
strings”, “missing values”, “read a file”…) and jump straight to the
docs. The [full
reference](https://benwiseman.github.io/roperators/reference/index.html)
lists everything.*

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
