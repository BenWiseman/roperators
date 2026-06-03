# Load a package, installing it first if necessary

Attempts to load `pkg`; if it is not installed, installs it (from CRAN
by default) and then loads it. `require.force` is an alias for
`library.force`.

## Usage

``` r
library.force(pkg, ...)

require.force(pkg, ...)
```

## Arguments

- pkg:

  name of the package to load or install

- ...:

  further arguments passed to
  [`install.packages`](https://rdrr.io/r/utils/install.packages.html)

## Value

Invisibly returns `NULL`; called for the side effect of loading (and
possibly installing) `pkg`.
