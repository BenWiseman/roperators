# Operating system and environment checks

Determine the current operating system and R environment, and provide
simple flags for common questions such as "are we on a Mac?", "is this
64-bit R?", or "are we running inside RStudio?". These are useful when
writing code that must behave differently across platforms (for example,
choosing a parallel back-end on Unix versus Windows).

## Usage

``` r
get_os()

get_R_version()

get_R_version_age(units = c("years", "months", "weeks", "days"), rounding = 2)

get_latest_CRAN_version()

get_system_python()

is.os_mac()

is.os_win()

is.os_lnx()

is.os_unx()

is.os_x64()

is.os_arm()

is.R_x64()

is.R_revo()

is.RStudio()

is.http_available()
```

## Arguments

- units:

  character; the unit to report the R version age in, one of `"years"`,
  `"months"`, `"weeks"`, or `"days"`.

- rounding:

  integer; the number of decimal places to round the age to.

## Value

For the `is.*` checks, a single logical value. `get_os()` returns a
character string (`"win"`, `"mac"`, `"linux"`, or `"unix"`);
`get_R_version()` and `get_latest_CRAN_version()` return version
strings; and `get_R_version_age()` returns a numeric age.

## Author

Ben Wiseman, <benjamin.h.wiseman@gmail.com>

Steven Nydick, <steven.nydick@kornferry.com>

## Examples

``` r
# determine the operating system
get_os()
#> [1] "linux"

# test for a particular operating system
is.os_mac()
#> [1] FALSE
is.os_win()
#> [1] FALSE
is.os_lnx()
#> [1] TRUE
is.os_unx()
#> [1] TRUE

# environment checks
is.os_x64()
#> [1] TRUE
is.RStudio()
#> [1] FALSE
get_R_version()
#> [1] "4.6.0"
```
