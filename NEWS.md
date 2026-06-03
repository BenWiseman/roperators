# roperators 1.4.0

## New features

* `f()` — string interpolation ("f-strings"):
  `f("Hi {name}, you have {n} messages")`.
* `%else%` — inline fallback for an expression that might error:
  `read.csv(path) %else% data.frame()`.
* `%+-%` — tolerance-interval constructor that composes with the `between`
  operators: `x %><% (5 %+-% 0.5)`.
* `%/0%` — safe division that returns `NA` instead of `Inf`/`NaN` on
  divide-by-zero.
* `%~%` — case- and whitespace-insensitive string equality (the string
  counterpart to `%~=%`).
* `as.percent()` — format a proportion as a percentage string.

## Bug fixes

* `is.bad_for_calcs()` now correctly flags `Inf`/`-Inf` as bad for calculations.
  Previously infinities slipped through, which also affected the comparison
  operators (`%==%`, `%>=%`, ...) that rely on it.
* `is.R_x64()` now calls `is.os_unx()` correctly (it previously referenced the
  function without calling it).
* `seq_around()` now accepts ordinary numeric values for `n`; it previously
  errored unless `n` was given as an explicit integer (e.g. `5L`).
* The `between` operators (`%><%`, `%>=<%`) now handle reversed bounds, so
  `5 %><% c(10, 1)` behaves the same as `5 %><% c(1, 10)`.
* `%na<-%` now also accepts a replacement vector with one entry per missing
  value (filled in order), in addition to a scalar or a full-length vector.
* Removed a stray duplicate (and subtly broken) internal definition of
  `is.bad_for_calcs()`.

## Improvements

* `as.class()` is now vector-safe and no longer builds and parses code from a
  string internally.
* `is.http_available()` uses `requireNamespace()` instead of the much slower
  `installed.packages()`.
* `get_system_python()`, `is_rda_file()`, and `is_rds_file()` are now exported
  (they were documented in earlier versions but never actually exported).

## Documentation, tests & infrastructure

* Documentation overhaul for clarity and consistency: tightened every help
  page, fixed typos throughout, and added missing `\value` documentation and
  runnable examples.
* `%rlike%` and `%perl%` now have their own help page ("SQL-style pattern
  matching") instead of being grouped under the logical operators.
* Rewritten, comprehensive vignette and a `README.Rmd` so the README's output
  is always real.
* Added a full `testthat` test suite covering the operators and helpers, a
  `BugReports` URL, a `pkgdown` site configuration, and GitHub Actions for
  `R-CMD-check` and pkgdown deployment.


# roperators 1.3.14


* added `is.os_arm()` to check if running ARM cpu
* added `get_R_version()` to pull current R version
* added `get_R_version_age()` to find how old the installed R version is
* added `get_latest_CRAN_version()` to find the latest version on CRAN (rvest required)
* added `get_system_python()` to return default python called in system calls to "python"
* added `%C%` for choose operator and `%P%` for permute
* added `%integrate%` for inline integrations
* added `is.http_available()` to check whether you can send an HTTP request (via httr, RCurl, or a system call to curl).


# roperators 1.3.0

* added `seq_around()` for evenly spaced sequences around an origin point 
* Cleaned up roxygen comments, DESCRIPTION, and NAMESPACE files.
* Added a `NEWS.md` file to track changes to the package.
* New complete cases functions (`length_cc()`, `min_cc()`, `max_cc()`,
  `range_cc()`, `all_cc()`, `any_cc()`, `sum_cc()`, `prod_cc()`, `mean_cc()`,
  `median_cc()`, `var_cc()`, `cov_cc()`, `cor_cc()`, `sd_cc()`,
  `weighted.mean_cc()`, `quantile_cc()`, `IQR_cc()`, `mad_cc()`,
  `rowSums_cc()`, `colSums_cc()`, `rowMeans_cc()`, `colMeans_cc()`) that set
  default argument to standard base/stats functions for only calculating on
  complete cases.
* New `get_os()`, `is.os_mac`, `is.os_win`, `is.os_lnx`, and `is.os_unx` helper
  functions to quickly determine operating system.
* New `is_txt_file()`, `is_csv_file()`, `is_excel_file()`, `is_r_file()`, 
  `is_rdata_file()`, `is_rda_file()`, `is_spss_file()`, and `check_ext_against()`
  functions to quickly determine if file has specific file extensions.
* New `is.R_Revo()` and `is.RStudio()` functions to determine whether Revolution
  R and RStudio are installed.
* New `n_unique()` function to count unique items and `n_unique_cc()` version
  for complete cases.
* New `is.constant()` and `is.binary()` functions to indicate whether a vector
  is constant (contains only ONE value type, ignoring NA) or binary (contains
  at most TWO value types, ignoring NA).
* New `get_most_frequent()` function to get most frequent thing(s) in x.
* New `get_most_frequent_word()` function to get most frequent word from string. 
* New `get_nth_word()` function to get nth word from string
* New `as.class()` pipe-able function for arbitrary class conversions
* New `paste_()`is the same as `paste0` but uses an underscore to separate
* New `cat0()` is analogous to `paste0` but for cat
* New `catN()` is the same as `cat0` but automatically inserts a new line after the cat
* New `paste_series()` paste a series of things with a conjunction
* New `paste_oxford()` shortcut for `paste_series` as oxford comma
