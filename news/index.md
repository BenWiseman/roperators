# Changelog

## roperators 1.4.0

### New features

- [`f()`](https://benwiseman.github.io/roperators/reference/f.md) —
  string interpolation (“f-strings”):
  `f("Hi {name}, you have {n} messages")`.
- `%else%` — inline fallback for an expression that might error:
  `read.csv(path) %else% data.frame()`.
- `%+-%` — tolerance-interval constructor that composes with the
  `between` operators: `x %><% (5 %+-% 0.5)`.
- `%/0%` — safe division that returns `NA` instead of `Inf`/`NaN` on
  divide-by-zero.
- `%~%` — case- and whitespace-insensitive string equality (the string
  counterpart to `%~=%`).
- [`as.percent()`](https://benwiseman.github.io/roperators/reference/as.percent.md)
  — format a proportion as a percentage string.

### Bug fixes

- [`is.bad_for_calcs()`](https://benwiseman.github.io/roperators/reference/type_checks.md)
  now correctly flags `Inf`/`-Inf` as bad for calculations. Previously
  infinities slipped through, which also affected the comparison
  operators (`%==%`, `%>=%`, …) that rely on it.
- [`is.R_x64()`](https://benwiseman.github.io/roperators/reference/os.md)
  now calls
  [`is.os_unx()`](https://benwiseman.github.io/roperators/reference/os.md)
  correctly (it previously referenced the function without calling it).
- [`seq_around()`](https://benwiseman.github.io/roperators/reference/seq_around.md)
  now accepts ordinary numeric values for `n`; it previously errored
  unless `n` was given as an explicit integer (e.g. `5L`).
- The `between` operators (`%><%`, `%>=<%`) now handle reversed bounds,
  so `5 %><% c(10, 1)` behaves the same as `5 %><% c(1, 10)`.
- `%na<-%` now also accepts a replacement vector with one entry per
  missing value (filled in order), in addition to a scalar or a
  full-length vector.
- Removed a stray duplicate (and subtly broken) internal definition of
  [`is.bad_for_calcs()`](https://benwiseman.github.io/roperators/reference/type_checks.md).

### Improvements

- [`as.class()`](https://benwiseman.github.io/roperators/reference/cleaner_conversions.md)
  is now vector-safe and no longer builds and parses code from a string
  internally.
- [`is.http_available()`](https://benwiseman.github.io/roperators/reference/os.md)
  uses [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html)
  instead of the much slower
  [`installed.packages()`](https://rdrr.io/r/utils/installed.packages.html).
- [`get_system_python()`](https://benwiseman.github.io/roperators/reference/os.md),
  [`is_rda_file()`](https://benwiseman.github.io/roperators/reference/file_checks.md),
  and
  [`is_rds_file()`](https://benwiseman.github.io/roperators/reference/file_checks.md)
  are now exported (they were documented in earlier versions but never
  actually exported).

### Documentation, tests & infrastructure

- Documentation overhaul for clarity and consistency: tightened every
  help page, fixed typos throughout, and added missing `\value`
  documentation and runnable examples.
- `%rlike%` and `%perl%` now have their own help page (“SQL-style
  pattern matching”) instead of being grouped under the logical
  operators.
- Rewritten, comprehensive vignette and a `README.Rmd` so the README’s
  output is always real.
- Added a full `testthat` test suite covering the operators and helpers,
  a `BugReports` URL, a `pkgdown` site configuration, and GitHub Actions
  for `R-CMD-check` and pkgdown deployment.

## roperators 1.3.14

CRAN release: 2023-07-20

- added
  [`is.os_arm()`](https://benwiseman.github.io/roperators/reference/os.md)
  to check if running ARM cpu
- added
  [`get_R_version()`](https://benwiseman.github.io/roperators/reference/os.md)
  to pull current R version
- added
  [`get_R_version_age()`](https://benwiseman.github.io/roperators/reference/os.md)
  to find how old the installed R version is
- added
  [`get_latest_CRAN_version()`](https://benwiseman.github.io/roperators/reference/os.md)
  to find the latest version on CRAN (rvest required)
- added
  [`get_system_python()`](https://benwiseman.github.io/roperators/reference/os.md)
  to return default python called in system calls to “python”
- added `%C%` for choose operator and `%P%` for permute
- added `%integrate%` for inline integrations
- added
  [`is.http_available()`](https://benwiseman.github.io/roperators/reference/os.md)
  to check whether you can send an HTTP request (via httr, RCurl, or a
  system call to curl).

## roperators 1.3.0

- added
  [`seq_around()`](https://benwiseman.github.io/roperators/reference/seq_around.md)
  for evenly spaced sequences around an origin point
- Cleaned up roxygen comments, DESCRIPTION, and NAMESPACE files.
- Added a `NEWS.md` file to track changes to the package.
- New complete cases functions
  ([`length_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md),
  [`min_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md),
  [`max_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md),
  [`range_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md),
  [`all_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md),
  [`any_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md),
  [`sum_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md),
  [`prod_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md),
  [`mean_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md),
  [`median_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md),
  [`var_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md),
  [`cov_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md),
  [`cor_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md),
  [`sd_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md),
  [`weighted.mean_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md),
  [`quantile_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md),
  [`IQR_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md),
  [`mad_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md),
  [`rowSums_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md),
  [`colSums_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md),
  [`rowMeans_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md),
  [`colMeans_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md))
  that set default argument to standard base/stats functions for only
  calculating on complete cases.
- New
  [`get_os()`](https://benwiseman.github.io/roperators/reference/os.md),
  `is.os_mac`, `is.os_win`, `is.os_lnx`, and `is.os_unx` helper
  functions to quickly determine operating system.
- New
  [`is_txt_file()`](https://benwiseman.github.io/roperators/reference/file_checks.md),
  [`is_csv_file()`](https://benwiseman.github.io/roperators/reference/file_checks.md),
  [`is_excel_file()`](https://benwiseman.github.io/roperators/reference/file_checks.md),
  [`is_r_file()`](https://benwiseman.github.io/roperators/reference/file_checks.md),
  [`is_rdata_file()`](https://benwiseman.github.io/roperators/reference/file_checks.md),
  [`is_rda_file()`](https://benwiseman.github.io/roperators/reference/file_checks.md),
  [`is_spss_file()`](https://benwiseman.github.io/roperators/reference/file_checks.md),
  and
  [`check_ext_against()`](https://benwiseman.github.io/roperators/reference/file_checks.md)
  functions to quickly determine if file has specific file extensions.
- New `is.R_Revo()` and
  [`is.RStudio()`](https://benwiseman.github.io/roperators/reference/os.md)
  functions to determine whether Revolution R and RStudio are installed.
- New
  [`n_unique()`](https://benwiseman.github.io/roperators/reference/n_unique.md)
  function to count unique items and
  [`n_unique_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md)
  version for complete cases.
- New
  [`is.constant()`](https://benwiseman.github.io/roperators/reference/content_checks.md)
  and
  [`is.binary()`](https://benwiseman.github.io/roperators/reference/content_checks.md)
  functions to indicate whether a vector is constant (contains only ONE
  value type, ignoring NA) or binary (contains at most TWO value types,
  ignoring NA).
- New
  [`get_most_frequent()`](https://benwiseman.github.io/roperators/reference/time_savers.md)
  function to get most frequent thing(s) in x.
- New
  [`get_most_frequent_word()`](https://benwiseman.github.io/roperators/reference/time_savers.md)
  function to get most frequent word from string.
- New
  [`get_nth_word()`](https://benwiseman.github.io/roperators/reference/time_savers.md)
  function to get nth word from string
- New
  [`as.class()`](https://benwiseman.github.io/roperators/reference/cleaner_conversions.md)
  pipe-able function for arbitrary class conversions
- New
  [`paste_()`](https://benwiseman.github.io/roperators/reference/paste_and_cat.md)is
  the same as `paste0` but uses an underscore to separate
- New
  [`cat0()`](https://benwiseman.github.io/roperators/reference/paste_and_cat.md)
  is analogous to `paste0` but for cat
- New
  [`catN()`](https://benwiseman.github.io/roperators/reference/paste_and_cat.md)
  is the same as `cat0` but automatically inserts a new line after the
  cat
- New
  [`paste_series()`](https://benwiseman.github.io/roperators/reference/paste_and_cat.md)
  paste a series of things with a conjunction
- New
  [`paste_oxford()`](https://benwiseman.github.io/roperators/reference/paste_and_cat.md)
  shortcut for `paste_series` as oxford comma
