# Package index

## Operators — arithmetic & strings

String arithmetic, in-place modifiers, regex assignment, and maths
operators.

- [`` `%+%` ``](https://benwiseman.github.io/roperators/reference/string_arithmetic.md)
  [`` `%-%` ``](https://benwiseman.github.io/roperators/reference/string_arithmetic.md)
  [`` `%s*%` ``](https://benwiseman.github.io/roperators/reference/string_arithmetic.md)
  [`` `%s/%` ``](https://benwiseman.github.io/roperators/reference/string_arithmetic.md)
  : String arithmetic operators
- [`` `%+=%` ``](https://benwiseman.github.io/roperators/reference/assign_ops.md)
  [`` `%-=%` ``](https://benwiseman.github.io/roperators/reference/assign_ops.md)
  [`` `%*=%` ``](https://benwiseman.github.io/roperators/reference/assign_ops.md)
  [`` `%/=%` ``](https://benwiseman.github.io/roperators/reference/assign_ops.md)
  [`` `%^=%` ``](https://benwiseman.github.io/roperators/reference/assign_ops.md)
  [`` `%log=%` ``](https://benwiseman.github.io/roperators/reference/assign_ops.md)
  [`` `%root=%` ``](https://benwiseman.github.io/roperators/reference/assign_ops.md)
  : Assignment (in-place modifier) operators
- [`` `%+-%` ``](https://benwiseman.github.io/roperators/reference/arithmetic_sugar.md)
  [`` `%/0%` ``](https://benwiseman.github.io/roperators/reference/arithmetic_sugar.md)
  : Arithmetic convenience operators
- [`` `%regex=%` ``](https://benwiseman.github.io/roperators/reference/overwrite_by_regex.md)
  : Modify an object in place by regular expression
- [`` `%regex<-%` ``](https://benwiseman.github.io/roperators/reference/assign_by_regex.md)
  : Assign to a vector only where a regular expression matches
- [`` `%na<-%` ``](https://benwiseman.github.io/roperators/reference/overwrite_missing.md)
  : Assign a value to a vector's missing values
- [`` `%C%` ``](https://benwiseman.github.io/roperators/reference/choose_permute.md)
  [`` `%P%` ``](https://benwiseman.github.io/roperators/reference/choose_permute.md)
  : Choose and permute operators
- [`` `%integrate%` ``](https://benwiseman.github.io/roperators/reference/integrate.md)
  : Inline integration operator

## Operators — comparison & logic

Comparisons that handle missing values and floating point, plus logical
and pattern-matching operators.

- [`` `%==%` ``](https://benwiseman.github.io/roperators/reference/comparisons.md)
  [`` `%===%` ``](https://benwiseman.github.io/roperators/reference/comparisons.md)
  [`` `%>=%` ``](https://benwiseman.github.io/roperators/reference/comparisons.md)
  [`` `%<=%` ``](https://benwiseman.github.io/roperators/reference/comparisons.md)
  [`` `%><%` ``](https://benwiseman.github.io/roperators/reference/comparisons.md)
  [`` `%>=<%` ``](https://benwiseman.github.io/roperators/reference/comparisons.md)
  : Comparison operators with better missing-value handling
- [`` `%~=%` ``](https://benwiseman.github.io/roperators/reference/floating_point_comparisons.md)
  [`` `%>~%` ``](https://benwiseman.github.io/roperators/reference/floating_point_comparisons.md)
  [`` `%<~%` ``](https://benwiseman.github.io/roperators/reference/floating_point_comparisons.md)
  : Floating-point comparison operators
- [`` `%ni%` ``](https://benwiseman.github.io/roperators/reference/logicals.md)
  [`` `%xor%` ``](https://benwiseman.github.io/roperators/reference/logicals.md)
  [`` `%aon%` ``](https://benwiseman.github.io/roperators/reference/logicals.md)
  : Logical operators
- [`` `%rlike%` ``](https://benwiseman.github.io/roperators/reference/pattern_matching.md)
  [`` `%perl%` ``](https://benwiseman.github.io/roperators/reference/pattern_matching.md)
  : SQL-style pattern-matching operators
- [`` `%~%` ``](https://benwiseman.github.io/roperators/reference/fuzzy_match.md)
  : Fuzzy (case- and whitespace-insensitive) string equality

## Strings & interpolation

Build and format strings, including f-string interpolation.

- [`f()`](https://benwiseman.github.io/roperators/reference/f.md) :
  String interpolation (f-strings for R)
- [`as.percent()`](https://benwiseman.github.io/roperators/reference/as.percent.md)
  : Format a proportion as a percentage string
- [`paste_()`](https://benwiseman.github.io/roperators/reference/paste_and_cat.md)
  [`cat0()`](https://benwiseman.github.io/roperators/reference/paste_and_cat.md)
  [`catN()`](https://benwiseman.github.io/roperators/reference/paste_and_cat.md)
  [`paste_series()`](https://benwiseman.github.io/roperators/reference/paste_and_cat.md)
  [`paste_oxford()`](https://benwiseman.github.io/roperators/reference/paste_and_cat.md)
  : Paste and cat helpers

## Inline control flow

A fallback operator for expressions that might error.

- [`` `%else%` ``](https://benwiseman.github.io/roperators/reference/inline_fallback.md)
  : Inline fallback for expressions that might error

## Type conversion & checks

Shorthand conversions and a family of type/content/file predicates.

- [`chr()`](https://benwiseman.github.io/roperators/reference/cleaner_conversions.md)
  [`int()`](https://benwiseman.github.io/roperators/reference/cleaner_conversions.md)
  [`dbl()`](https://benwiseman.github.io/roperators/reference/cleaner_conversions.md)
  [`num()`](https://benwiseman.github.io/roperators/reference/cleaner_conversions.md)
  [`bool()`](https://benwiseman.github.io/roperators/reference/cleaner_conversions.md)
  [`as.class()`](https://benwiseman.github.io/roperators/reference/cleaner_conversions.md)
  : Cleaner type-conversion functions
- [`f.as.numeric()`](https://benwiseman.github.io/roperators/reference/factor_conversion.md)
  : Convert a factor with numeric labels into a numeric vector
- [`is.scalar()`](https://benwiseman.github.io/roperators/reference/type_checks.md)
  [`is.scalar_or_null()`](https://benwiseman.github.io/roperators/reference/type_checks.md)
  [`is.numeric_or_null()`](https://benwiseman.github.io/roperators/reference/type_checks.md)
  [`is.character_or_null()`](https://benwiseman.github.io/roperators/reference/type_checks.md)
  [`is.logical_or_null()`](https://benwiseman.github.io/roperators/reference/type_checks.md)
  [`is.df_or_null()`](https://benwiseman.github.io/roperators/reference/type_checks.md)
  [`is.list_or_null()`](https://benwiseman.github.io/roperators/reference/type_checks.md)
  [`is.atomic_nan()`](https://benwiseman.github.io/roperators/reference/type_checks.md)
  [`is.irregular_list()`](https://benwiseman.github.io/roperators/reference/type_checks.md)
  [`any_bad_for_calcs()`](https://benwiseman.github.io/roperators/reference/type_checks.md)
  [`all_good_for_calcs()`](https://benwiseman.github.io/roperators/reference/type_checks.md)
  [`is.bad_for_indexing()`](https://benwiseman.github.io/roperators/reference/type_checks.md)
  [`is.good_for_indexing()`](https://benwiseman.github.io/roperators/reference/type_checks.md)
  [`is.bad_and_equal()`](https://benwiseman.github.io/roperators/reference/type_checks.md)
  [`is.bad_for_calcs()`](https://benwiseman.github.io/roperators/reference/type_checks.md)
  [`is.good_for_calcs()`](https://benwiseman.github.io/roperators/reference/type_checks.md)
  [`is.null_or_na()`](https://benwiseman.github.io/roperators/reference/type_checks.md)
  : Type checks
- [`is.constant()`](https://benwiseman.github.io/roperators/reference/content_checks.md)
  [`is.binary()`](https://benwiseman.github.io/roperators/reference/content_checks.md)
  : Vector content checks
- [`is_txt_file()`](https://benwiseman.github.io/roperators/reference/file_checks.md)
  [`is_csv_file()`](https://benwiseman.github.io/roperators/reference/file_checks.md)
  [`is_excel_file()`](https://benwiseman.github.io/roperators/reference/file_checks.md)
  [`is_r_file()`](https://benwiseman.github.io/roperators/reference/file_checks.md)
  [`is_rdata_file()`](https://benwiseman.github.io/roperators/reference/file_checks.md)
  [`is_rda_file()`](https://benwiseman.github.io/roperators/reference/file_checks.md)
  [`is_rds_file()`](https://benwiseman.github.io/roperators/reference/file_checks.md)
  [`is_spss_file()`](https://benwiseman.github.io/roperators/reference/file_checks.md)
  [`check_ext_against()`](https://benwiseman.github.io/roperators/reference/file_checks.md)
  : File Extension Checks

## Vectors, extraction & statistics

Pull elements and words out of vectors, count uniques, build sequences,
and run complete-cases summaries.

- [`get_1st()`](https://benwiseman.github.io/roperators/reference/time_savers.md)
  [`get_last()`](https://benwiseman.github.io/roperators/reference/time_savers.md)
  [`get_nth()`](https://benwiseman.github.io/roperators/reference/time_savers.md)
  [`get_1st_word()`](https://benwiseman.github.io/roperators/reference/time_savers.md)
  [`get_last_word()`](https://benwiseman.github.io/roperators/reference/time_savers.md)
  [`get_nth_word()`](https://benwiseman.github.io/roperators/reference/time_savers.md)
  [`get_most_frequent()`](https://benwiseman.github.io/roperators/reference/time_savers.md)
  [`get_most_frequent_word()`](https://benwiseman.github.io/roperators/reference/time_savers.md)
  : Get the first, last, n-th, or most frequent element or word
- [`n_unique()`](https://benwiseman.github.io/roperators/reference/n_unique.md)
  : Count the number of unique values
- [`seq_around()`](https://benwiseman.github.io/roperators/reference/seq_around.md)
  : Sequence of evenly spaced points around an origin
- [`length_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md)
  [`n_unique_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md)
  [`min_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md)
  [`max_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md)
  [`range_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md)
  [`all_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md)
  [`any_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md)
  [`sum_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md)
  [`prod_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md)
  [`mean_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md)
  [`median_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md)
  [`var_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md)
  [`cov_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md)
  [`cor_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md)
  [`sd_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md)
  [`weighted.mean_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md)
  [`quantile_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md)
  [`IQR_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md)
  [`mad_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md)
  [`rowSums_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md)
  [`colSums_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md)
  [`rowMeans_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md)
  [`colMeans_cc()`](https://benwiseman.github.io/roperators/reference/complete_cases.md)
  : Statistics/Summaries with (Only) Missing Data Removed

## System, environment & I/O

Detect the operating system and R environment, check file types, and
read delimited files.

- [`get_os()`](https://benwiseman.github.io/roperators/reference/os.md)
  [`get_R_version()`](https://benwiseman.github.io/roperators/reference/os.md)
  [`get_R_version_age()`](https://benwiseman.github.io/roperators/reference/os.md)
  [`get_latest_CRAN_version()`](https://benwiseman.github.io/roperators/reference/os.md)
  [`get_system_python()`](https://benwiseman.github.io/roperators/reference/os.md)
  [`is.os_mac()`](https://benwiseman.github.io/roperators/reference/os.md)
  [`is.os_win()`](https://benwiseman.github.io/roperators/reference/os.md)
  [`is.os_lnx()`](https://benwiseman.github.io/roperators/reference/os.md)
  [`is.os_unx()`](https://benwiseman.github.io/roperators/reference/os.md)
  [`is.os_x64()`](https://benwiseman.github.io/roperators/reference/os.md)
  [`is.os_arm()`](https://benwiseman.github.io/roperators/reference/os.md)
  [`is.R_x64()`](https://benwiseman.github.io/roperators/reference/os.md)
  [`is.R_revo()`](https://benwiseman.github.io/roperators/reference/os.md)
  [`is.RStudio()`](https://benwiseman.github.io/roperators/reference/os.md)
  [`is.http_available()`](https://benwiseman.github.io/roperators/reference/os.md)
  : Operating system and environment checks
- [`read.tsv()`](https://benwiseman.github.io/roperators/reference/read.tsv.md)
  [`read.psv()`](https://benwiseman.github.io/roperators/reference/read.tsv.md)
  : Read tab- or pipe-separated files
- [`library.force()`](https://benwiseman.github.io/roperators/reference/library.force.md)
  [`require.force()`](https://benwiseman.github.io/roperators/reference/library.force.md)
  : Load a package, installing it first if necessary
