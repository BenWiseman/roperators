# roperators 1.2.0

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
