# roperators 1.2.0

* Cleaned up roxygen comments, DESCRIPTION, and NAMESPACE files.
* Added a `NEWS.md` file to track changes to the package.
* New complete cases functions (`length_cc()`, `min_cc()`, `max_cc()`,
  `range_cc()`, `all_cc()`, `any_cc()`, `sum_cc()`, `prod_cc()`, `mean_cc()`,
  `median_cc()`, `var_cc()`, `cov_cc()`, `cor_cc()`, `sd_cc()`,
  `weighted_mean_cc()`, `quantile_cc()`, `IQR_cc()`, `mad_cc()`,
  `rowSums_cc()`, `colSums_cc()`, `rowMeans_cc()`, `colMeans_cc()`) that set
  default argument to standard base/stats functions for only calculating on
  complete cases.
* New `get_os()`, `is.os_mac`, `is.os_win`, `is.os_lnx`, and `is.os_unx` helper
  functions to quickly determine operating system.
* New `is.R_Revo()` and `is.RStudio()` functions to determine whether Revolution
  R and RStudio are installed.
* New `n_unique()` function to count unique items.
* New `get_most_frequent()` function to get most frequent thing(s) in x.
