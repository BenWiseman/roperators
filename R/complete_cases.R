#' Statistics/Summaries with (Only) Missing Data Removed
#'
#' Univariate and bivariate summaries and statistics with the least missing
#' data removed (such as complete-cases correlations). These are typically
#' default arguments to standard statistics functions.
#'
#' @inheritParams base::mean
#' @inheritParams base::sum
#' @inheritParams stats::cor
#' @inheritParams stats::weighted.mean
#' @param rescale whether to rescale the matrix/df/vector before calculating
#'        summaries
#' @param ... arguments to pass to wrapped functions
#'
#' @examples
#' n_o <- 20
#' n_m <- round(n_o / 3)
#' x   <- rnorm(n_o)
#' y   <- rnorm(n_o)
#'
#' x[sample(n_o, n_m)] <- NA
#' y[sample(n_o, n_m)] <- NA
#'
#' mean_cc(x)   # mean of complete cases
#' mean_cc(y)
#' var_cc(x)    # variance of complete cases
#' var_cc(y)
#' cor_cc(x, y) # correlation between available cases
#'
#' @import stats
#' @name complete_cases
NULL

# BASE #

#' @rdname complete_cases
#' @export
length_cc <- function(x, ...){
  if(is.atomic(x)){
    sum(!is.na(x))
  } else{
    length(x, ...)
  } # END ifelse STATEMENT
}

#' @rdname complete_cases
n_unique_cc <- function(x, ...){
  n_unique(x, ..., na.rm = TRUE)
}

# SUMMARIES #

#' @rdname complete_cases
#' @export
min_cc <- function(x, ...){
  min(x, ..., na.rm = TRUE)
}

#' @rdname complete_cases
#' @export
max_cc <- function(x, ...){
  max(x, ..., na.rm = TRUE)
}

#' @rdname complete_cases
#' @export
range_cc <- function(x, ...){
  range(x, ..., na.rm = TRUE)
}

#' @rdname complete_cases
#' @export
all_cc <- function(x, ...){
  all(x, ..., na.rm = TRUE)
}

#' @rdname complete_cases
#' @export
any_cc <- function(x, ...){
  any(x, ..., na.rm = TRUE)
}

#' @rdname complete_cases
#' @export
sum_cc <- function(x, ...){
  sum(x, ..., na.rm = TRUE)
}

#' @rdname complete_cases
#' @export
prod_cc <- function(x, ...){
  prod(x, ..., na.rm = TRUE)
}

# STATISTICS #

#' @rdname complete_cases
#' @export
mean_cc <- function(x, ...){
  mean(x, ..., na.rm = TRUE)
}

#' @rdname complete_cases
#' @export
median_cc <- function(x, ...){
  stats::median(x, ..., na.rm = TRUE)
}

#' @rdname complete_cases
#' @export
var_cc <- function(x, y = NULL, ...){
  stats::var(x, ..., na.rm = TRUE)
}

#' @rdname complete_cases
#' @export
cov_cc <- function(x, y = NULL, ...){
  stats::cov(x, y, ..., use = "pairwise.complete.obs")
}

#' @rdname complete_cases
#' @export
cor_cc <- function(x, y = NULL, ...){
  stats::cor(x, y, ..., use = "pairwise.complete.obs")
}

#' @rdname complete_cases
#' @export
sd_cc <- function(x, ...){
  stats::sd(x, ..., na.rm = TRUE)
}

#' @rdname complete_cases
#' @export
weighted.mean_cc <- function(x, w, ...){
  stats::weighted.mean(x, w, ..., na.rm = TRUE)
}

#' @rdname complete_cases
#' @export
quantile_cc <- function(x, ...){
  stats::quantile(x, ..., na.rm = TRUE)
}

#' @rdname complete_cases
#' @export
IQR_cc <- function(x, ...){
  stats::IQR(x, ..., na.rm = TRUE)
}

#' @rdname complete_cases
#' @export
mad_cc <- function(x, ...){
  stats::mad(x, ..., na.rm = TRUE)
}

# TWO WAY FUNCTIONS #

#' @rdname complete_cases
#' @export
rowSums_cc <- function(x, ...){
  rowSums(x, ..., na.rm = TRUE)
}

#' @rdname complete_cases
#' @export
colSums_cc <- function(x, ...){
  colSums(x, ..., na.rm = TRUE)
}

#' @rdname complete_cases
#' @export
rowMeans_cc <- function(x,
                        ...,
                        rescale = FALSE){

  # rescale if required
  if(rescale){
    x <- scale(x)
  }

  rowMeans(x, ..., na.rm = TRUE)
}

#' @rdname complete_cases
#' @export
colMeans_cc <- function(x,
                        ...,
                        rescale = FALSE){

  # rescale if required
  if(rescale){
    x <- scale(x)
  }

  colMeans(x, ..., na.rm = TRUE)
}
