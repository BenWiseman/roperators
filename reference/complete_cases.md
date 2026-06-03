# Statistics/Summaries with (Only) Missing Data Removed

Univariate and bivariate summaries and statistics with the least missing
data removed (such as complete-cases correlations). These are typically
default arguments to standard statistics functions.

## Usage

``` r
length_cc(x, ...)

n_unique_cc(x, ...)

min_cc(x, ...)

max_cc(x, ...)

range_cc(x, ...)

all_cc(x, ...)

any_cc(x, ...)

sum_cc(x, ...)

prod_cc(x, ...)

mean_cc(x, ...)

median_cc(x, ...)

var_cc(x, y = NULL, ...)

cov_cc(x, y = NULL, ...)

cor_cc(x, y = NULL, ...)

sd_cc(x, ...)

weighted.mean_cc(x, w, ...)

quantile_cc(x, ...)

IQR_cc(x, ...)

mad_cc(x, ...)

rowSums_cc(x, ...)

colSums_cc(x, ...)

rowMeans_cc(x, ..., rescale = FALSE)

colMeans_cc(x, ..., rescale = FALSE)
```

## Arguments

- x:

  an R object. Currently there are methods for numeric/logical vectors
  and [date](https://rdrr.io/r/base/Dates.html),
  [date-time](https://rdrr.io/r/base/DateTimeClasses.html) and [time
  interval](https://rdrr.io/r/base/difftime.html) objects. Complex
  vectors are allowed for `trim = 0`, only.

- ...:

  arguments to pass to wrapped functions

- y:

  `NULL` (default) or a vector, matrix or data frame with compatible
  dimensions to `x`. The default is equivalent to `y = x` (but more
  efficient).

- w:

  a numerical vector of weights the same length as `x` giving the
  weights to use for elements of `x`.

- rescale:

  whether to rescale the matrix/df/vector before calculating summaries

## Value

The same value as the base/stats function each one wraps (for example a
numeric summary, vector, or matrix), but computed with missing values
removed by default.

## Examples

``` r
n_o <- 20
n_m <- round(n_o / 3)
x   <- rnorm(n_o)
y   <- rnorm(n_o)

x[sample(n_o, n_m)] <- NA
y[sample(n_o, n_m)] <- NA

mean_cc(x)   # mean of complete cases
#> [1] -0.2644023
mean_cc(y)
#> [1] 0.1887549
var_cc(x)    # variance of complete cases
#> [1] 1.439278
var_cc(y)
#> [1] 1.09564
cor_cc(x, y) # correlation between available cases
#> [1] 0.2796028

# the row/column helpers also drop NAs by default
m <- matrix(c(1, NA, 3, 4, 5, 9), nrow = 2)
rowMeans_cc(m)
#> [1] 3.0 6.5
colSums_cc(m)
#> [1]  1  7 14

# colMeans_cc()/rowMeans_cc() can z-score each column first via rescale = TRUE
colMeans_cc(matrix(1:6, nrow = 3), rescale = TRUE)
#> [1] 0 0
```
