# Read tab- or pipe-separated files

Convenience wrappers around
[`read.table`](https://rdrr.io/r/utils/read.table.html) for
tab-separated (`read.tsv`) and pipe-separated (`read.psv`) files. Both
default to `header = TRUE`, like `read.csv`.

## Usage

``` r
read.tsv(file, ...)

read.psv(file, ...)
```

## Arguments

- file:

  path of the file to load

- ...:

  further arguments passed to
  [`read.table`](https://rdrr.io/r/utils/read.table.html)

## Value

A `data.frame`.
