# File Extension Checks

Check whether file extension is as specified

## Usage

``` r
is_txt_file(x)

is_csv_file(x)

is_excel_file(x)

is_r_file(x)

is_rdata_file(x)

is_rda_file(x)

is_rds_file(x)

is_spss_file(x)

check_ext_against(x, ext = "txt")
```

## Arguments

- x:

  file(s) to be tested

- ext:

  extension to test against

## Value

a logical value

## Note

These only check the file extension and not the contents of the file.
Checking the contents of a file might come later but would be quite a
bit more involved. You can use \`readr\` or \`readxl\` (for example) to
check the file contents.

## Examples

``` r
# create your own file extension checks
is_word_file <- function(x){
  check_ext_against(x, ext = c("doc", "docx"))
}
is_word_file(c("blah.doc", "blah.docx", "blah.txt"))
#> [1]  TRUE  TRUE FALSE
```
