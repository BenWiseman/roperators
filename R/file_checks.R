#' File Extension Checks
#'
#' Check whether file extension is as specified
#'
#' @param x file(s) to be tested
#' @param ext extension to test against
#'
#' @note
#' These only check the file extension and not the contents of the file. Checking
#' the contents of a file might come later but would be quite a bit more involved.
#' You can use `readr` or `readxl` (for example) to check the file contents.
#'
#' @return a logical value
#'
#' @examples
#' # create your own file extension checks
#' is_word_file <- function(x){
#'   check_ext_against(x, ext = c("doc", "docx"))
#' }
#' is_word_file(c("blah.doc", "blah.docx", "blah.txt"))
#' @name file_checks
NULL

#' @rdname file_checks
#' @export
is_txt_file <- function(x){
  check_ext_against(x, ext = c("txt", "text", "dat"))
}

#' @rdname file_checks
#' @export
is_csv_file <- function(x){
  check_ext_against(x, ext = c("csv"))
} # END is_csv_file FUNCTION

#' @rdname file_checks
#' @export
is_excel_file <- function(x){
  check_ext_against(x, ext = c("xls", "xlsx"))
} # END is_excel_file FUNCTION

#' @rdname file_checks
#' @export
is_r_file <- function(x){
  check_ext_against(x, ext = c("r"))
} # END is_r_file FUNCTION

#' @rdname file_checks
#' @export
is_rdata_file <- function(x){
  check_ext_against(x, ext = c("rdata"))
} # END is_rdata_file CHECK

#' @rdname file_checks
is_rda_file <- function(x){
  check_ext_against(x, ext = c("rda"))
}

#' @rdname file_checks
is_rds_file <- function(x){
  check_ext_against(x, ext = c("rds"))
}

#' @rdname file_checks
#' @export
is_spss_file <- function(x){
  check_ext_against(x, ext = c("sav"))
} # END is_spss_file FUNCTION

#' @rdname file_checks
#' @export
check_ext_against <- function(x,
                              ext = "txt"){
  tolower(tools::file_ext(x)) %in% ext
} # END check_ext_against FUNCTION
