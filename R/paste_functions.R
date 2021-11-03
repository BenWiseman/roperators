#' New Paste and Cat Rules
#'
#' @inheritParams base::paste
#' @inheritParams base::cat
#' @param conjunction the conjunction to use to collapse the final elements
#'        in the series (such as and, or, &, or something else)
#' @param use_oxford_comma whether to use the oxford comma in the series
#'        (standard in American English) or to not use the oxford comma
#'
#' @details paste_ is the same as paste0 but uses an underscore to separate
#'          values, cat0 is analogous to paste0 but for cat, and catN is the
#'          same as cat0 but automatically inserts a new line after the cat
#'          statement (so is similar to message).
#'
#' @author Steven Nydick, \email{steven.nydick@@kornferry.com}
#'
#' @examples
#' # works if adding entries sequentially
#' paste_series("a")
#' paste_series("a", "b")
#' paste_series("a", "b", "c")
#'
#' # works if putting entries into c function
#' paste_series(c("a", "b", "c"), "d")
#'
#' # can use oxford comma or not
#' paste_series("a", "b", "c",
#'              use_oxford_comma = TRUE)
#' paste_series("a", "b", "c",
#'              use_oxford_comma = FALSE)
#'
#' # makes no difference if fewer than 3 items
#' paste_series("a", "b",
#'              use_oxford_comma = TRUE)
#'
#' @name paste_and_cat
NULL

#' @rdname paste_and_cat
#' @export
paste_ <- function(...,
                   collapse = NULL){
  paste(...,
        sep      = "_",
        collapse = collapse)
} # END paste_ FUNCTION


#' @rdname paste_and_cat
#' @export
cat0 <- function(...,
                 file   = "",
                 fill   = FALSE,
                 labels = NULL,
                 append = FALSE){
  cat(...,
      sep    = "",
      file   = file,
      fill   = fill,
      labels = labels,
      append = append)
} # END cat0 FUNCTION

#' @rdname paste_and_cat
#' @export
catN <- function(...,
                 file   = "",
                 fill   = FALSE,
                 labels = NULL,
                 append = FALSE){
  cat0(..., "\n",
       file   = file,
       fill   = fill,
       labels = labels,
       append = append)
} # END catN FUNCTION

#' @rdname paste_and_cat
#' @export
paste_series <- function(...,
                         sep = c(",", ";"),
                         conjunction = c("and", "or", "&"),
                         use_oxford_comma = TRUE){

  # make sure sep is appropriate and x is appropriate
  # (and define last separator)
  sep         <- sep[1]
  conjunction <- conjunction[1]

  # combine everything to form x and determine total number of entries
  x           <- as.character(c(...))
  n           <- length(x)

  # return early if we only have 0-to-2 entries (no need for sep then)
  if(n <= 2){
    return(paste(x, collapse = paste0(" ", conjunction, " ")))
  } # END ifelse STATEMENT

  # indicate the index for oxford or not
  # - take all but last 1 IF oxford
  # - take all but last 2 IF not oxford
  sep_idx    <- seq_len(n - 2 + isTRUE(use_oxford_comma))

  # add the commas and the "and" part
  x[sep_idx] <- paste0(x[sep_idx], sep)
  x[n]       <- paste(conjunction, x[n])

  # paste everything together
  paste(x,
        collapse = " ")
} # END paste_comma FUNCTION

#' @rdname paste_and_cat
#' @export
paste_oxford <- paste_series
