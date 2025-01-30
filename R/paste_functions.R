# helepers for creating nicer messages/strings

#' New Paste and Cat Rules
#'
#' @description
#' The available functions are:
#'
#' `paste_()`is the same as `paste0` but uses an underscore to separate
#'
#' `cat0()` is analogous to `paste0` but for cat
#'
#' `catN()` is the same as `cat0` but automatically inserts a new line after the cat
#'
#' `paste_series()` paste a series of things with a conjunction
#'
#' `paste_oxford()` shortcut for `paste_series` as oxford comma
#'
#' @param conjunction the conjunction to use to collapse the final elements
#'        in the series (such as and, or, &, or something else)
#' @param use_oxford_comma whether to use the oxford comma in the series
#'        (standard in American English) or to not use the oxford comma
#' @inheritParams base::paste
#' @inheritParams base::cat
#' @author Steven Nydick, \email{steven.nydick@@kornferry.com}
#' @name paste_and_cat


#' @rdname paste_and_cat
#' @export
paste_ <- function(...,
                   collapse = NULL){
  paste(...,
        sep      = "_",
        collapse = collapse)
}


#' @rdname paste_and_cat
#' @param file character - A connection, or a character string naming the file to print to. If "" (the default), cat prints to the standard output connection, the console unless redirected by sink.
#' @param fill 	a logical or (positive) numeric controlling how the output is broken into successive lines. see `?cat`
#' @param labels character vector of labels for the lines printed. Ignored if fill is FALSE.
#' @param sep a character vector of strings to append after each element
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
}

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
}


#' @examples
#'
#' paste_series("a")
#' paste_series("a", "b")
#' paste_series("a", "b", "c")
#' # works if putting entries into c function
#' paste_series(c("a", "b", "c"), "d")
#' # can use oxford comma or not
#' paste_series("a", "b", "c",
#'              use_oxford_comma = TRUE)
#' paste_series("a", "b", "c",
#'              use_oxford_comma = FALSE)
#' # makes no difference if fewer than 3 items
#' paste_series("a", "b",
#'              use_oxford_comma = TRUE)
#' @rdname paste_and_cat
#' @param  conjunction indicates the ending conjunction. e.g. setting to "and" would make c("a", "b", "c") paste into "a, b, and c"
#' @param use_oxford_comma logical - do you want to use an oxford comma at the end?
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
  }

  # indicate the index for oxford or not
  # - take all but last 1 IF oxford
  # - take all but last 2 IF not oxford
  sep_idx    <- seq_len(n - 2 + isTRUE(use_oxford_comma))

  # add the commas and the "and" part
  x[sep_idx] <- paste0(x[sep_idx], sep)
  x[n]       <- paste(conjunction, x[n])

  # paste everything together
  paste(x, collapse = " ")
}

#' @rdname paste_and_cat
#' @export
paste_oxford <- function(...){
  paste_series(...)
}
