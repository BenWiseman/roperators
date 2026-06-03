# helepers for creating nicer messages/strings

#' Paste and cat helpers
#'
#' @description
#' A small family of \code{paste}/\code{cat} conveniences:
#'
#' \itemize{
#'   \item \code{paste_()} - like \code{paste0()}, but separates with an
#'     underscore.
#'   \item \code{cat0()} - like \code{paste0()}, but for \code{cat} (no
#'     separator).
#'   \item \code{catN()} - like \code{cat0()}, but appends a new line.
#'   \item \code{paste_series()} - paste a series of items together with a
#'     conjunction, e.g. \code{"a, b, and c"}.
#'   \item \code{paste_oxford()} - a shortcut for \code{paste_series()} using an
#'     Oxford comma.
#' }
#'
#' @param conjunction the conjunction used to join the final elements of a
#'   series, such as \code{"and"}, \code{"or"}, or \code{"&"}
#' @param use_oxford_comma logical; whether to use the Oxford comma (standard in
#'   American English) before the conjunction
#' @inheritParams base::paste
#' @inheritParams base::cat
#'
#' @return \code{paste_()}, \code{paste_series()}, and \code{paste_oxford()}
#'   return a character vector. \code{cat0()} and \code{catN()} are called for
#'   their side effect (printing) and return \code{NULL} invisibly.
#'
#' @author Steven Nydick, \email{steven.nydick@@kornferry.com}
#' @name paste_and_cat
NULL


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
