# These are just space savers

#' Cleaner type-conversion functions
#'
#' @description
#' Short aliases for the most common \code{as.*} conversions. There is nothing
#' magical here, but the shorter names can make data-wrangling code much easier
#' to read, especially for users coming from other languages. \code{as.class()}
#' additionally converts to a class chosen by name at run time.
#'
#' @param x value to be converted
#' @param ... further arguments passed to the underlying \code{as.*} function
#' @param class character; the name of the class to convert \code{x} to (used by
#'   \code{as.class})
#'
#' @return The value of \code{x} coerced to the requested type.
#'
#' @examples
#' chr(42)    # "42" = as.character()
#' int(42.1)  # 42L  = as.integer()
#' dbl("42")  # 42   = as.double()
#' num("42")  # 42   = as.numeric()
#' bool(42)   # TRUE = as.logical()
#'
#' @author Steven Nydick, \email{steven.nydick@@kornferry.com}
#' @author Ben Wiseman, \email{benjamin.h.wiseman@@gmail.com}
#' @export
#' @rdname cleaner_conversions
chr  <- function(x, ...) as.character(x, ...)

#' @export
#' @rdname cleaner_conversions
int  <- function(x, ...)  as.integer(x, ...)

#' @export
#' @rdname cleaner_conversions
dbl <- function(x, ...) as.double(x, ...)

#' @export
#' @rdname cleaner_conversions
num <- function(x, ...) as.numeric(x, ...)

#' @export
#' @rdname cleaner_conversions
bool  <- function(x, ...) as.logical(x, ...)



#### Conversion shorthands

#' @rdname cleaner_conversions
#' @export
#' @examples
#'
#' # as.class() converts to an arbitrary class chosen by name:
#' as.class(255, "roman") # CCLV
as.class <- function(x, class){
  get(paste0("as.", class), mode = "function")(x)
}


#' Convert a factor with numeric labels into a numeric vector
#'
#' @description
#' Converting a factor with \code{as.numeric()} returns the underlying integer
#' codes, not the labels, which is rarely what you want when the labels are
#' themselves numbers. \code{f.as.numeric()} returns the labels as numbers
#' instead.
#'
#' @param x a factor with numeric labels
#'
#' @return A numeric vector of the factor's labels.
#'
#' @examples
#' x <- factor(c(11, 22, 33, 99))
#'
#' as.numeric(x)
#' # 1 2 3 4      # the integer codes - NOT usually what you want
#'
#' f.as.numeric(x)
#' # 11 22 33 99  # the labels as numbers - usually what you want
#'
#' # equivalent to the clunkier base idiom:
#' as.numeric(as.character(x))
#'
#' @author Ulrike Grömping, \email{groemping@@beuth-hochschule.de}
#' @author Ben Wiseman, \email{benjamin.h.wiseman@@gmail.com}
#' @export
#' @rdname factor_conversion
f.as.numeric <- function(x){
  as.numeric(levels(x))[as.numeric(x)]
  #as.numeric(as.character(x)) # for factors
}


#### Time Savers
#' Get the first, last, n-th, or most frequent element or word
#'
#' @description
#' Small helpers for pulling pieces out of vectors and strings - handy inside
#' \code{apply}-style calls. \code{get_1st()}, \code{get_last()}, and
#' \code{get_nth()} extract elements of a vector; the \code{*_word} variants
#' split strings into words first; and \code{get_most_frequent()} /
#' \code{get_most_frequent_word()} return the most common value(s).
#'
#' @param x an R object, usually a vector or character string
#' @param type \code{'v'} (default) to index as a vector (\code{x[1]}), or
#'   \code{'l'} to index as a list (\code{x[[1]]})
#' @param n integer; the n-th element or word to select
#' @param split character used to separate words (default \code{' '})
#' @param collapse optional character; if supplied, the result is pasted into a
#'   single string using this separator
#'
#' @return The selected element(s) or word(s). \code{get_most_frequent*()}
#'   return the most common value(s), as a character vector unless \code{x} is
#'   numeric and \code{collapse} is not used.
#'
#' @examples
#' # a list of split-up car names
#' car_names <- strsplit(row.names(mtcars)[1:5], " ")
#'
#' sapply(car_names, get_1st)
#' # [1] "Mazda" "Mazda" "Datsun" "Hornet" "Hornet"
#'
#' sapply(car_names, get_nth, 2)
#' # [1] "RX4" "RX4" "710" "4" "Sportabout"
#'
#' # Or pull a simple string apart (e.g. someone's full name):
#' get_1st_word(rownames(mtcars)[1:5])
#' # [1] "Mazda" "Mazda" "Datsun" "Hornet" "Hornet"
#'
#' get_last_word(rownames(mtcars)[1:5])
#' # [1] "RX4" "Wag" "710" "Drive" "Sportabout"
#'
#' get_nth_word(rownames(mtcars)[1:5], 2)
#' # [1] "RX4" "RX4" "710" "4" "Sportabout"
#'
#' @author Ben Wiseman, \email{benjamin.h.wiseman@@gmail.com}
#' @export
#' @rdname time_savers
get_1st <- function(x, type = 'v') {
  if(type %in% c('l', "list")) x[[1]]
  else x[1]
}

#' @export
#' @rdname time_savers
get_last <- function(x, type = 'v') {
  if(type %in% c('l', "list")) x[[length(x)]]
  else x[length(x)]
}

#' @export
#' @rdname time_savers
get_nth <- function(x, n = 1, type = 'v') {
  if(type %in% c('l', "list")) x[[n]]
  else x[n]
}

#' @export
#' @rdname time_savers
get_1st_word <- function(x, type = 'v', split = ' ') {
  unlist(lapply(strsplit(x, split), get_1st, type))
}

#' @export
#' @rdname time_savers
get_last_word <- function(x, type = 'v', split = ' ') {
  unlist(lapply(strsplit(x, split), get_last, type))
}

#' @export
#' @rdname time_savers
get_nth_word <- function(x, n = 1, type = 'v', split = ' ') {
  unlist(lapply(strsplit(x, split), get_nth, n=n, type=type))
}


#' @examples
#'
#' # get_most_frequent() returns the mode(s)
#' my_stuff <- c(1:10, 10, 5)
#' get_1st(my_stuff)           # 1
#' get_nth(my_stuff, 3)        # 3
#' get_last(my_stuff)          # 5
#' get_most_frequent(my_stuff) # the modes (5 and 10), as a numeric vector
#'
#' my_chars <- c("a", "b", "b", "a", "g", "o", "l", "d")
#' get_most_frequent(my_chars)                    # "a" "b"
#' get_most_frequent(my_chars, collapse = " & ")  # "a & b"
#' @export
#' @rdname time_savers
get_most_frequent <- function(x, collapse = NULL){
  x_class   <- class(x)
  x_summary <- sort(summary(as.factor(as.character(x))), decreasing = TRUE)
  out       <- names(x_summary[x_summary==max(x_summary, na.rm = TRUE)])
  #out      <- as.class(out, x_class) weird with factors

  # if collapsing
  if(!is.null(collapse)) out <- paste(unique(out), collapse = collapse)
  if(is.null(collapse) && is.numeric(x)) out <- as.numeric(out)

  return(out )
}


#' @param ignore.punct logical; ignore punctuation marks
#' @param ignore.case logical; ignore case (if \code{TRUE}, the result is lower-case)
#' @param punct.regex character; regex used to strip punctuation (default \code{[[:punct:]]})
#' @param punct.replace character; what to replace punctuation with (default \code{""})
#' @examples
#'
#' # the *_word helpers split a string into words first
#' generic_string <- "Who's A good boy? Winston's a good boy!"
#' get_1st_word(generic_string)    # "Who's"
#' get_nth_word(generic_string, 3) # "good"
#' get_last_word(generic_string)   # "boy!"
#'
#' # default ignores case and punctuation
#' get_most_frequent_word(generic_string)
#' # keep case and punctuation:
#' get_most_frequent_word(generic_string, ignore.case = FALSE, ignore.punct = FALSE)
#'
#' @export
#' @rdname time_savers
get_most_frequent_word <- function(x,
                                   ignore.punct = TRUE,
                                   ignore.case  = TRUE,
                                   split = " ",
                                   collapse = NULL,
                                   punct.regex = "[[:punct:]]",
                                   punct.replace = ""
                                   ){
  x_class   <- class(x)
  if(x_class != "character") x <- as.character(x)
  if(ignore.case) x <- tolower(x)
  if(ignore.punct) x <- gsub(punct.regex, punct.replace, x)

  x_words <- unlist(strsplit(x, split))

  x_summary <- sort(summary(as.factor(as.character(x_words))), decreasing = TRUE)

  out       <- names(x_summary[x_summary==max(x_summary, na.rm = TRUE)])
  #out      <- as.class(out, x_class)

  # if collapsing
  if(!is.null(collapse)) out <- paste(unique(out), collapse = collapse)

  return(out )
}


#' Count the number of unique values
#'
#' @description
#' Returns the number of unique elements in \code{x}, optionally ignoring
#' \code{NA}s.
#'
#' @param x a vector
#' @param na.rm logical; if \code{TRUE}, \code{NA}s are ignored when counting
#'   unique values
#'
#' @return An integer count of unique values.
#'
#' @examples
#' n_unique(c(1, 2, 2, 3, NA))               # 4
#' n_unique(c(1, 2, 2, 3, NA), na.rm = TRUE) # 3
#'
#' @author Ben Wiseman, \email{benjamin.h.wiseman@@gmail.com}
#' @export
#' @rdname n_unique
n_unique <- function(x, na.rm = FALSE){
  if(isTRUE(na.rm)){
    calc_len <- length_cc
  } else{
    calc_len <- length
  }
  calc_len(unique(x))
}



#' Sequence of evenly spaced points around an origin
#'
#' @description
#' Returns a vector of \code{n} points evenly spaced around \code{origin}, with
#' the given \code{spacing} between neighbours.
#'
#' @param origin number to centre the sequence on
#' @param n number of points to create (a single whole number)
#' @param spacing distance between any two neighbouring points
#'
#' @return A numeric vector. Defaults to \code{1} when called with no arguments,
#'   to mirror the default behaviour of \code{\link[base]{seq}}.
#'
#' @examples
#' seq_around(0, n = 5, spacing = 1) # -2 -1  0  1  2
#' seq_around(10, n = 3)             # 9.75 10.00 10.25
#'
#' @author Ben Wiseman, \email{benjamin.h.wiseman@@gmail.com}
#' @export
#' @rdname seq_around
seq_around <- function(origin = 1, n = 1, spacing = 0.25){
  if(length(n) != 1 || n %% 1 != 0) rop_stop("Must provide a single whole number as the n argument")
  # easy cases
  if(n==1) return(origin)
  if(n>=2) {
    y <- origin +  seq(from = -((n-1)/2)*spacing, to = ((n-1)/2)*spacing, length.out = n )
    return(y)
  }
  # clearly n is broken or negative
  rop_stop("Could not generate numeric sequence in seq_around() - check arguments.")

}


#' Read tab- or pipe-separated files
#'
#' @description
#' Convenience wrappers around \code{\link[utils]{read.table}} for tab-separated
#' (\code{read.tsv}) and pipe-separated (\code{read.psv}) files. Both default to
#' \code{header = TRUE}, like \code{read.csv}.
#'
#' @param file path of the file to load
#' @param ... further arguments passed to \code{\link[utils]{read.table}}
#'
#' @return A \code{data.frame}.
#'
#' @rdname read.tsv
#' @export
read.tsv <- function(file, ...){
  utils::read.table(file, header = TRUE, sep = '\t', ...)
}

#' @rdname read.tsv
#' @export
read.psv <- function(file, ...){
  utils::read.table(file, header = TRUE, sep = '|', ...)
}



#' Load a package, installing it first if necessary
#'
#' @description
#' Attempts to load \code{pkg}; if it is not installed, installs it (from CRAN
#' by default) and then loads it. \code{require.force} is an alias for
#' \code{library.force}.
#'
#' @param pkg name of the package to load or install
#' @param ... further arguments passed to \code{\link[utils]{install.packages}}
#'
#' @return Invisibly returns \code{NULL}; called for the side effect of loading
#'   (and possibly installing) \code{pkg}.
#'
#' @rdname library.force
#' @export
library.force <- function(pkg, ...){
  # Try to load the package
  installed <- require(pkg, character.only = TRUE)

  # If the package is not installed, install it
  if (!installed) {
    utils::install.packages(pkg, ...)
    # Try to load the package again after installing
    library(pkg, character.only = TRUE)
  }
}

#' @rdname library.force
#' @export
require.force <- function(pkg, ...) {
  library.force(pkg, ...)
}
