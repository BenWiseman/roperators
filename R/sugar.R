# Syntactic sugar: small, opinionated quality-of-life helpers and operators.

#' String interpolation (f-strings for R)
#'
#' @description
#' Interpolate R expressions into a string, like 'Python' f-strings. Anything
#' inside curly braces is evaluated in the calling environment and inserted into
#' the string. Doubled braces are treated as a single literal brace.
#'
#' @param ... one or more strings containing \code{{expr}} placeholders
#' @param .envir the environment in which to evaluate the placeholders
#'   (defaults to the calling environment)
#'
#' @return A character vector the same length as the input, with every
#'   \code{{expr}} replaced by its evaluated, comma-collapsed value.
#'
#' @note
#' \code{f} is also a popular name for throwaway functions, so be aware it may
#' mask (or be masked by) a local \code{f} of your own.
#'
#' @examples
#' name <- "Ben"
#' n    <- 2
#' f("Hi {name}, you have {n} new messages")
#' f("{n} + {n} = {n + n}")
#'
#' # vectors are collapsed with ", "
#' f("today's letters: {head(LETTERS, n)}")
#'
#' @author Ben Wiseman, \email{benjamin.h.wiseman@@gmail.com}
#' @export
f <- function(..., .envir = parent.frame()) {
  lb <- "__ROP_OPEN_BRACE__"
  rb <- "__ROP_CLOSE_BRACE__"
  vapply(c(...), function(s) {
    # protect doubled braces so they can be emitted as single literal braces
    s <- gsub("{{", lb, s, fixed = TRUE)
    s <- gsub("}}", rb, s, fixed = TRUE)

    m    <- gregexpr("\\{[^{}]+\\}", s, perl = TRUE)
    hits <- regmatches(s, m)[[1]]

    if (length(hits)) {
      exprs <- substr(hits, 2L, nchar(hits) - 1L)
      vals  <- vapply(exprs, function(e) {
        paste(eval(parse(text = e), envir = .envir), collapse = ", ")
      }, character(1L))
      regmatches(s, m) <- list(vals)
    }

    s <- gsub(lb, "{", s, fixed = TRUE)
    s <- gsub(rb, "}", s, fixed = TRUE)
    s
  }, character(1L), USE.NAMES = FALSE)
}


#' Inline fallback for expressions that might error
#'
#' @description
#' Evaluate the left-hand side; if it raises an error, return the right-hand
#' side instead. The right-hand side is only evaluated when needed (lazily), so
#' an expensive or side-effecting fallback is safe to use.
#'
#' @param expr an expression to try
#' @param alternative value (or expression) to return if \code{expr} errors
#'
#' @return The value of \code{expr}, or of \code{alternative} if \code{expr}
#'   raised an error.
#'
#' @examples
#' sqrt(4)         %else% NA_real_     # 2
#' sqrt("a")       %else% NA_real_     # NA, instead of an error
#' (1:3)[[99]]     %else% "out of range"
#' stop("boom")    %else% "recovered"
#'
#' @author Ben Wiseman, \email{benjamin.h.wiseman@@gmail.com}
#' @rdname inline_fallback
#' @export
`%else%` <- function(expr, alternative) {
  tryCatch(expr, error = function(e) alternative)
}


#' Arithmetic convenience operators
#'
#' @description
#' \code{\%+-\%} builds a tolerance interval: \code{x \%+-\% y} returns
#' \code{c(x - y, x + y)}, which slots straight into the "between" operators in
#' \code{\link{comparisons}}. \code{\%/0\%} is a safe division that returns
#' \code{NA} instead of \code{Inf}/\code{NaN} when dividing by zero, so a stray
#' zero will not poison a downstream sum or mean.
#'
#' @param x a numeric value (or vector)
#' @param y a numeric value (or vector)
#'
#' @return \code{\%+-\%} returns a length-2 numeric vector
#'   \code{c(lower, upper)}; \code{\%/0\%} returns \code{x / y} with any
#'   divide-by-zero results replaced by \code{NA}.
#'
#' @examples
#' 5 %+-% 0.5             # 4.5 5.5
#' 4.9 %><% (5 %+-% 0.5)  # TRUE  - composes with the 'between' operator
#'
#' 10 %/0% 2                    # 5
#' 10 %/0% 0                    # NA (not Inf)
#' c(1, 2, 3) %/0% c(1, 0, 3)   # 1 NA 1
#'
#' @author Ben Wiseman, \email{benjamin.h.wiseman@@gmail.com}
#' @name arithmetic_sugar
NULL

#' @rdname arithmetic_sugar
#' @export
`%+-%` <- function(x, y) {
  c(x - y, x + y)
}

#' @rdname arithmetic_sugar
#' @export
`%/0%` <- function(x, y) {
  out <- x / y
  out[!is.na(y) & y == 0] <- NA_real_
  out
}


#' Fuzzy (case- and whitespace-insensitive) string equality
#'
#' @description
#' The string counterpart to the numeric operator \code{\%~=\%} (see
#' \code{\link{floating_point_comparisons}}). \code{x \%~\% y} is \code{TRUE}
#' when \code{x} and \code{y} are equal ignoring case, leading/trailing
#' whitespace, and runs of internal whitespace - handy for joining or matching
#' messy, hand-entered data.
#'
#' @param x a character vector
#' @param y a character vector
#'
#' @return A logical vector, with \code{NA} where either side is \code{NA}.
#'
#' @examples
#' "Foo " %~% "foo"                   # TRUE
#' "a  b" %~% "a b"                    # TRUE
#' c("Yes", "NO") %~% c("yes", "no")  # TRUE TRUE
#'
#' @author Ben Wiseman, \email{benjamin.h.wiseman@@gmail.com}
#' @rdname fuzzy_match
#' @export
`%~%` <- function(x, y) {
  norm <- function(s) tolower(trimws(gsub("\\s+", " ", as.character(s))))
  norm(x) == norm(y)
}


#' Format a proportion as a percentage string
#'
#' @description
#' Turn a proportion (such as \code{0.75}) into a human-friendly percentage
#' string (such as \code{"75\%"}).
#'
#' @param x a numeric proportion, or vector of proportions, where \code{1}
#'   represents one hundred percent
#' @param digits number of decimal places to show
#' @param ... further arguments passed to \code{\link{formatC}}
#'
#' @return A character vector of percentage strings.
#'
#' @examples
#' as.percent(0.75)              # "75.0\%"
#' as.percent(c(0.1, 0.005))     # "10.0\%" "0.5\%"
#' as.percent(2 / 3, digits = 0) # "67\%"
#'
#' @author Ben Wiseman, \email{benjamin.h.wiseman@@gmail.com}
#' @export
as.percent <- function(x, digits = 1, ...) {
  paste0(formatC(x * 100, format = "f", digits = digits, ...), "%")
}
