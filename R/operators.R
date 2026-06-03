#' @include type_checks.R
NULL

#### String operators   -----------------
#' String arithmetic operators
#'
#' @description
#' Perform string concatenation and arithmetic in a similar way to other
#' languages. String addition (\code{\%+\%}) glues strings together, string
#' subtraction (\code{\%-\%}) removes a pattern, string multiplication
#' (\code{\%s*\%}) repeats a string, and string division (\code{\%s/\%}) counts how
#' many times a pattern occurs. String division has no equivalent in languages
#' like 'Python', yet is arguably more useful than string multiplication, and it
#' accepts regular expressions.
#'
#' @param x a string (character vector)
#' @param y a string (character vector or regular expression)
#'
#' @return A character vector for \code{\%+\%}, \code{\%-\%}, and \code{\%s*\%}, and an
#'   integer count for \code{\%s/\%}.
#'
#' @examples
#' ("ab" %+% "c") == "abc" # TRUE
#' ("abc" %-% "b") == "ac" # TRUE
#' ("ac" %s*% 2) == "acac" # TRUE
#' ("acac" %s/% "c") == 2  # TRUE
#' # String division with a regular expression:
#' "an apple a day keeps the malignant spirit of Steve Jobs at bay" %s/% "Steve Jobs|apple"
#'
#' @author Ben Wiseman, \email{benjamin.h.wiseman@@gmail.com}
#' @name string_arithmetic
NULL

#' @rdname string_arithmetic
#' @export
`%+%`   <- function(x, y) {
  paste0(x, y)
}

#' @rdname string_arithmetic
#' @export
`%-%`   <- function(x, y) {
  unname(mapply(function(x,y) gsub(pattern = y, replacement = '', x = x), x, y))
}

# Added in ulrike's suggestion for making compatible with vector or scalar input
#' @rdname string_arithmetic
#' @export
`%s*%`  <- function(x, y) {
  mapply(function(x,y)  paste0(rep(x, y), collapse = ''), x, y)
}

#' @rdname string_arithmetic
#' @export
`%s/%`  <- function(x, y) {
  out <- mapply(function(x, y) lengths(regmatches(x, gregexpr(y, x))), x, y)
  names(out) <- y
  return(out)
  #lengths(regmatches(x, gregexpr(y, x)))
}


#### For assignment operators  ------------------

#### Apply mathematical operator and reassignment

#' Assignment (in-place modifier) operators
#'
#' @description
#' Modify the stored value of the left-hand-side object in place. These are the
#' equivalent of operators such as \code{+=}, \code{-=}, \code{*=}, and
#' \code{/=} in languages like C++ or 'Python'. \code{\%+=\%} and \code{\%-=\%} also
#' work with strings, and \code{\%-=\%} accepts regular expressions.
#'
#' @param x a stored value
#' @param y value to modify the stored value by
#'
#' @return Used for the side effect of reassigning \code{x} in the calling
#'   environment; returns the new value of \code{x} invisibly.
#'
#' @author Ben Wiseman, \email{benjamin.h.wiseman@@gmail.com}
#' @examples
#' x <- 1
#' x %+=% 2
#' x == 3 # TRUE
#'
#' x %-=% 3
#' x == 0 # TRUE
#'
#' # Or with data frames...
#' test <- iris
#'
#' # Simply modify in place
#' test$Sepal.Length[test$Species == 'setosa' & test$Petal.Length < 1.5] %+=% 1
#'
#' # Which is much nicer than typing:
#' test$Sepal.Length[test$Species == 'setosa' & test$Petal.Length < 1.5] <-
#' test$Sepal.Length[test$Species == 'setosa' & test$Petal.Length < 1.5] + 1
#' # ...which is over the 100 character limit for R documentation!
#'
#' # %+=% and %-=% also work with strings
#' x <- "ab"
#' x %+=% "c"
#' x %-=% "b"
#' x == "ac" # TRUE
#'
#' # %-=% can also take regular expressions
#' x <- "foobar"
#' x %-=% "[fb]"
#' print(x)
#' # "ooar"
#' @name assign_ops
NULL

#' @rdname assign_ops
#' @export
`%+=%`   <- function(x, y){
  if(is.character(x) | is.character(y)){
    #.create_operator(x, y, `%+%`)
    v_name  <- substitute(x)
    v_value <- paste0(x,y)
    eval(call("<-", v_name, v_value), envir = parent.frame(n = 1))
  } else {
    #.create_operator(x, y, `+`)
    v_name  <- substitute(x)
    v_value <- x+y
    eval(call("<-", v_name, v_value), envir = parent.frame(n = 1))
  }
}

#' @rdname assign_ops
#' @export
`%-=%`  <- function(x, y){
  if(is.character(x) | is.character(y)){
    #.create_operator(x, y, `%-%`)
    v_name  <- substitute(x)
    v_value <- gsub(y, '', x)
    eval(call("<-", v_name, v_value), envir = parent.frame(n = 1))
  } else{
      #.create_operator(x, y, `-`)
      v_name  <- substitute(x)
      v_value <- x-y
      eval(call("<-", v_name, v_value), envir = parent.frame(n = 1))
  }
}

#' @rdname assign_ops
#' @export
`%*=%`   <- function(x, y) {
  #.create_operator(x, y, `*`)
  v_name  <- substitute(x)
  v_value <- x*y
  eval(call("<-", v_name, v_value), envir = parent.frame(n = 1))
}

#' @rdname assign_ops
#' @export
`%/=%`   <- function(x, y) {
  #.create_operator(x, y, `/`)
  v_name  <- substitute(x)
  v_value <- x/y
  eval(call("<-", v_name, v_value), envir = parent.frame(n = 1))
}

#' @rdname assign_ops
#' @export
`%^=%`   <- function(x, y) {
  #.create_operator(x, y, `^`)
  v_name  <- substitute(x)
  v_value <- x^y
  eval(call("<-", v_name, v_value), envir = parent.frame(n = 1))
}

#' @rdname assign_ops
#' @export
`%log=%` <- function(x, y){
  #.create_operator(x, y, log)
  v_name  <- substitute(x) #with user defined functions, can't run in parent frame
  v_value <- log(x, y)
  eval(call("<-", v_name, v_value), envir = parent.frame(n = 1))
}

#' @rdname assign_ops
#' @export
`%root=%`<- function(x, y) {
  v_name  <- substitute(x)
  v_value <- x^(1/y)
  eval(call("<-", v_name, v_value), envir = parent.frame(n = 1))
}


#' Modify an object in place by regular expression
#'
#' @description
#' This takes two arguments, just like \code{gsub}: a pattern and a replacement.
#' It overwrites only the matched portion of each element of \code{x}. If you
#' want to overwrite whole elements that match (rather than just the matched
#' portion), use \code{\%regex<-\%} instead.
#'
#' @param x a character vector
#' @param value a length-2 character vector of the form \code{c(pattern, replacement)}
#'
#' @return Used for the side effect of reassigning \code{x} in the calling
#'   environment; returns the modified \code{x} invisibly.
#'
#' @examples
#' x <- c("a1b", "b1", "c", "d0")
#'
#' # change any digit to "x"
#' x %regex=% c("\\d+", "x")
#' print(x)
#' # "axb" "b" "c" "dx"
#'
#' @author Ben Wiseman, \email{benjamin.h.wiseman@@gmail.com}
#' @rdname overwrite_by_regex
#' @export
`%regex=%`<- function(x, value){
  if(length(value) != 2) warning("roperators: \n right-hand-side isn't length 2 but it MUST be in the form c(pattern, replacement)")
  v_name  <- substitute(x) #with user defined functions, can't run in parent frame
  v_value <- gsub(value[1], value[2], x)
  eval(call("<-", v_name, v_value), envir = parent.frame(n = 1))
}


####### CONDITIONAL ASSIGNMENT    ----------------------------------------------


#' Assign to a vector only where a regular expression matches
#'
#' @description
#' This takes two arguments, just like \code{gsub}: a pattern and a replacement.
#' It overwrites the \emph{entire} element wherever the pattern matches. If you
#' want to substitute only the matched portion, use \code{\%regex=\%} instead; to
#' replace matches with nothing (\code{""}), use \code{\%-\%} or \code{\%-=\%}.
#'
#' @param x a character vector
#' @param value a length-2 character vector of the form \code{c(pattern, replacement)}
#'
#' @return Used for the side effect of reassigning \code{x} in the calling
#'   environment; returns the modified \code{x} invisibly.
#'
#' @examples
#' x <- c("a1b", "b1", "c", "d0")
#'
#' # overwrite any element containing a digit
#' x %regex<-% c("\\d+", "x")
#' print(x)
#' # "x" "b" "c" "x"
#'
#' @author Ben Wiseman, \email{benjamin.h.wiseman@@gmail.com}
#' @rdname assign_by_regex
#' @export
`%regex<-%`<- function(x, value){
  if(length(value) != 2) warning("roperators: \n right-hand-side isn't length 2 but it MUST be in the form c(pattern, replacement)")
  v_name  <- substitute(x) #with user defined functions, can't run in parent frame
  idx <- grep(value[1], x)
  v_value <- x
  v_value[idx] <- value[2]
  eval(call("<-", v_name, v_value), envir = parent.frame(n = 1))
}




#' Assign a value to a vector's missing values
#'
#' @description
#' \code{\%na<-\%} is a shortcut to assign a value to all \code{NA} elements of
#' \code{x}. The replacement may be a single value (recycled to every \code{NA}),
#' a vector with one entry per missing element (filled in order), or a vector the
#' same length as \code{x} (its values at the missing positions are used).
#'
#' @param x a vector
#' @param value a single value, a vector with one entry per \code{NA}, or a
#'   vector the same length as \code{x}
#'
#' @return Used for the side effect of reassigning \code{x} in the calling
#'   environment; returns the modified \code{x} invisibly.
#'
#' @examples
#' x <- c("a", NA, "c")
#' x %na<-% "b"
#' print(x)
#' # "a" "b" "c"
#'
#' x <- c(1, NA, 3, NA)
#' x %na<-% c(2, 4)   # one replacement per NA, in order
#' print(x)
#' # 1 2 3 4
#'
#' @author Ben Wiseman, \email{benjamin.h.wiseman@@gmail.com}
#' @rdname overwrite_missing
#' @export
`%na<-%` <- function(x, value) {
  v_name <- substitute(x)
  i      <- which(is.na(x))
  if (is.scalar(value)) {
    # single value, recycled to every NA
    x[i] <- value
  } else if (length(value) == length(i)) {
    # one replacement per missing element, in order
    x[i] <- value
  } else if (length(value) == length(x)) {
    # full-length replacement: take the values at the missing positions
    x[i] <- value[i]
  } else {
    warning("roperators: \n replacement length must be 1, the number of NAs (",
            length(i), "), or length(x) (", length(x), "); recycling instead")
    x[i] <- value
  }
  eval(call("<-", v_name, x), envir = parent.frame(n = 1))
}


#### Boolean return operators (because why not)  --------------------------

#' Comparison operators with better missing-value handling
#'
#' @description
#' A set of comparison operators that improve on base R by treating missing
#' values as comparable (so two \code{NA}s are considered equal) and by adding
#' convenient interval ("between") tests. The operators are:
#'
#' \itemize{
#'   \item \code{\%==\%} - equality that treats \code{NA == NA} as \code{TRUE}.
#'   \item \code{\%===\%} - strict equality of both value \emph{and} class, for
#'     those familiar with 'JavaScript' \code{===}.
#'   \item \code{\%>=\%}, \code{\%<=\%} - greater/less than or equal to, with
#'     missing-value equality.
#'   \item \code{\%><\%}, \code{\%>=<\%} - between, with the ends excluded or
#'     included respectively.
#' }
#'
#' For approximate (floating-point) comparisons, see
#' \code{\link{floating_point_comparisons}}.
#'
#' @param x a vector
#' @param y a vector (for the "between" operators, a length-2 vector of the form
#'   \code{c(lower, upper)})
#'
#' @return A logical vector.
#'
#' @examples
#'  ## Equality and ordering, with missing-value equality
#'  c(1, NA, 3, 4)  ==  c(1, NA, 4, 3)
#'  #  TRUE    NA  FALSE FALSE
#'
#'  c(1, NA, 3, 4) %==% c(1, NA, 4, 3)
#'  #  TRUE  TRUE  FALSE FALSE
#'
#'  c(1, NA, 3, 4) %>=% c(1, NA, 4, 3)
#'  #  TRUE  TRUE FALSE  TRUE
#'
#'  c(1, NA, 3, 4) %<=% c(1, NA, 4, 3)
#'  #  TRUE  TRUE  TRUE FALSE
#'
#'  ## Strict equality - a la 'JavaScript' ===
#'  # Only TRUE if the class AND value of x and y are the same
#'  x <- int(2)
#'  y <- 2
#'  x == y         # TRUE
#'  x %===% y      # FALSE
#'  x %===% int(y) # TRUE
#'
#'  ## Between
#'  # ends excluded
#'  2 %><% c(1, 3)  # TRUE
#'  3 %><% c(1, 3)  # FALSE
#'
#'  # ends included
#'  2 %>=<% c(1, 3) # TRUE
#'  3 %>=<% c(1, 3) # TRUE
#'
#' @author Ben Wiseman, \email{benjamin.h.wiseman@@gmail.com}
#' @name comparisons
NULL

#' @rdname comparisons
#' @family comparisons
#' @export
`%==%` <- function(x, y){
  bad_x <- is.bad_for_calcs(x)
  bad_y <- is.bad_for_calcs(y)

  ((x == y) & !bad_x & !bad_y) | is.bad_and_equal(x, y)
}

#' @rdname comparisons
#' @export
`%===%` <- function(x, y){
  bad_x <- is.bad_for_calcs(x)
  bad_y <- is.bad_for_calcs(y)

  class(x) == class(y) & (((!bad_x & !bad_y) & (x == y)) | is.bad_and_equal(x, y))
}

# >= with missings as false
#' @rdname comparisons
#' @export
`%>=%` <- function(x, y){
  bad_x <- is.bad_for_calcs(x)
  bad_y <- is.bad_for_calcs(y)
  ((x >= y) & !bad_x & !bad_y) | is.bad_and_equal(x, y)
}

# <= with missings as false
#' @rdname comparisons
#' @export
`%<=%` <- function(x, y){
  bad_x <- is.bad_for_calcs(x)
  bad_y <- is.bad_for_calcs(y)
  ((x <= y) & !bad_x & !bad_y) | is.bad_and_equal(x, y)
}

#' between (ends excluded)
#' @rdname comparisons
#' @export
`%><%` <- function(x, y){
  if(length(y) != 2) warning("roperators: \n  right-hand-side isn't length 2 but it MUST be in the form c(lower_bound, upper_bound)")
  b <- range(y)
  x > b[1] & x < b[2]
}

#' between (ends included)
#' @rdname comparisons
#' @export
`%>=<%`<- function(x, y){
  if(length(y) != 2) warning("roperators: \n right-hand-side isn't length 2 but it MUST be in the form c(lower_bound, upper_bound)")
  b <- range(y)
  x >= b[1] & x <= b[2]
}

#' Floating-point comparison operators
#'
#' @description
#' An important set of operators missing from base R. Using \code{==} on two
#' non-integer numbers can give unexpected results (see examples), because of the
#' way floating-point numbers are represented. These operators instead test
#' equality up to a small tolerance, via \code{\link[base]{all.equal}}.
#'
#' For a fuller explanation, see
#' \url{https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html}.
#'
#' @param x numeric
#' @param y numeric
#'
#' @return A logical value.
#'
#' @examples
#'  ## Floating-point test of equality
#'  # base R:
#'  (0.1 + 0.1 + 0.1) == 0.3   # FALSE
#'  # with roperators:
#'  (0.1 + 0.1 + 0.1) %~=% 0.3 # TRUE
#'
#'  # Note how the base >= and <= behave here:
#'  (0.1 + 0.1 + 0.1) %>=% 0.3 # TRUE
#'  (0.1 + 0.1 + 0.1) %<=% 0.3 # FALSE
#'
#'  # Use %>~% and %<~% for greater/less than OR approximately equal
#'  (0.1 + 0.1 + 0.1) %>~% 0.3 # TRUE
#'  (0.1 + 0.1 + 0.1) %<~% 0.3 # TRUE
#'
#' @author Ben Wiseman, \email{benjamin.h.wiseman@@gmail.com}
#' @name floating_point_comparisons
NULL

#' @rdname floating_point_comparisons
#' @family comparisons
#' @export
`%~=%` <- function(x, y) {
  isTRUE(all.equal(x, y))
}

# >= with missings as false and approx equal
#' @rdname floating_point_comparisons
#' @export
`%>~%` <- function(x, y){
  bad_x <- is.bad_for_calcs(x)
  bad_y <- is.bad_for_calcs(y)
  ((x >= y) & !bad_x & !bad_y) | is.bad_and_equal(x, y) | isTRUE(all.equal(x, y))
}

# <= with missings as false and approx equal
#' @rdname floating_point_comparisons
#' @export
`%<~%` <- function(x, y){
  bad_x <- is.bad_for_calcs(x)
  bad_y <- is.bad_for_calcs(y)
  ((x <= y) & !bad_x & !bad_y) | is.bad_and_equal(x, y) | isTRUE(all.equal(x, y))
}

# Logicals
#' Logical operators
#'
#' @description
#' A few convenience logical operators: "not in" (\code{\%ni\%}), exclusive or
#' (\code{\%xor\%}), and all-or-nothing (\code{\%aon\%}, which is \code{TRUE} when
#' \code{x} and \code{y} are both \code{TRUE} or both \code{FALSE}).
#'
#' @param x a vector
#' @param y a vector
#'
#' @return A logical vector.
#'
#' @examples
#'  #### Not in ####
#'  "z" %ni% c("a", "b", "c") # TRUE
#'
#'  #### Exclusive or ####
#'  TRUE  %xor% TRUE  # FALSE
#'  FALSE %xor% FALSE # FALSE
#'  FALSE %xor% TRUE  # TRUE
#'
#'  #### All-or-nothing ####
#'  TRUE  %aon% TRUE  # TRUE
#'  FALSE %aon% FALSE # TRUE
#'  FALSE %aon% TRUE  # FALSE
#'
#' @author Ben Wiseman, \email{benjamin.h.wiseman@@gmail.com}
#' @name logicals
NULL

# Not in
#' @rdname logicals
#' @export
`%ni%` <- function(x, y) {
  !(x %in% y)
}

# Exclusive or
#' @rdname logicals
#' @export
`%xor%`<- function(x, y){
  xor(x, y)
}

# All or nothing
#' @rdname logicals
#' @export
`%aon%`<- function(x, y){
  (x && y) || (!x && !y)
}



#' SQL-style pattern-matching operators
#'
#' @description
#' Convenience operators for regular-expression matching, inspired by SQL's
#' \code{LIKE}. Each takes a character vector \code{x} and a single pattern, and
#' returns a logical vector the same length as \code{x}.
#'
#' \code{\%rlike\%} matches case-insensitively, equivalent to
#' \code{grepl(pattern, x, ignore.case = TRUE)}.
#'
#' \code{\%perl\%} matches case-sensitively using Perl-compatible regular
#' expressions, equivalent to \code{grepl(pattern, x, perl = TRUE)}.
#'
#' @note
#' If you are working with \code{data.table}, prefer its own (faster)
#' \code{\%like\%} operator.
#'
#' @param x a character vector
#' @param pattern a single character expression (regular expression)
#'
#' @return A logical vector the same length as \code{x}.
#'
#' @examples
#' x <- c("foo", "bar", "dOe", "rei", "mei", "obo")
#'
#' # case-insensitive: where x contains an "o" (any case)
#' x[x %rlike% "O"]
#' # [1] "foo" "dOe" "obo"
#'
#' # case-sensitive Perl matching: middle letter is an upper-case "O"
#' x[x %perl% "[a-z]O[a-z]"]
#' # [1] "dOe"
#'
#' @author Ben Wiseman, \email{benjamin.h.wiseman@@gmail.com}
#' @name pattern_matching
NULL

#' @rdname pattern_matching
#' @export
`%rlike%`<- function(x, pattern){
  if(length(pattern) != 1) rop_stop("right-hand-side isn't length 1 but it MUST be")
  grepl(pattern, x, ignore.case = TRUE)
}

#' @rdname pattern_matching
#' @export
`%perl%`<- function(x, pattern){
  if(length(pattern) != 1) rop_stop("right-hand-side isn't length 1 but it MUST be")
  grepl(pattern, x, ignore.case = FALSE, perl = TRUE)
}



##### Mathematical operators
#' Choose and permute operators
#'
#' @description
#' Shorthand infix operators for common combinatorics: \code{n \%C\% k} gives the
#' number of combinations ("n choose k"), and \code{n \%P\% k} gives the number of
#' permutations ("n permute k").
#'
#' @param n whole number (the \code{n} in "n choose/permute k")
#' @param k whole number (the \code{k} in "n choose/permute k")
#'
#' @return A numeric value.
#'
#' @examples
#' 5 %C% 3 # 10  (5 choose 3)
#' 5 %P% 3 # 60  (5 permute 3)
#'
#' @author Ben Wiseman, \email{benjamin.h.wiseman@@gmail.com}
#' @name choose_permute
NULL
#' operator for combinations
#' @rdname choose_permute
#' @export
"%C%" <- function(n, k) {
  choose(n, k)
}

#' operator for permutations
#' @rdname choose_permute
#' @export
"%P%" <- function(n, k) {
  factorial(n) / factorial(n - k)
}


#' Inline integration operator
#'
#' @description
#' An inline call to \code{\link[stats]{integrate}} that returns the value of
#' the integral directly, rather than the usual list.
#'
#' @param f a function with a numeric return value
#' @param range a length-2 numeric vector, \code{c(lower, upper)}
#'
#' @return A single numeric value: the value of the integral.
#'
#' @examples
#' f <- function(x) x^2
#' f %integrate% c(0, 1) # 0.3333333
#'
#' # compared with base R, which returns a list:
#' str(integrate(f, 0, 1))
#'
#' @author Ben Wiseman, \email{benjamin.h.wiseman@@gmail.com}
#' @name integrate
NULL

#' @rdname integrate
#' @export
"%integrate%" <- function(f, range) {
  x <- integrate(f, range[1], range[2])
  return(x$value)
}

