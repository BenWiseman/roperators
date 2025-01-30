#' @include type_checks.R
NULL

#### String operators   -----------------
#' String operators
#'
#' @description
#' Perform string concatenation and arithmetic is a similar way to other languages.
#'  String division is not present in languages like Python, although arguably it is
#'  more useful than string multiplication and can be used with regulr expressions.
#'
#' @param x a string
#' @param y a string
#'
#' @examples
#' ("ab" %+% "c") == "abc" # TRUE
#' ("abc" %-% "b") == "ac" # TRUE
#' ("ac" %s*% 2) == "acac" # TRUE
#' ("acac" %s/% "c") == 2  # TRUE
#' # String division with a regular expression:
#' 'an apple a day keeps the malignant spirit of Steve Jobs at bay' %s/% 'Steve Jobs|apple'
#'
#' @author Ben Wiseman, \email{benjamin.wiseman@@kornferry.com}
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

#' Assignment operators
#'
#' @description
#' Modifies the stored value of the left-hand-side object by the right-hand-side object.
#' Equivalent of operators such as \code{+=} \code{-=} \code{*=} \code{/=} in languages like c++ or python.
#' \code{%+=%} and \code{%-=%} can also work with strings.
#'
#' @param x a stored value
#' @param y value to modify stored value by
#'
#' @author Ben Wiseman, \email{benjamin.wiseman@@kornferry.com}
#' @examples
#' x <- 1
#'
#' x %+=% 2
#'
#' x == 3 # TRUE
#'
#' x %-=% 3
#'
#' x == 0 # TRUE
#'
#' # Or with data frames...
#' test <- iris
#'
#' # Simply modify in-place
#' test$Sepal.Length[test$Species == 'setosa' & test$Petal.Length < 1.5] %+=% 1
#'
#' # Which is much nicer than typing:
#' test$Sepal.Length[test$Species == 'setosa' & test$Petal.Length < 1.5] <-
#' test$Sepal.Length[test$Species == 'setosa' & test$Petal.Length < 1.5] + 1
#' # ...which is over the 100 character limit for R doccumentation!
#'
#' # %+=% and %-=% also work with strings
#'
#'    x <- "ab"
#'
#'    x %+=% "c"
#'
#'    x %-=% "b"
#'
#'    x == "ac" # TRUE
#'
#' # %-=% can also take regular expressions
#'
#'    x <- "foobar"
#'
#'    x %-=% "[f|b]"
#'
#'    print(x)
#'    # "ooar"
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


#' Modify existing object by regular expression
#'
#' @description
#' This takes two arguments just like \code{gsub} - a patterns and a replacement.
#' It will only overwrite the parts of any character where the pattern is matched with the second argument.
#' If you want to overwrite whole elements via a regex (i.e. replace the entire element if it matches),
#' use \code{%regex<-%} instead.
#'
#' @param x a character vector
#' @param value c(pattern, replacement)
#'
#' @examples
#' # Apply a regular expression/substitution to x:
#'
#'  x <- c("a1b", "b1", "c", "d0")
#'
#'  # change any number to "x"
#'
#'   x %regex=% c("\\d+", "x")
#'
#'  print(x)
#'
#'  # "axb" "b" "c" "dx"
#' @author Ben Wiseman, \email{benjamin.wiseman@@kornferry.com}
#' @rdname overwrite_by_regex
#' @export
`%regex=%`<- function(x, value){
  if(length(value) != 2) warning("roperators: \n right-hand-side isn't length 2 but it MUST be in the form c(pattern, replacement)")
  v_name  <- substitute(x) #with user defined functions, can't run in parent frame
  v_value <- gsub(value[1], value[2], x)
  eval(call("<-", v_name, v_value), envir = parent.frame(n = 1))
}


####### CONDITIONAL ASSIGNMENT    ----------------------------------------------


#' Assign to vector only where regular expression is matched
#'
#' @description
#' This takes two arguments just like \code{gsub} - a patterns and a replacement.
#' It will totally overwrite any element where the pattern is matched with the second.
#' If you want to simply apply a regex (i.e. replace only the specific bit that matches),
#' use \code{%regex=%} instead. If you want to replace with nothing (""), just just \code{%-%} or
#' \code{%-=% instead}.
#'
#' @param x a character vector
#' @param value c(pattern, replacement)
#'
#' @examples
#' # Overwrite elements that match regex:
#'
#'  x <- c("a1b", "b1", "c", "d0")
#'
#'  # overwrite any element containing a number
#'
#'  x %regex<-% c("\\d+", "x")
#'
#'  print(x)
#'
#'  # "x" "b" "c" "x"
#' @author Ben Wiseman, \email{benjamin.wiseman@@kornferry.com}
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




#' Assign value to a vector's missing values
#'
#' @description
#' \code{%na<-%} is a simple shortcut to assign a specific value to all
#' NA elements contained in x.
#'
#' @param x a vector
#' @param value value to replace vector's missing values with
#'
#' @examples
#'  x <- c("a", NA, "c")
#'
#'  x %na<-% "b"
#'
#'  print(x)
#'  # "a" "b" "c"
#'
#'  x <- c(1, NA, 3, NA)
#'
#'  x %na<-% c(2,4)
#'
#'  print(x)
#'  # 1 2 3 4
#' @author Ben Wiseman, \email{benjamin.wiseman@@kornferry.com}
#' @rdname overwrite_missing
#' @export
`%na<-%` <- function(x, value) {
  v_name <- substitute(x)
  if (is.scalar(value)) {
    x[is.na(x)] <- value
  } else {
    i <- which(is.na(x))
    if(length(x) != length(value)){
      warning('roperators: \n using a replacement vector of unequal size')
    }
    x[i] <- value[i]
  }
  v_value <- x
  eval(call("<-", v_name, v_value), envir = parent.frame(n = 1))
}


#### Boolean return operators (because why not)  --------------------------

#' Enhanced comparisons
#'
#' @description
#' These operators introduce improved NA handling, reliable floating point tests,
#' and intervals. Specifically:
#'
#'  Equality that handles missing values
#'
#'  Floating point equality, an important bit of functionality missing in base R (\code{%~=%})
#'
#'  Strict (value and type) equality, for those familiar with Javascript \code{===}
#'
#'  Greater/less than or equal to with missing value equality
#'
#'  Greater/less than or equal to with floating point and missing equality
#'
#'  Between (ends excluded)
#'
#'  Between (ends included)
#'
#' @param x a vector
#' @param y a vector
#'
#' @examples
#'  ## Greater/Less than | Equal
#'
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
#'  #  TRUE  TRUE TRUE  FALSE
#'
#'
#'  # Strict equality - a la javascript's ===
#'  # Only tru if the class and value of x and y are the same
#' x <- int(2)
#' y <- 2
#' x == y         # TRUE
#' x %===% y      # FALSE
#' x %===% int(y) # TRUE
#'
#'
#'  # NOTE parentheses surrounding expression before this operator are necessary
#'  # Without parentheses it would be interpreted as .1 + .1 + (.1 %~=% .3)
#'
#'
#'  #### Between ####
#'
#'  # ends excluded
#'
#'  2 %><% c(1, 3)
#'  # TRUE
#'
#'  3 %><% c(1, 3)
#'  # FALSE
#'
#'  # ends included
#'
#'  2 %>=<% c(1, 3)
#'  # TRUE
#'
#'  3 %>=<% c(1, 3)
#'  # TRUE
#'
#' @author Ben Wiseman, \email{benjamin.wiseman@@kornferry.com}
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
  x > y[1] & x < y[2]
}

#' between (ends included)
#' @rdname comparisons
#' @export
`%>=<%`<- function(x, y){
  if(length(y) != 2) warning("roperators: \n right-hand-side isn't length 2 but it MUST be in the form c(lower_bound, upper_bound)")
  x >= y[1] & x <= y[2]
}

#'  Floating point comparison operators
#'
#'@description
#'  These are an important set of operators missing from base R. In particular,
#'  using \code{==} on two non-interger numbers can give unexpected results (see examples.)
#'
#'  See this for details:
#'  \url{https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html}
#' @param x numeric
#' @param y numeric
#'
#' @examples
#'  ## Floating point test of equality ####
#'
#'  # Basic Equality - no roperators:
#'  (0.1 + 0.1 + 0.1) == 0.3   # FALSE
#'  # Basic Equality - with roperators:
#'  (0.1 + 0.1 + 0.1) %~=% 0.3 # TRUE
#'
#'
#'  # NOTE: for floating point >= and <=
#'  (0.1 + 0.1 + 0.1) %>=% 0.3 # TRUE
#'  (0.1 + 0.1 + 0.1) %<=% 0.3 # FALSE
#'
#'  # Use >~ and <~ for greater/less than or approx equal
#'  (0.1 + 0.1 + 0.1) %>~% 0.3 # TRUE
#'  (0.1 + 0.1 + 0.1) %<~% 0.3 # TRUE
#'
#' @author Ben Wiseman, \email{benjamin.wiseman@@kornferry.com}
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
#' These are some convienience functions, such as a not-in, and xor operator.
#'
#' @param x a vector
#' @param y a vector
#'
#' @examples
#'  #### Not in ####
#'
#'  "z" %ni%  c("a", "b", "c")
#'  #  TRUE
#'
#'  #### Exclusive or  ####
#'
#'  TRUE %xor% TRUE
#'  # FALSE
#'
#'  FALSE %xor% FALSE
#'  # FALSE
#'
#'  FALSE %xor% TRUE
#'  # TRUE
#'
#'  #### All-or-nothing ####
#'
#'  TRUE %aon% TRUE
#'  # TRUE
#'
#'  FALSE %aon% FALSE
#'  # TRUE
#'
#'  FALSE %aon% TRUE
#'  # FALSE
#'
#' @author Ben Wiseman, \email{benjamin.wiseman@@kornferry.com}
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



#' mySQL-like rlike
#'
#' @description
#' This takes two arguments just like \code{grepl} - a string and a pattern.
#' TRUE if \code{grepl(pattern, x, ignore.case=TRUE)} would be TRUE
#'
#' @note
#' data.table has a %like% operator which you should try to use instead if working with data.table!
#'
#' @param x a character vector
#' @param pattern a single character expression
#'
#' @examples
#' # Apply a regular expression/substitution to x:
#'
#'  x <- c("foo", "bar", "dOe", "rei", "mei", "obo")
#'
#'  # where x has an O
#'
#'  x[x %rlike% "O"]
#'
#'  # [1] "foo" "dOe" "obo"
#'
#'  # find x where middle letter is "O"
#'
#'  x[x %rlike% "[a-z]O[a-z]"]
#'
#'  # will print [1] "foo" "dOe"
#'
#' @rdname logicals
#' @export
`%rlike%`<- function(x, pattern){
  if(length(pattern) != 1) rop_stop("right-hand-side isn't length 1 but it MUST be")
  grepl(pattern, x, ignore.case = TRUE)
}



#' SQL-like like
#'
#' @description
#' This takes two arguments just like \code{grepl} - a string and a pattern.
#' TRUE if \code{grepl(pattern, x, ignore.case=FALSE, perl=TRUE)} would be TRUE.
#' It's like \code{%like%} from data.table (but slower, preferably use data.table).
#'
#'
#' @param x a character vector
#' @param pattern a single character expression
#'
#' @examples
#' # Apply a regular expression/substitution to x:
#'
#'  x <- c("foo", "bar", "dOe", "rei", "mei", "obo")
#'
#'  # find x where middle letter is upper-case "O"
#'
#'  x[x %perl% "[a-z]O[a-z]"]
#'
#'  # will print [1] "dOe"
#'
#' @rdname logicals
#' @export
`%perl%`<- function(x, pattern){
  if(length(pattern) != 1) rop_stop("right-hand-side isn't length 1 but it MUST be")
  grepl(pattern, x, ignore.case=FALSE, perl = TRUE)
}



##### Mathematical operators
#' Choose and permute
#'
#' @description
#' Shorthand for some common mathematical operators
#'
#' @param n whole number (from n choose/permute k)
#' @param k whole number (from n choose/permute k)
#'
#' @examples
#' # Calculate 5 choose 3
#' print(5 %C% 3)
#' # Calculate 5 permute 3
#' print(5 %P% 3)
#'
#' @author Ben Wiseman, \email{benjamin.wiseman@@kornferry.com}
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


#' Inline integration
#' @description
#' inline call to integrate that returns integration value rather than list
#'
#' @param f function (with numeric return)
#' @param range vector of two numbers c(low, high)
#'
#' @examples
#' f <- function(x) x^2
#' print(f %integrate% c(0, 1))
#' # vs base
#' x <- integrate(f, 0, 1)
#' str(x)
#' @author Ben Wiseman, \email{benjamin.wiseman@@kornferry.com}
#' @name integrate
NULL

#' @rdname integrate
#' @export
"%integrate%" <- function(f, range) {
  x <- integrate(f, range[1], range[2])
  return(x$value)
}

