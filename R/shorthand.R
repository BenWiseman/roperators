# These are just space savers

#' Cleaner conversion functions
#'
#' @param x Value to be converted
#' @param ... other args for as. conversion
#'
#' @author Steven Nydick, \email{steven.nydick@@kornferry.com}
#' @author Ben Wiseman, \email{benjamin.wiseman@@kornferry.com}
#'
#'
#' @note
#' These are shorthand aliases for common conversions There is nothing magical
#' here, but it can make your code more readable
#'
#' @examples
#' chr(42)   # "42" = as.character
#' int(42.1) # 42L  = as.integer
#' dbl("42L") # 42.0 = as.double
#' num("42") # 42   = as.numeric
#' bool(42)  # TRUE = as.logical
#'
#' @author Ben Wiseman, \email{benjamin.wiseman@@kornferry.com}
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

#' convert x to arbitrary class
#' @param x object to be converted
#' @param class chatracter name of the class to convert x to
#' @examples
#'
#' foo <- 255
#' as.class(foo, "roman")
#' # [1] CCLV
#' @rdname cleaner_conversions
#' @author Ben Wiseman, \email{benjamin.wiseman@@kornferry.com}
#' @export
as.class <- function(x, class){
  eval(parse(text = paste0("as.", class, "('", as.character(x), "')")))
}


#' Convert factor with numeric labels into numeric vector
#' @param x a factor with numeric labels
#' @author Ulrike Grömping, \email{groemping@@beuth-hochschule.de}
#'
#' @examples
#'
#'  x <- factor(c(11, 22, 33, 99))
#'  as.numeric(x)
#'  # 1 2 3 4   # NOT typically the desired.expected output
#'
#'  f.as.numeric(x)
#'  # 11 22 33 99  # Typically desired output
#'
#'  # Or...
#'  as.numeric(as.character(x)) # A tad unsightly
#'
#' @author Ben Wiseman, \email{benjamin.wiseman@@kornferry.com}
#' @export
#' @rdname factor_conversion
f.as.numeric <- function(x){
  as.numeric(levels(x))[as.numeric(x)]
  #as.numeric(as.character(x)) # for factors
}


#### Time Savers
#' Little functions to replace common minor functions. useful in apply sttements
#' @param x an R object, usually a character vector
#' @param type 'v' (default) for vector `x[1]`; 'l' for list `x[[1]]`
#'
#' @examples
#' unlist(lapply(strsplit(rownames(mtcars), ' '), get_1st))
#' #[1] "Mazda"    "Mazda"    "Datsun"   "Hornet"   "Hornet"   "Valiant"  "Duster"   "Merc"     "Merc"     "Merc"     "Merc"     "Merc"
#' #[13] "Merc"     "Merc"     "Cadillac" "Lincoln"  "Chrysler" "Fiat"     "Honda"    "Toyota"   "Toyota"   "Dodge"    "AMC"      "Camaro"
#' #[25] "Pontiac"  "Fiat"     "Porsche"  "Lotus"    "Ford"     "Ferrari"  "Maserati" "Volvo"
#'
#' unlist(lapply(strsplit(rownames(mtcars), ' '), get_nth, 2))
#' #[1] "RX4"         "RX4"         "710"         "4"           "Sportabout"  NA            "360"         "240D"        "230"
#' #[10] "280"         "280C"        "450SE"       "450SL"       "450SLC"      "Fleetwood"   "Continental" "Imperial"    "128"
#' #[19] "Civic"       "Corolla"     "Corona"      "Challenger"  "Javelin"     "Z28"         "Firebird"    "X1-9"        "914-2"
#' #[28] "Europa"      "Pantera"     "Dino"        "Bora"        "142E"
#'
#' # OR if you just want to pull a simple string apart (e.g. someone's full name):
#'
#' get_1st_word(rownames(mtcars))
#' #[1] "Mazda"    "Mazda"    "Datsun"   "Hornet"   "Hornet"   "Valiant"  "Duster"   "Merc"     "Merc"     "Merc"     "Merc"     "Merc"
#' #[13] "Merc"     "Merc"     "Cadillac" "Lincoln"  "Chrysler" "Fiat"     "Honda"    "Toyota"   "Toyota"   "Dodge"    "AMC"      "Camaro"
#' #[25] "Pontiac"  "Fiat"     "Porsche"  "Lotus"    "Ford"     "Ferrari"  "Maserati" "Volvo"
#'
#' get_last_word(rownames(mtcars))
#' #[1] "RX4"         "Wag"         "710"         "Drive"       "Sportabout"  "Valiant"     "360"         "240D"        "230"
#' #[10] "280"         "280C"        "450SE"       "450SL"       "450SLC"      "Fleetwood"   "Continental" "Imperial"    "128"
#' #[19] "Civic"       "Corolla"     "Corona"      "Challenger"  "Javelin"     "Z28"         "Firebird"    "X1-9"        "914-2"
#' #[28] "Europa"      "L"           "Dino"        "Bora"        "142E"
#'
#' @author Ben Wiseman, \email{benjamin.wiseman@@kornferry.com}
#' @export
#' @rdname time_savers
get_1st <- function(x, type = 'v') {
  if(type %in% c('l', "list")) x[[1]]
  else x[1]
}

#' @export
#' @param n integer, the nth element to select
#' @rdname time_savers
get_last <- function(x, type = 'v') {
  if(type %in% c('l', "list")) x[[length(x)]]
  else x[length(x)]
}

#' @export
#' @param n integer, the nth element to select
#' @rdname time_savers
get_nth <- function(x, n = 1, type = 'v') {
  if(type %in% c('l', "list")) x[[n]]
  else x[n]
}

#' @export
#' @param split character that separated words. Default = ' '
#' @rdname time_savers
get_1st_word <- function(x, type = 'v', split = ' ') {
  unlist(lapply(strsplit(x, split), get_1st, type))
}

#' @export
#' @param split character that separated words. Default = ' '
#' @rdname time_savers
get_last_word <- function(x, type = 'v', split = ' ') {
  unlist(lapply(strsplit(x, split), get_last, type))
}

#' @export
#' @param n integer, the nth word to select
#' @param split character that separated words. Default = ' '
#' @rdname time_savers
get_nth_word <- function(x, n = 1, type = 'v', split = ' ') {
  unlist(lapply(strsplit(x, split), get_nth, n=n, type=type))
}


#' Get most common thing(s)
#' @param  x a vector of things
#' @param  collapse OPTIONAL character - paste output into single string with collapse
#' @return a vector of most common element(s). Will be character unless x is numeric and you don't tell it to collapse into a single string!
#' @examples
#' my_stuff <- c(1:10, 10, 5)
#' # These are straight forward
#' get_1st(my_stuff)
#' get_nth(my_stuff, 3)
#' get_last(my_stuff)
# Returns numeric vector of mode(s) if x is numeric
#' get_most_frequent(my_stuff)
# Else it returns a character vector
#' my_chars <- c("a", "b", "b", "a", "g", "o", "l", "d")
#' get_most_frequent(my_chars)
# can collapse into a single string (for convienience)
#' get_most_frequent(my_chars, collapse = " & ")
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


#' @param  x a string
#' @param  ignore.punct logical - ignore punctuation marks
#' @param  ignore.case logical - ignore case (if true, will return in lower)
#' @param  split character that separated words. Default = ' '
#' @param  collapse OPTIONAL character - paste output into single string with collapse
#' @param  punct.regex character - regex used to remove punctuation (by default `[[:punct:]]`)
#' @param  punct.replace character - what to replace punctuation with (default is "")
#' @return a vector of most common element(s). Will be character unless x is numeric and you don't tell it to collapse into a single string!
#' @examples
#' generic_string <- "Who's A good boy? Winston's a good boy!"
#'
#'get_1st_word(generic_string)
#'get_nth_word(generic_string, 3)
#'get_last_word(generic_string)
#'# default ignores case and punctuation
#'get_most_frequent_word(generic_string)
#'# can change like so:
#'get_most_frequent_word(generic_string, ignore.case = FALSE, ignore.punct = FALSE)
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


#' Return numbetr of unique things in x
#' @export
#' @param x vector
#' @param na.rm whether to ignore NAs when determining uniqueness
#' @rdname time_savers
n_unique <- function(x, na.rm = FALSE){
  if(isTRUE(na.rm)){
    calc_len <- length_cc
  } else{
    calc_len <- length
  }
  calc_len(unique(x))
}


#' like read.csv, but for tsv and default header = TRUE
#' @param file path of file you want to load
#' @param ...  other args used by read.table
#' @rdname read.tsv
#' @export
read.tsv <- function(file, ...){
  utils::read.table(file, header = TRUE, sep = '\t', ...)
}

#' like read.csv, but for pipe-delineated and defaults to header = TRUE
#' @param file path of file you want to load
#' @param ...  other args used by read.table
#' @rdname read.tsv
#' @export
read.psv <- function(file, ...){
  utils::read.table(file, header = TRUE, sep = '|', ...)
}
