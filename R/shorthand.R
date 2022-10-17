#' Cleaner Conversion Functions
#'
#' @param x Value to be converted
#' @param ... other args for as. conversion
#'
#' @author Ben Wiseman, \email{benjamin.wiseman@@kornferry.com}
#'
#' @note
#' These are shorthand aliases for common conversions There is nothing magical
#' here, but it can make your code more readable.
#'
#' @examples
#' chr(42)    # "42" = as.character
#' int(42.1)  # 42L  = as.integer
#' dbl("42L") # 42.0 = as.double
#' num("42")  # 42   = as.numeric
#' bool(42)   # TRUE = as.logical
#'
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

#' Convert x to Arbitrary Class
#'
#' @param x object to be converted
#' @param class chatracter name of the class to convert x to
#'
#' @examples
#' foo <- 255
#' as.class(foo, "roman")
#' # [1] CCLV
#' @rdname cleaner_conversions
#' @export
as.class <- function(x, class){
  eval(parse(text = paste0("as.", class, "('", as.character(x), "')")))
}

#' Convert Factor with Numeric Labels into Numeric Vector
#'
#' @param x a factor with numeric labels
#'
#' @examples
#' x <- factor(c(11, 22, 33, 99))
#' as.numeric(x)
#' # 1 2 3 4   # NOT typically the desired.expected output
#'
#' f.as.numeric(x)
#' # 11 22 33 99  # Typically desired output
#'
#' # Or...
#' as.numeric(as.character(x)) # A tad unsightly
#' @author Ulrike Grömping, \email{groemping@@beuth-hochschule.de}
#' @author Ben Wiseman, \email{benjamin.wiseman@@kornferry.com}
#' @export
#' @rdname factor_conversion
f.as.numeric <- function(x){
  as.numeric(levels(x))[as.numeric(x)]
}

#' Get Parts from Vectors
#'
#' @description
#' These are little functions to replace common minor functions and typically
#' useful in apply statements.
#'
#' @param x an R object, usually a character vector
#' @param type 'v' (default) for vector `x[1]`; 'l' for list `x[[1]]`
#' @param n integer, the nth element to select
#' @param split character that separated words. Default = ' '
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


#' Get Most Common Thing(s)
#'
#' @param x a vector of things
#' @param collapse OPTIONAL character - paste output into single string with
#'        collapse
#'
#' @return A vector of most common element(s). Will be character unless x is
#'         numeric and you don't tell it to collapse into a single string!
#'
#' @examples
#' my_stuff <- c(1:10, 10, 5)
#'
#' # These are straight forward
#' get_1st(my_stuff)
#' get_nth(my_stuff, 3)
#' get_last(my_stuff)
#'
#' # Returns numeric vector of mode(s) if x is numeric
#' get_most_frequent(my_stuff)
#'
#' # Else it returns a character vector
#' my_chars <- c("a", "b", "b", "a", "g", "o", "l", "d")
#' get_most_frequent(my_chars)
#'
#' # Can collapse into a single string (for convienience)
#' get_most_frequent(my_chars, collapse = " & ")
#' @export
#' @rdname time_savers
get_most_frequent <- function(x, collapse = NULL){
  x_class   <- class(x)
  x_summary <- sort(summary(as.factor(as.character(x))), decreasing = TRUE)
  out       <- names(x_summary[x_summary == max(x_summary, na.rm = TRUE)])

  # if collapsing
  if(!is.null(collapse)){
    out <- paste(unique(out), collapse = collapse)
  } else if(is.numeric(x)){
    out <- as.numeric(out)
  }

  return(out)
}

#' @param  x a string
#' @param  ignore.punct logical - ignore punctuation marks
#' @param  ignore.case logical - ignore case (if true, will return in lower)
#' @param  split character that separated words. Default = ' '
#' @param  collapse OPTIONAL character - paste output into single string with
#'         collapse
#' @param  punct.regex character - regex used to remove punctuation
#'         (by default `[[:punct:]]`)
#' @param  punct.replace character - what to replace punctuation with (default is "")
#'
#' @examples
#' generic_string <- "Who's A good boy? Winston's a good boy!"
#'
#' get_1st_word(generic_string)
#' get_nth_word(generic_string, 3)
#' get_last_word(generic_string)
#'
#' # default ignores case and punctuation
#' get_most_frequent_word(generic_string)
#'
#' # can change like so:
#' get_most_frequent_word(generic_string, ignore.case = FALSE, ignore.punct = FALSE)
#' @export
#' @rdname time_savers
get_most_frequent_word <- function(x,
                                   ignore.punct  = TRUE,
                                   ignore.case   = TRUE,
                                   split         = " ",
                                   collapse      = NULL,
                                   punct.regex   = "[[:punct:]]",
                                   punct.replace = ""
                                   ){
  x_class   <- class(x)

  if(x_class != "character"){
    x <- as.character(x)
  }

  if(ignore.case){
    x <- tolower(x)
  }

  if(ignore.punct){
    x <- gsub(punct.regex, punct.replace, x)
  }

  x_words   <- unlist(strsplit(x, split))
  out       <- get_most_frequent(x_words, collapse)

  return(out)
}

#' Sort a factor with optional reference level
#' @param x A vector to be turned into a sorted factor
#' @param ref Character - optional level to be set first
#' @param decreasing Logical - sort resulting factor in decreasing order
#' @param force_ref Logical - force ref level even if `ref` is not in the data
#' @rdname sort_factor
#' @export
sort_factor <- function(x, ref = NULL, decreasing = FALSE, force_ref = FALSE){
  # check if ref exists in x.
  # skip if force_ref is TRUE -> allow user to make factor level with 0 observations
  if(!force_ref){
    if(length(ref) && !ref %in% x) ref <- NULL # make NULL if it doesn't exist
  }
  # Will make this a pipe when more people have R >= 4.1
  lbl <- factor(x)
  lbl <- summary(lbl)
  lbl <- sort(lbl, decreasing = decreasing)
  lbl <- names(lbl)
  lbl <- c(ref, lbl)
  out <- factor(x, levels = lbl, labels=lbl)
  return(out)
}


#' Return Number of Unique Things
#'
#' @param x vector
#' @param na.rm whether to ignore NAs when determining uniqueness
#'
#' @rdname time_savers
#' @export
n_unique <- function(x, na.rm = FALSE){
  if(isTRUE(na.rm)){
    calc_len <- length_cc
  } else{
    calc_len <- length
  }
  calc_len(unique(x))
}

#' Read TSV Files
#'
#' @description
#' Like read.csv but for tsv and default header = TRUE.
#'
#' @param file path of file you want to load
#' @param ...  other args used by read.table
#'
#' @name read.tsv
NULL

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
