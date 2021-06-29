# These are just space savers

#' Cleaner conversion functions
#'
#' @param x Value to be converted
#' @param ... other args for as. conversion
#'
#' # Shorthand aliases for common conversions
#' # Nothing magical here, but it can make your code more readable
#'
#' @examples
#' chr(42)   # "42" = as.character
#' int(42.1) # 42L  = as.integer
#' dbl("42L") # 42.0 = as.double
#' num("42") # 42   = as.numeric
#' bool(42)  # TRUE = as.logical
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



#### Conversion shorthands
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
#'
#' @export
#' @rdname factor_conversion
f.as.numeric <- function(x){
  as.numeric(levels(x))[as.numeric(x)]
  #as.numeric(as.character(x)) # for factors
}


#### Time Savers
#' Little functions to replace common minor functions. useful in apply sttements
#' @param x an R object, usually a character vector
#' @param type 'v' (default) for vector x[1]; 'l' for list x[[1]]
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
#' @export
#' @rdname time_savers
get_1st <- function(x, type = 'v') {
  if(type == 'l') x[[1]]
  else x[1]
}

get_last <- function(x, type = 'v') {
  if(type == 'l') x[[length(x)]]
  else x[length(x)]
}

#' @export
#' @param n integer, the nth element to select
#' @rdname time_savers
get_nth <- function(x, n, type = 'v') {
  if(type == 'l') x[[n]]
  else x[n]
}

#' @export
#' @param split character that seperated words. Default = ' '
#' @rdname time_savers
get_1st_word <- function(x, type = 'v', split = ' ') {
  unlist(lapply(strsplit(x, split), get_1st, type))
}

#' @export
#' @param split character that seperated words. Default = ' '
#' @rdname time_savers
get_last_word <- function(x, type = 'v', split = ' ') {
  unlist(lapply(strsplit(x, split), get_last, type))
}


#' like read.csv, but for tsv and default header = TRUE
#' @param file path of file you want to load
#' @param ...  other args used by read.table
#' @rdname read.tsv
#' @export
read.tsv <- function(file, ...){
  read.table(x, header = TRUE, sep = '\t', ...)
}

#' like read.csv, but for pipe-delineated and defaults to header = TRUE
#' @param file path of file you want to load
#' @param ...  other args used by read.table
#' @rdname read.tsv
#' @export
read.psv <- function(file, ...){
  read.table(x, header = TRUE, sep = '|', ...)
}