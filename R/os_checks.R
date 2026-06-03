#' Operating system and environment checks
#'
#' @description
#' Determine the current operating system and R environment, and provide simple
#' flags for common questions such as "are we on a Mac?", "is this 64-bit R?",
#' or "are we running inside RStudio?". These are useful when writing code that
#' must behave differently across platforms (for example, choosing a parallel
#' back-end on Unix versus Windows).
#'
#' @param units character; the unit to report the R version age in, one of
#'   \code{"years"}, \code{"months"}, \code{"weeks"}, or \code{"days"}.
#' @param rounding integer; the number of decimal places to round the age to.
#'
#' @return
#' For the \code{is.*} checks, a single logical value. \code{get_os()} returns a
#' character string (\code{"win"}, \code{"mac"}, \code{"linux"}, or
#' \code{"unix"}); \code{get_R_version()} and \code{get_latest_CRAN_version()}
#' return version strings; and \code{get_R_version_age()} returns a numeric age.
#'
#' @examples
#' # determine the operating system
#' get_os()
#'
#' # test for a particular operating system
#' is.os_mac()
#' is.os_win()
#' is.os_lnx()
#' is.os_unx()
#'
#' # environment checks
#' is.os_x64()
#' is.RStudio()
#' get_R_version()
#'
#' @author Ben Wiseman, \email{benjamin.h.wiseman@@gmail.com}
#' @author Steven Nydick, \email{steven.nydick@@kornferry.com}
#' @name os
NULL

#' @rdname os
#' @export
get_os <- function(){

  # pull out the original OS
  os <- tolower(Sys.info()["sysname"])

  # if we do not have an os to pull, pull from .Platform/R.version
  if(is.null(os)){
    os     <- tolower(.Platform$OS.type)
    os_ver <- tolower(R.version$os)

    # be more specific if OS is unix and we have additional info
    if(grepl("^darwin", os_ver)){
      os   <- "darwin"
    } else if(grepl("^linux", os_ver)){
      os   <- "linux"
    }
  }

  # flag os and return
  if(os %in% "windows"){
    return("win")
  } else if(os %in% "darwin"){
    return("mac")
  } else if(os %in% "linux"){
    return("linux")
  } else if(os %in% "unix"){
    return("unix")
  } else{
    return(os)
  }
}


#' get R version number
#' @rdname os
#' @export
get_R_version <- function() {
  paste0(R.version$major, ".", R.version$minor)
}


#' get R version age
#' @rdname os
#' @export
get_R_version_age <- function(units = c("years", "months", "weeks", "days"), rounding = 2) {
  # Check if units is missing
  units_missing <- missing(units)

  # if units not supplied, pull the first default option
  # vector was added to default to make intellisense nicer
  units <- units[1]

  # Extract the release date from the version string
  release_date_string <- gsub(".*\\((.*)\\)$", "\\1", R.version$version.string)

  # Convert the release date string to a Date object
  release_date <- as.Date(release_date_string, format = "%Y-%m-%d")

  # Calculate the age of the R installation
  age <- as.numeric(difftime(Sys.Date(), release_date, units = "days"))

  # Convert the age to the specified units
  if (units %in% c("years", "y")) {
    age <- age / 365.25
    units <- "years"
  } else if (units %in% c("months", "m")) {
    age <- age / 30.44
    units <- "months"
  } else if (units %in% c("weeks", "w")) {
    age <- age / 7
    units <- "weeks"
  } else if (units %in% c("days", "d")) {
    units <- "days"
  } else {
    stop("Invalid units. Valid units are 'years', 'months', 'weeks', or 'days'.")
  }

  # Round the age to the specified number of decimal places
  age <- round(age, rounding)

  # Print a message stating the units if units was missing
  if (units_missing) {
    message("The age of the R installation is given in ", units, ".")
  }

  return(age)
}

#' look on R's release notes to figure out if there's a new version
#' This is reliant on rvest
#' @rdname os
#' @export
get_latest_CRAN_version <- function() {
  # Check if the rvest package is installed
  if ("rvest" %ni% rownames(utils::installed.packages())) {
    # Ask the user if they want to install rvest
    install_rvest <- readline(prompt = "rvest will be required to pull the latest build from CRAN. Install it now? (yes/no): ")

    # If the user answered yes, install rvest
    if (tolower(install_rvest) == "yes") {
      utils::install.packages("rvest")
      #library(rvest)
    } else {
      stop("rvest is required to pull the latest build from CRAN.")
    }
  }

  # Read the R release notes page
  release_notes_page <- rvest::read_html("https://cran.r-project.org/doc/manuals/r-release/NEWS.html")

  # Extract the first line of the body text which contains the latest R version
  latest_R_version <- release_notes_page
  latest_R_version <- rvest::html_nodes(latest_R_version, xpath = '//h3[1]')
  latest_R_version <- rvest::html_text(latest_R_version)
  latest_R_version <- gsub(" CHANGES IN R ", "", latest_R_version)


  return(latest_R_version)
}


#' get which python version is found via system calls
#' @rdname os
#' @export
get_system_python <- function() {
  # Call the 'python --version' command and capture the output
  python_version <- system("python --version", intern = TRUE)

  return(gsub("^Python ","",python_version))
}


#' @rdname os
#' @export
is.os_mac <- function(){
  get_os() == "mac"
}

#' @rdname os
#' @export
is.os_win <- function(){
  get_os() == "win"
}

#' @rdname os
#' @export
is.os_lnx <- function(){
  get_os() == "linux"
}

#' @rdname os
#' @export
is.os_unx <- function(){
  (get_os() == "unix") || is.os_lnx() || is.os_mac()
}

#' work out if running 64 bit machine
#' @rdname os
#' @export
is.os_x64 <- function(){
  #grepl("(64-bit)", sessionInfo()[["platform"]]) |
  grepl("x86_64", Sys.info()[["machine"]])
}

#' Check if running on Arm CPU
#' @rdname  os
#' @export
is.os_arm <- function(){
   grepl("arm64", Sys.info()["machine"])
}

#' work out if running 64 bit machine
#' @rdname os
#' @export
is.R_x64 <- function(){
  if(is.os_unx()) grepl("x86_64", Sys.getenv("R_PLATFORM")) else Sys.getenv("R_ARCH") == "/x64"
}

#' TRUE if running revolution R/Microsoft R Open
#' @rdname os
#' @export
is.R_revo <- function(){
  "RevoUtils" %in% names(utils::sessionInfo()[["otherPkgs"]])
}

#' TRUE if running RStudio interactively
#' @rdname os
#' @export
is.RStudio <- function(){
  identical(.Platform$GUI, "RStudio")
}

#' TRUE if system can either call or use HTTR to post to http
#' @rdname os
#' @export
is.http_available <- function() {
  # httr or RCurl give us HTTP from within R (checked without loading them, and
  # without forcing them into Suggests)
  if (nzchar(system.file(package = "httr")))  return(TRUE)
  if (nzchar(system.file(package = "RCurl"))) return(TRUE)

  # otherwise fall back to a system 'curl' command if one is on the PATH
  curl_version <- tryCatch(
    system2("curl", "--version", stdout = TRUE, stderr = NULL),
    error   = function(e) character(0),
    warning = function(w) character(0)
  )

  length(curl_version) > 0 && nzchar(curl_version[1])
}

