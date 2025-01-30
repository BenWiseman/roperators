#' Operating system checks
#'
#' @description
#' Determine the current operating system as well as provide flags to indicate
#' whether the operating system is a Mac/Windows/Linux.
#'
#' @examples
#' # determine operating system
#' get_os()
#'
#' # do we have a particular operating system
#' is.os_mac()
#' is.os_win()
#' is.os_lnx()
#' is.os_unx()
#'
#' @author Ben Wiseman, \email{benjamin.wiseman@@kornferry.com}
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
#' @param units character - how do you want to display the age? e.g. years or months?
#' @param rounding integer - how many decimal points do you want to see. e.g. 0.25 years
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
  if(is.os_unx) grepl("x86_64", Sys.getenv("R_PLATFORM")) else  Sys.getenv("R_ARCH") == "/x64"
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
  # Check if the httr package is installed
  if ("httr" %in% utils::installed.packages()) {
    return(TRUE)
  }

  # Check if the RCurl package is installed
  if ("RCurl" %in% utils::installed.packages()) {
    return(TRUE)
  }

  # If neither httr nor RCurl are installed, check if the curl command-line tool is available
  # The system2 function is used to call the 'curl --version' command and suppress any error messages
  curl_version <- system2("curl", "--version", stdout = TRUE, stderr = NULL)

  # If the 'curl --version' command returned a non-empty string, curl is available
  if (length(curl_version) > 0 && curl_version[1] != "") {
    return(TRUE)
  }

  # If neither httr, RCurl, nor curl are available, return FALSE
  return(FALSE)
}

