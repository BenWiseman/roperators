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
