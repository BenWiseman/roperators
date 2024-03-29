% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/os_checks.R
\name{os}
\alias{os}
\alias{get_os}
\alias{is.os_mac}
\alias{is.os_win}
\alias{is.os_lnx}
\alias{is.os_unx}
\alias{is.os_x64}
\alias{is.R_x64}
\alias{is.R_revo}
\alias{is.RStudio}
\title{Operating System Checks}
\usage{
get_os()

is.os_mac()

is.os_win()

is.os_lnx()

is.os_unx()

is.os_x64()

is.R_x64()

is.R_revo()

is.RStudio()
}
\description{
Determine the current operating system as well as provide flags to indicate
whether the operating system is a Mac/Windows/Linux.
}
\examples{
# determine operating system
get_os()

# do we have a particular operating system
is.os_mac()
is.os_win()
is.os_lnx()
is.os_unx()

}
\author{
Ben Wiseman, \email{benjamin.wiseman@kornferry.com}

Steven Nydick, \email{steven.nydick@kornferry.com}
}
