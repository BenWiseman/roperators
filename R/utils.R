# Specific Version of Stop for roperators
rop_stop <- function(...){
  stop("roperators: \n ", ...,
       call. = FALSE)
}

rop_warn <- function(...){
  warning("roperators: \n ", ...,
          call. = FALSE)
}

# function to do reassignment automatically for assignment operators!
reassign <- function(name, value, n = 2){
  eval(call("<-", name, value), envir = parent.frame(n = n))
}
