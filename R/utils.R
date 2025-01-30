# Specific Version of Stop for roperators
rop_stop <- function(...){
  stop("roperators: \n ", ...,
       call. = FALSE)
}