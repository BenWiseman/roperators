# IP checks
# Quick ways to get your IP address from within R
# EXPERIMENTAL for windows and linux only

# get_ip <- function(){
#
#   ip <- NA
#
#   if(is.os_win()){
#     systext <- system("ipconfig", intern=TRUE)
#     ip      <- gsub(".*? ([[:digit:]])", "\\1", systext[grep("IPv4", systext)])
#   } else if(is.os_lnx() || is.os_unx()) {
#     systext <- system("ifconfig", intern=TRUE)
#     ip      <- gsub(".*? ([[:digit:]])", "\\1", systext[grep("inet", systext)])
#   } else if(is.os_mac()){
#     rop_stop("not supported on OSX")
#     #systext <- system("ifconfig", intern=TRUE)
#     #ip      <- gsub(".*? ([[:digit:]])", "\\1", systext[grep("inet ", systext)])
#   } else{
#     rop_stop("couldn't detect your operating system or IP address from system calls")
#   }
#
#   return(ip)
# }
