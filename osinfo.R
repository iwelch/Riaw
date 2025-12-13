
#' Operating System Information
#'
#' @name osinfo
#'
#' osinfo gives system information, trying to figure out the OS.
#'
#' @return one of macos, linux, or windows.
#'


iaw$osinfo <- function(){
    sysname <- tolower(Sys.info()["sysname"])
    if (grepl("darwin", sysname)) return(c(os="macos"))
    if (grepl("linux", sysname)) return(c(os="linux"))
    c(os="windows")
}
