
#' Memory Information
#'
#' @name meminfo
#'
#' meminfo gives system information, trying to figure out the OS.
#'
#' @return the amount of system memory, in KB
#'

iaw$meminfo <- function(){
    os <- iaw$osinfo()
    if (os == "macos") {
        c( mem = as.numeric(system("sysctl hw.memsize | cut -c13-99", intern=TRUE))/1024 )
    } else if (os == "linux") {
        c( mem = as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) )
    } else {
        c( mem = memory.limit() )
    }
}

iaw$mem.setmc <- function() {
    used.mb <- sum(gc()[,2])
    avail.mb <- iaw$meminfo()/1024
    tolerable <- as.integer( 1.2*avail.mb/used.mb )
    nc <- min( tolerable , detectCores() ) - 1  ## one core remains unused
    options(mc.cores=nc)
    nc
}


