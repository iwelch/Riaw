
#' Time Difference in Seconds
#'
#' @name tmsec( a, Sys.time() ), " seconds" )
#'
#'   return number of seconds between two times
#'
#' @usage tmdiffsec( Sys.time(), Sys.time() )
#'
#' @param t1 the first time, often set at the start of the program
#' @param t1 the first time, a second time
#'
#' @return an integer: the number of seconds
#'
#' @examples a <- Sys.time(); sleep(10); cat("Run: ", iaw$tmsec( a, Sys.time() ), " seconds" )
#'
#' @seealso system.time()

iaw$tmdiffsec <- function(s1,s2) abs(as.integer(difftime(s1 , s2, units="secs")))
