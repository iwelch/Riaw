
#' NOW
#'
#' @name now() )
#'       2019-02-16 17:37:40
#' the current day and time
#'
#' @usage now ()
#'
#' @return the current data in readable format, such as "2019-02-16 17:37:02"
#'
#' @examples
#'    message( iaw$now() )
#'       2019-02-16 17:37:40

iaw$now <- function () as.character(Sys.time())
