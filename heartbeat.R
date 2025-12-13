
#' HEARTBEAT
#'
#' @name heartbeat(i, N)
#'
#' @return
#'
#' If the 'now' value is exactly equal to a value that crosses a percentage figure (for 'total'),
#' then print it.  otherwise, just be quiet.
#'
#' @usage heartbeat (now, total, length.out = 100)
#'
#' @param now something that counts up
#' @param total the final value of now
#'
#' @examples
#'     N= 1312315; for (i in 1:N) iaw$heartbeat(i, N)
#'
#' @return
#'


iaw$heartbeat <- function (now, total, length.out = 100) {
  if (now %in% as.integer(seq(0, total, length.out = length.out)))
      message("[", now, "=", as.integer(now * 100/total), "%]")
  if (now==total) cat("\n", file=stderr())
}
