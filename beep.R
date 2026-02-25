#' Audio Beep
#'
#' @name beep
#'
#' Plays audio beep notification.
#'
#' @param n Number of beeps.
#'
#' @return Invisible NULL.
#'
#' @examples
#' \dontrun{
#' # Single beep when a long job finishes
#' iaw$beep()
#'
#' # Three beeps to signal an error condition
#' iaw$beep(3)
#' }
#'
#' @family utilities
#' @export

iaw$beep <- function(n = 1) {
    stopifnot(is.numeric(n), n >= 1)
    for (i in seq_len(n)) {
        cat("\a")
        Sys.sleep(0.2)
    }
    invisible(NULL)
}
