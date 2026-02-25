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
#'
#' # Alert after a long-running backtest completes
#' results <- Sys.sleep(0.1)  # placeholder for long computation
#' iaw$beep(2)
#'
#' # Beep once after overnight data download finishes
#' download_data <- function() Sys.sleep(0.1)
#' download_data()
#' iaw$beep()
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
