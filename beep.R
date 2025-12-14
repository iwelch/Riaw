#' Make a System Beep Sound
#'
#' Plays a system beep sound if available. Useful for signaling completion
#' of long-running computations.
#'
#' @return Invisibly returns the exit status of the beep command.
#'
#' @note Requires the \code{beep} command to be available on the system.
#'   On macOS, you may need to install it via Homebrew. On Linux, it's
#'   often available in the \code{beep} package.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Signal completion
#' result <- long_computation()
#' iaw$beep()
#' }

iaw$beep <- function() {
    invisible(system("beep", ignore.stdout = TRUE, ignore.stderr = TRUE))
}
