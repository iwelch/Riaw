#' File Locking
#'
#' @name lock
#'
#' Creates a lock file.
#'
#' @param lockfile Lock file path.
#' @param timeout Timeout in seconds.
#'
#' @return TRUE if lock acquired.
#'
#' @examples
#' \dontrun{
#' # Acquire a lock, do work, then release it
#' lockfile <- tempfile(fileext = ".lock")
#' if (iaw$lock(lockfile)) {
#'   message("Lock acquired, doing work...")
#'   # ... critical section ...
#'   iaw$unlock(lockfile)
#' } else {
#'   message("Could not acquire lock within timeout")
#' }
#'
#' # Short timeout for non-blocking attempt
#' acquired <- iaw$lock(lockfile, timeout = 1)
#' if (acquired) iaw$unlock(lockfile)
#' }
#'
#' @family io
#' @export

iaw$lock <- function(lockfile, timeout = 60) {
    stopifnot(is.character(lockfile), length(lockfile) == 1L)
    stopifnot(is.numeric(timeout), length(timeout) == 1L)
    
    start <- Sys.time()
    while (file.exists(lockfile)) {
        if (difftime(Sys.time(), start, units = "secs") > timeout) {
            return(FALSE)
        }
        Sys.sleep(0.5)
    }
    writeLines(as.character(Sys.getpid()), lockfile)
    TRUE
}

#' @rdname lock
#'
#' @return \code{unlock}: Invisible \code{NULL}.
#'
#' @export
iaw$unlock <- function(lockfile) {
    if (file.exists(lockfile)) file.remove(lockfile)
    invisible(NULL)
}
