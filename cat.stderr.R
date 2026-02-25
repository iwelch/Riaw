#' Cat to stderr
#'
#' @name cat.stderr
#'
#' Prints to standard error.
#'
#' @param ... Items to print.
#'
#' @return Invisible NULL.
#'
#' @examples
#' \dontrun{
#' # Send a diagnostic message to stderr without polluting stdout
#' iaw$cat.stderr("Loading data...\n")
#'
#' # Report progress in a script whose stdout is redirected to a file
#' iaw$cat.stderr(sprintf("Processed %d rows\n", nrow(mydf)))
#'
#' # Emit timing information during a multi-step pipeline
#' t0 <- Sys.time()
#' Sys.sleep(0.01)
#' iaw$cat.stderr(sprintf("Step 1 done in %.1fs\n", as.numeric(Sys.time() - t0)))
#'
#' # Log warnings to stderr while writing clean CSV output to stdout
#' iaw$cat.stderr("Warning: 3 firms have missing GICS codes\n")
#' }
#'
#' @family utilities
#' @export

iaw$cat.stderr <- function(...) {
    cat(..., file = stderr())
    invisible(NULL)
}
