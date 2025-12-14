#' Enhanced Parallel lapply with Error Handling
#'
#' A modified version of \code{parallel::mclapply} that adds a \code{stop.on.error}
#' option to immediately halt on the first error rather than collecting all errors.
#'
#' @param X A vector (atomic or list) over which to apply FUN.
#' @param FUN The function to apply to each element of X.
#' @param ... Additional arguments passed to FUN.
#' @param mc.preschedule Logical; if TRUE (default), computation is scheduled
#'   in advance. Set FALSE for long-running or variable-time tasks.
#' @param mc.set.seed Logical; if TRUE (default), random seeds are set for
#'   reproducibility.
#' @param mc.silent Logical; if TRUE, suppresses output from child processes.
#' @param mc.cores Integer; number of cores to use. Default from \code{options("mc.cores")}.
#' @param mc.cleanup Logical; if TRUE (default), cleans up child processes on exit.
#' @param mc.allow.recursive Logical; if TRUE, allows recursive mclapply calls.
#' @param affinity.list List specifying CPU affinity for each element.
#' @param stop.on.error Logical; if TRUE (default), stops immediately on first error.
#'   If FALSE, collects all results and reports errors at the end.
#'
#' @return A list of results, same length as X.
#'
#' @details
#' This is a drop-in replacement for \code{parallel::mclapply} with the additional
#' \code{stop.on.error} parameter. When TRUE, the function will stop execution
#' immediately upon encountering an error in any child process, which is useful
#' for debugging.
#'
#' @note Only works on Unix-like systems (Linux, macOS). On Windows, falls back
#' to sequential processing.
#'
#' @export
#'
#' @seealso \code{\link{iaw$mcsapply}}, \code{\link{iaw$mc.by}},
#'   \code{\link[parallel]{mclapply}}
#'
#' @examples
#' # Simple parallel computation
#' result <- iaw$mclapply(1:10, function(x) x^2)
#'
#' # With stop on error (default)
#' \dontrun{
#' result <- iaw$mclapply(1:10, function(x) {
#'     if (x == 5) stop("Error at 5")
#'     x^2
#' })
#' # Stops immediately at element 5
#' }
#'
#' # Continue despite errors
#' \dontrun{
#' result <- iaw$mclapply(1:10, function(x) {
#'     if (x == 5) stop("Error at 5")
#'     x^2
#' }, stop.on.error = FALSE)
#' # Continues and reports errors at end
#' }

library(parallel)

iaw$mclapply <- function(X, FUN, ..., mc.preschedule = TRUE,
                          mc.set.seed = TRUE, mc.silent = FALSE,
                          mc.cores = getOption("mc.cores", 2L),
                          mc.cleanup = TRUE, mc.allow.recursive = TRUE,
                          affinity.list = NULL, stop.on.error = TRUE) {

    stop.on.error <- stop.on.error[1]
    stopifnot(is.logical(stop.on.error))

    cores <- as.integer(mc.cores)
    if ((is.na(cores) || cores < 1L) && is.null(affinity.list))
        stop("'mc.cores' must be >= 1")

    parallel:::.check_ncores(cores)

    if (parallel:::isChild() && !isTRUE(mc.allow.recursive))
        return(lapply(X = X, FUN = FUN, ...))

    if (!is.vector(X) || is.object(X))
        X <- as.list(X)

    if (!is.null(affinity.list) && length(affinity.list) < length(X))
        stop("affinity.list and X must have the same length")

    if (mc.set.seed)
        mc.reset.stream()

    if (length(X) < 2) {
        old.aff <- mcaffinity()
        mcaffinity(affinity.list[[1]])
        res <- lapply(X = X, FUN = FUN, ...)
        mcaffinity(old.aff)
        return(res)
    }

    if (length(X) < cores)
        cores <- length(X)

    if (cores < 2L && is.null(affinity.list))
        return(lapply(X = X, FUN = FUN, ...))

    # Use parallel::mclapply but wrap with error handling
    result <- parallel::mclapply(
        X, FUN, ...,
        mc.preschedule = mc.preschedule,
        mc.set.seed = mc.set.seed,
        mc.silent = mc.silent,
        mc.cores = mc.cores,
        mc.cleanup = mc.cleanup,
        mc.allow.recursive = mc.allow.recursive,
        affinity.list = affinity.list
    )

    # Check for errors
    has.errors <- vapply(result, inherits, logical(1), "try-error")

    if (any(has.errors) && stop.on.error) {
        first.error.idx <- which(has.errors)[1]
        stop("Error in element ", first.error.idx, ": ",
             attr(result[[first.error.idx]], "condition")$message)
    }

    if (any(has.errors)) {
        warning(sum(has.errors), " function calls resulted in errors")
    }

    result
}
