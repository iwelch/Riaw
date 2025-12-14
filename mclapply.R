#' Enhanced Multicore Lapply
#'
#' @name mclapply
#'
#' Wrapper for parallel::mclapply with fallback.
#'
#' @param X List or vector.
#' @param FUN Function to apply.
#' @param ... Arguments to FUN.
#' @param mc.cores Number of cores.
#'
#' @return List of results.
#'
#' @family parallel
#' @export
#'
#' @importFrom parallel mclapply detectCores
#'
#' @examples
#' \dontrun{
#' iaw$mclapply(1:10, function(x) x^2)
#' }

iaw$mclapply <- function(X, FUN, ..., mc.cores = parallel::detectCores()) {
    stopifnot(is.list(X) || is.vector(X))
    stopifnot(is.function(FUN))
    stopifnot(is.numeric(mc.cores), length(mc.cores) == 1L)
    
    if (.Platform$OS.type == "windows") {
        lapply(X, FUN, ...)
    } else {
        parallel::mclapply(X, FUN, ..., mc.cores = mc.cores)
    }
}
