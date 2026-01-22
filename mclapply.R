#' Enhanced Multicore Lapply
#'
#' @name mclapply
#'
#' Wrapper for parallel::mclapply with error handling.
#'
#' @param X List or vector.
#' @param FUN Function to apply.
#' @param ... Arguments to FUN.
#' @param mc.cores Number of cores.
#' @param stop.on.error If TRUE, stop all workers on first error. Default TRUE.
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

iaw$mclapply <- function(X, FUN, ..., mc.cores = parallel::detectCores(),
                         stop.on.error = TRUE, verbose = TRUE) {
  stopifnot(is.list(X) || is.vector(X))
  stopifnot(is.function(FUN))
  stopifnot(is.numeric(mc.cores), length(mc.cores) == 1L)
  stopifnot(.Platform$OS.type != "windows")

  if (!stop.on.error)
    return(parallel::mclapply(X, FUN, ..., mc.cores = mc.cores))

  flag <- tempfile(pattern = "mclapply_flag_")
  errfile <- tempfile(pattern = "mclapply_err_")
  on.exit(unlink(c(flag, errfile)), add = TRUE)

  n <- length(X)
  wrapper <- function(i) {
    if (file.exists(flag)) return(NULL)
    tryCatch(
      FUN(X[[i]], ...),
      error = function(e) {
        # Write diagnostics before signaling others to stop
        err_info <- paste0(
          "Index: ", i, " / ", n, "\n",
          "Message: ", conditionMessage(e), "\n",
          "Call: ", deparse(conditionCall(e)), "\n"
        )
        try(writeLines(err_info, errfile), silent = TRUE)
        file.create(flag)
        stop(e)
      }
    )
  }

  results <- parallel::mclapply(seq_along(X), wrapper, mc.cores = mc.cores)

  # Check for failures

  failed <- which(sapply(results, inherits, "try-error"))

  if (length(failed) > 0) {
    # Read captured error info if available
    if (file.exists(errfile)) {
      if (verbose) {
        message("=== Parallel error details ===")
        message(paste(readLines(errfile), collapse = "\n"))
      }
      err_info <- readLines(errfile)
      idx_line <- grep("^Index:", err_info, value = TRUE)
      if (length(idx_line) > 0) {
        first_fail <- as.integer(sub("Index: (\\d+).*", "\\1", idx_line[1]))
      } else {
        first_fail <- failed[1]
      }
    } else {
      first_fail <- failed[1]
    }

    if (verbose) {
      message("=== Re-running index ", first_fail, " with lapply for full traceback ===")
    }
    # This will produce a proper error with full traceback
    FUN(X[[first_fail]], ...)
  }

  results
}
