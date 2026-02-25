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
#' @param verbose Logical. Print error details on failure (default \code{TRUE}).
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
#' # Square numbers in parallel
#' iaw$mclapply(1:10, function(x) x^2)
#'
#' # Process a list of data frames in parallel, one per ticker
#' tickers <- list(AAPL = df_aapl, MSFT = df_msft, GOOG = df_goog)
#' results <- iaw$mclapply(tickers, function(d) lm(ret ~ mkt, data = d))
#'
#' # Use stop.on.error = FALSE to collect partial results despite failures
#' out <- iaw$mclapply(1:5, function(x) {
#'   if (x == 3) stop("bad element")
#'   x^2
#' }, stop.on.error = FALSE)
#'
#' # Limit to 2 cores explicitly
#' iaw$mclapply(1:8, function(x) x^2, mc.cores = 2)
#'
#' # Read and summarize multiple CSV files in parallel
#' files <- list.files("/data/daily", pattern = "\\.csv$", full.names = TRUE)
#' summaries <- iaw$mclapply(files, function(f) {
#'   d <- read.csv(f)
#'   data.frame(file = basename(f), nrow = nrow(d), mean_ret = mean(d$ret))
#' })
#' do.call(rbind, summaries)
#' }

iaw$mclapply <- function(X, FUN, ..., mc.cores = getOption("mc.cores", parallel::detectCores()),
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
  if (n == 0L) return(list())

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
