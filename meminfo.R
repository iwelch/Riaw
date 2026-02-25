#' Memory Information
#'
#' @name meminfo
#'
#' Returns current memory usage.
#'
#' @return List with memory info.
#'
#' @family utilities
#' @export
#'
#' @examples
#' # Check current memory usage (triggers a GC cycle)
#' iaw$meminfo()
#'
#' # Allocate a large object and see memory rise
#' \dontrun{
#' m <- matrix(rnorm(1e6), ncol = 100)
#' iaw$meminfo()$used_MB   # higher after allocation
#' rm(m); gc()
#' iaw$meminfo()$used_MB   # lower after removal
#' }
#'
#' # Access individual components of the memory report
#' info <- iaw$meminfo()
#' info$used_MB   # current heap usage in MB
#' info$max_MB    # peak heap usage since session start
#'
#' # Log memory at checkpoints during a long pipeline
#' \dontrun{
#' for (step in c("load", "transform", "model")) {
#'   # ... processing ...
#'   message(step, ": ", iaw$meminfo()$used_MB, " MB")
#' }
#' }

iaw$meminfo <- function() {
    gc_result <- gc()
    list(
        used_MB = sum(gc_result[, 2]),
        max_MB = sum(gc_result[, 6])
    )
}
