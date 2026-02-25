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

iaw$meminfo <- function() {
    gc_result <- gc()
    list(
        used_MB = sum(gc_result[, 2]),
        max_MB = sum(gc_result[, 6])
    )
}
