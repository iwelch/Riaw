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
#' iaw$meminfo()

iaw$meminfo <- function() {
    gc_result <- gc()
    list(
        used_MB = sum(gc_result[, 2]),
        max_MB = sum(gc_result[, 6])
    )
}
