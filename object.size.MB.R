#' Object Size in MB
#'
#' @name object.size.MB
#'
#' Returns object size in megabytes.
#'
#' @param x Object to measure.
#'
#' @return Size in MB.
#'
#' @family utilities
#' @export
#'
#' @examples
#' iaw$object.size.MB(1:1000000)          # ~3.81 MB
#'
#' # Small object
#' iaw$object.size.MB(1:10)               # ~0 MB
#'
#' # Compare storage of different types
#' iaw$object.size.MB(double(1e6))        # 8 bytes each -> ~7.63 MB
#' iaw$object.size.MB(integer(1e6))       # 4 bytes each -> ~3.81 MB
#'
#' # Check size of a data frame
#' d <- data.frame(x = rnorm(1e5), y = rnorm(1e5), z = sample(letters, 1e5, replace = TRUE))
#' iaw$object.size.MB(d)

iaw$object.size.MB <- function(x) {
    round(object.size(x) / 1024^2, 2)
}
