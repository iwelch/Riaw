#' Rbind Results from mc.by
#'
#' @name rbind.mc.by
#'
#' Combines mc.by results into data frame.
#'
#' @param indata Data frame.
#' @param INDICES Grouping variable.
#' @param FUNIN Function returning data frames.
#' @param ... Additional arguments.
#'
#' @return Combined data frame.
#'
#' @examples
#' \dontrun{
#' # Demean x within each group and return all rows combined
#' df <- data.frame(
#'   group = c("A", "A", "B", "B", "B"),
#'   x     = c(1, 3, 2, 4, 6)
#' )
#' result <- iaw$rbind.mc.by(df, df$group, function(sub) {
#'   sub$x_dm <- sub$x - mean(sub$x)
#'   sub
#' })
#' result  # all rows combined into one data frame, preserving columns
#' }
#'
#' @family parallel
#' @export

iaw$rbind.mc.by <- function(indata, INDICES, FUNIN, ...) {
    stopifnot(is.data.frame(indata))
    result <- iaw$mc.by(indata, INDICES, FUNIN, ...)
    result <- Filter(Negate(is.null), result)
    if (length(result) == 0L) return(indata[0L, , drop = FALSE])
    out <- data.table::rbindlist(result)
    if (!inherits(indata, "data.table")) out <- as.data.frame(out)
    out
}
