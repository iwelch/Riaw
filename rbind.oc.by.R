#' Rbind Results from oc.by
#'
#' @name rbind.oc.by
#'
#' Combines oc.by results into data frame.
#'
#' @param indata Data frame.
#' @param INDICES Grouping variable.
#' @param FUNIN Function returning data frames.
#' @param ... Additional arguments.
#'
#' @return Combined data frame.
#'
#' @examples
#' # Demean x within each group and rbind all groups back together
#' df <- data.frame(
#'   group = c("A", "A", "B", "B", "B"),
#'   x     = c(1, 3, 2, 4, 6)
#' )
#' result <- iaw$rbind.oc.by(df, df$group, function(sub) {
#'   sub$x_dm <- sub$x - mean(sub$x)
#'   sub
#' })
#' result  # 5-row data frame with x_dm column added
#'
#' # Filter rows within groups and recombine
#' iaw$rbind.oc.by(df, df$group, function(sub) sub[sub$x > mean(sub$x), ])
#'
#' @family parallel
#' @export

iaw$rbind.oc.by <- function(indata, INDICES, FUNIN, ...) {
    stopifnot(is.data.frame(indata))
    result <- iaw$oc.by(indata, INDICES, FUNIN, ...)
    result <- Filter(Negate(is.null), result)
    if (length(result) == 0L) return(indata[0L, , drop = FALSE])
    out <- data.table::rbindlist(result)
    if (!inherits(indata, "data.table")) out <- as.data.frame(out)
    out
}
