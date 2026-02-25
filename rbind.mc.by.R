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
#'
#' # Compute rolling z-scores within each sector (parallel)
#' stocks <- data.frame(
#'   sector = rep(c("Tech", "Fin"), each = 4),
#'   ret    = c(0.01, 0.03, -0.02, 0.05, 0.02, -0.01, 0.01, 0.04)
#' )
#' result <- iaw$rbind.mc.by(stocks, stocks$sector, function(sub) {
#'   sub$z <- scale(sub$ret)
#'   sub
#' })
#' result  # 8-row data frame with z-score column added
#'
#' # Filter within groups in parallel, then combine survivors
#' result <- iaw$rbind.mc.by(stocks, stocks$sector, function(sub) {
#'   sub[sub$ret > median(sub$ret), ]
#' })
#' nrow(result)  # only rows above within-group median
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
