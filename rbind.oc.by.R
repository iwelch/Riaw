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
#' # Industry-adjust returns: subtract within-group mean
#' stocks <- data.frame(
#'   industry = c("Tech", "Tech", "Fin", "Fin", "Fin"),
#'   ret      = c(0.05, 0.08, 0.02, 0.04, 0.03)
#' )
#' adjusted <- iaw$rbind.oc.by(stocks, stocks$industry, function(sub) {
#'   sub$adj_ret <- sub$ret - mean(sub$ret)
#'   sub
#' })
#' adjusted$adj_ret  # industry-adjusted returns sum to 0 within each group
#'
#' # Rank-transform within groups
#' ranked <- iaw$rbind.oc.by(stocks, stocks$industry, function(sub) {
#'   sub$rank <- rank(sub$ret)
#'   sub
#' })
#' ranked  # each group has independent 1..n ranking
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
