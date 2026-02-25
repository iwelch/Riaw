#' One-Core By (Serial)
#'
#' @name oc.by
#'
#' Serial version of mc.by.
#'
#' @param indata Data frame.
#' @param INDICES Grouping variable.
#' @param FUNIN Function to apply.
#' @param dataframeout helps with conversion reversion to original behavior
#' @param ... Additional arguments.
#'
#' @return List of results.
#'
#' @examples
#' # Compute per-group column means (serial version of mc.by)
#' df <- data.frame(
#'   group = c("A", "A", "B", "B", "B"),
#'   x     = c(1, 3, 2, 4, 6),
#'   y     = c(10, 20, 30, 40, 50)
#' )
#' result <- iaw$oc.by(df, df$group, function(sub) colMeans(sub[, c("x", "y")]))
#' do.call(rbind, result)
#'
#' # Add a derived column within each group
#' iaw$oc.by(df, df$group, function(sub) {
#'   sub$z <- sub$x - mean(sub$x)
#'   sub
#' })
#'
#' # Cross-sectional z-scores within each year (finance panel)
#' panel <- data.frame(
#'   year = c(2020, 2020, 2020, 2021, 2021, 2021),
#'   ret  = c(0.05, -0.02, 0.10, 0.03, -0.01, 0.07)
#' )
#' result <- iaw$oc.by(panel, panel$year, function(sub) {
#'   sub$zscore <- (sub$ret - mean(sub$ret)) / sd(sub$ret)
#'   sub
#' })
#' do.call(rbind, result)
#'
#' # Summarise each group into a single row (like dplyr::summarise)
#' sales <- data.frame(
#'   region = c("East", "East", "West", "West", "West"),
#'   revenue = c(100, 200, 150, 250, 300)
#' )
#' summ <- iaw$oc.by(sales, sales$region, function(sub) {
#'   c(mean_rev = mean(sub$revenue), n = nrow(sub))
#' })
#' do.call(rbind, summ)  # data.frame with mean_rev and n per region
#'
#' @family parallel
#' @export

iaw$oc.by <- function(indata, INDICES, FUNIN, dataframeout=TRUE, ...) {
  stopifnot(is.data.frame(indata))
  if (is.list(INDICES)) {
    stopifnot(all(lengths(INDICES) == nrow(indata)))
  } else if (is.vector(INDICES) || is.factor(INDICES)) {
    stopifnot(length(INDICES) == nrow(indata))
  } else {
    stop(paste("INDICES must be list or vector, not", typeof(INDICES), "len=", length(INDICES)))
  }

  ssplit <- split(seq_len(nrow(indata)), INDICES)
  lapply(ssplit, function(.index) {
    res <- FUNIN(indata[.index, , drop = FALSE], ...)
    if (dataframeout && is.atomic(res) && !is.matrix(res)) {
      as.data.frame(t(res))
    } else {
      res
    }
  })
}
