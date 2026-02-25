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
