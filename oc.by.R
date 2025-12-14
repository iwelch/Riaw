#' Apply Function by Groups (Single Core)
#'
#' A fast single-core implementation of \code{by()} that applies a function
#' to subsets of a data frame defined by grouping indices.
#'
#' @param indata A data frame to process.
#' @param INDICES A factor or vector defining groups.
#' @param FUN A function to apply to each group's data frame subset.
#' @param ... Additional arguments passed to \code{FUN}.
#'
#' @return A list with one element per group, containing the result of
#'   applying \code{FUN} to each group.
#'
#' @details
#' This is a lightweight alternative to \code{by()} that uses \code{split()}
#' and \code{lapply()} internally. For parallel processing, use
#' \code{\link{iaw$mc.by}}.
#'
#' @export
#'
#' @seealso \code{\link{iaw$mc.by}} for parallel version,
#'   \code{\link{iaw$rbind.oc.by}} to combine results,
#'   \code{\link{by}}, \code{\link{lapply}}
#'
#' @examples
#' df <- data.frame(
#'     group = rep(c("A", "B", "C"), each = 10),
#'     value = rnorm(30)
#' )
#'
#' # Compute mean by group
#' iaw$oc.by(df, df$group, function(d) mean(d$value))
#'
#' # Return data frames
#' iaw$oc.by(df, df$group, function(d) {
#'     data.frame(group = d$group[1], mean = mean(d$value), n = nrow(d))
#' })

iaw$oc.by <- function(indata, INDICES, FUN, ...) {
    lapply(
        split(1:nrow(indata), INDICES),
        FUN = function(.index) FUN(indata[.index, , drop = FALSE], ...)
    )
}
