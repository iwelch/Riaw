#' Apply Function by Groups and Combine Results (Parallel)
#'
#' Applies a function to each group of a data frame in parallel and combines
#' the results using \code{rbind}. This is a convenience wrapper around
#' \code{iaw$mc.by()}.
#'
#' @param indataframe A data frame to process.
#' @param INDICES A factor or vector defining groups.
#' @param FUNIN A function to apply to each group's data frame subset.
#'   Should return a vector or single-row data frame.
#' @param ... Additional arguments passed to \code{FUNIN}.
#' @param FAST Logical; if TRUE, uses faster but less flexible \code{simplify2array}.
#'   Default FALSE.
#'
#' @return A data frame (or matrix if FAST=TRUE) with results from all groups
#'   combined row-wise.
#'
#' @export
#'
#' @seealso \code{\link{iaw$mc.by}}, \code{\link{iaw$rbind.oc.by}} for single-core version
#'
#' @examples
#' df <- data.frame(
#'     firm = rep(LETTERS[1:10], each = 100),
#'     year = rep(1:100, 10),
#'     revenue = rnorm(1000, 100, 20)
#' )
#'
#' # Summarize each firm in parallel
#' result <- iaw$rbind.mc.by(df, df$firm, function(d) {
#'     data.frame(
#'         firm = d$firm[1],
#'         mean_rev = mean(d$revenue),
#'         sd_rev = sd(d$revenue),
#'         n = nrow(d)
#'     )
#' })

iaw$rbind.mc.by <- function(indataframe, INDICES, FUNIN, ..., FAST = FALSE) {
    result <- iaw$mc.by(indataframe, INDICES, FUNIN, ...)

    if (FAST) {
        t(simplify2array(result))
    } else {
        as.data.frame(do.call("rbind", result))
    }
}
