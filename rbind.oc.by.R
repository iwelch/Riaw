#' Apply Function by Groups and Combine Results (Single Core)
#'
#' Applies a function to each group of a data frame and combines the results
#' using \code{rbind}. This is a convenience wrapper around \code{iaw$oc.by()}.
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
#' @seealso \code{\link{iaw$oc.by}}, \code{\link{iaw$rbind.mc.by}} for parallel version
#'
#' @examples
#' df <- data.frame(
#'     firm = rep(c("A", "B", "C"), each = 10),
#'     year = rep(2010:2019, 3),
#'     revenue = rnorm(30, 100, 20)
#' )
#'
#' # Summarize each firm
#' iaw$rbind.oc.by(df, df$firm, function(d) {
#'     c(firm = d$firm[1], mean_rev = mean(d$revenue), n = nrow(d))
#' })

iaw$rbind.oc.by <- function(indataframe, INDICES, FUNIN, ..., FAST = FALSE) {
    result <- iaw$oc.by(indataframe, INDICES, FUNIN, ...)

    if (FAST) {
        t(simplify2array(result))
    } else {
        as.data.frame(do.call("rbind", result))
    }
}
