#' ECDF Line Plot
#'
#' @name plot.ecdf.lineplot
#'
#' Plots empirical CDF.
#'
#' @param x Numeric vector.
#' @param ... Plot arguments.
#'
#' @return Invisible NULL.
#'
#' @examples
#' \dontrun{
#' # Plot ECDF of a standard normal sample
#' set.seed(42)
#' x <- rnorm(200)
#' iaw$plot.ecdf.lineplot(x, main = "ECDF of N(0,1) sample")
#'
#' # Compare two distributions on the same plot
#' x1 <- rnorm(200, mean = 0)
#' x2 <- rnorm(200, mean = 1)
#' iaw$plot.ecdf.lineplot(x1, col = "blue", main = "ECDF comparison")
#' iaw$plot.ecdf.lineplot(x2, col = "red", add = TRUE)
#' legend("topleft", c("N(0,1)", "N(1,1)"), col = c("blue", "red"), lty = 1)
#' }
#'
#' @family plotting
#' @export

iaw$plot.ecdf.lineplot <- function(x, ...) {
    stopifnot(is.numeric(x))
    plot(ecdf(x), ...)
    invisible(NULL)
}
