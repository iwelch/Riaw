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
#'
#' # Visualize stock return distribution vs. theoretical normal
#' set.seed(7)
#' returns <- rt(500, df = 5) * 0.02  # fat-tailed daily returns
#' iaw$plot.ecdf.lineplot(returns, main = "Return ECDF vs Normal",
#'                        xlab = "Daily return", col = "black")
#' curve(pnorm(x, mean(returns), sd(returns)), add = TRUE,
#'       col = "red", lty = 2)
#' legend("topleft", c("Empirical", "Normal"), col = c("black", "red"),
#'        lty = c(1, 2))
#'
#' # ECDF of absolute forecast errors (model evaluation)
#' set.seed(3)
#' errors <- abs(rnorm(300, sd = 2))
#' iaw$plot.ecdf.lineplot(errors, main = "Abs forecast error ECDF",
#'                        xlab = "|error|", ylab = "Cumulative fraction")
#' abline(h = 0.9, lty = 3, col = "gray")  # 90th percentile reference
#' }
#'
#' @family plotting
#' @export

iaw$plot.ecdf.lineplot <- function(x, ...) {
    stopifnot(is.numeric(x))
    plot(ecdf(x), ...)
    invisible(NULL)
}
