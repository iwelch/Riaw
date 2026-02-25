#' Density Heatmap for Large Scatterplots
#'
#' @name plot.points.toomany
#'
#' Bins (x, y) into a grid and draws density-colored rectangles.
#' Useful when a scatterplot has too many points to render individually.
#'
#' @param x X values.
#' @param y Y values.
#' @param bandwidth Numeric vector of length 2: c(x_binwidth, y_binwidth).
#' @param inlogs If TRUE (default), color by log(1 + count) for better contrast.
#' @param colstrength Multiplier for color intensity. Default 1.
#' @param ... Additional arguments passed to rect().
#'
#' @return Invisible NULL.
#'
#' @examples
#' \dontrun{
#' # Density heatmap for 10,000 points that would be overplotted as dots
#' set.seed(1)
#' x <- rnorm(10000)
#' y <- rnorm(10000)
#' plot(range(x), range(y), type = "n", xlab = "x", ylab = "y",
#'      main = "Density heatmap")
#' iaw$plot.points.toomany(x, y, bandwidth = c(0.1, 0.1))
#'
#' # Correlated data with coarser bins
#' y2 <- 0.7 * x + rnorm(10000, sd = 0.7)
#' plot(range(x), range(y2), type = "n")
#' iaw$plot.points.toomany(x, y2, bandwidth = c(0.2, 0.2), inlogs = FALSE)
#'
#' # Daily stock return scatter (e.g., SPY vs QQQ) with fine bins
#' set.seed(99)
#' spy <- rnorm(5000, 0, 0.01)
#' qqq <- 1.1 * spy + rnorm(5000, 0, 0.005)
#' plot(range(spy), range(qqq), type = "n",
#'      xlab = "SPY return", ylab = "QQQ return", main = "Return heatmap")
#' iaw$plot.points.toomany(spy, qqq, bandwidth = c(0.002, 0.002))
#' abline(lm(qqq ~ spy), col = "red", lwd = 2)
#'
#' # Boost color intensity with colstrength for sparse data
#' set.seed(5)
#' x3 <- rnorm(500)
#' y3 <- rnorm(500)
#' plot(range(x3), range(y3), type = "n", main = "Boosted color")
#' iaw$plot.points.toomany(x3, y3, bandwidth = c(0.3, 0.3), colstrength = 3)
#' }
#'
#' @family plotting
#' @export

iaw$plot.points.toomany <- function( x, y, bandwidth, inlogs=TRUE, colstrength=1, ... ) {

    stopifnot( !is.null(x) )
    stopifnot( !is.null(y) )

    stopifnot( length(x) > 10 )
    stopifnot( length(y) > 10 )

    xb <- bandwidth[1] ## rowvals[2] - rowvals[1]
    yb <- bandwidth[2] ## colvals[2] - colvals[1]

    xn <- round(x/xb)*xb
    yn <- round(y/yb)*yb
    tbl <- table(xn,yn)
    if (inlogs) tbl <- log( 1 + tbl )

    rowvals <- as.numeric( rownames( tbl ) )
    colvals <- as.numeric( colnames( tbl ) )
    maxval <- max( unlist( tbl ) ) / colstrength

    for (rw in 1:nrow(tbl)) {
        for (cl in 1:ncol(tbl)) {
            center <- c(rowvals[rw], colvals[cl] )
            fracn <- tbl[rw,cl]/maxval
            rect( rowvals[rw] - xb/2, colvals[cl] - yb/2, rowvals[rw] + xb/2, colvals[cl] + yb/2,
                 col= gray(1.0 - fracn), border= NA, ... )

        }
    }
    invisible(NULL)
}
