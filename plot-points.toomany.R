#' Plot Points with Subsampling
#'
#' @name plot.points.toomany
#'
#' Plots subsampled points for large datasets.
#'
#' @param x X values.
#' @param y Y values.
#' @param maxpoints Maximum points to plot.
#' @param ... Plot arguments.
#'
#' @return Invisible NULL.
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
}
