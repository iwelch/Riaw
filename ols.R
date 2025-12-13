
#' OLS
#'
#' @name ols
#'
#' an OLS coefficient creator.
#'
#' @details an interface to ols.nona()
#'
#' @usage
#'     ols( rnorm(10), data.frame( rep(1,10), x=rnorm(10), z=rnorm(10) ), detail=2 )
#' or
#'     d <- data.frame( y=rnorm(10), x=rnorm(10), z=rnorm(10) )
#'     ols( y ~ x + z, data=d, detail=1 )
#'
#'
#' @param either y is a formula and data is a data frame, or data is an x matrix
#'
#' @return depends on the detail requested.  see ols.nona
#'


iaw$ols <- function( y.or.f, data, detail=0 ) {
    if (inherits(y.or.f,"formula")) {
        yx <- model.frame( y.or.f, data= data )
        y.or.f <- yx[,1]
        data <- yx[,-1]
        if (attr( attr(yx, "terms"), "intercept")) data <- cbind( "intcpt"=1, data )
        rm(yx)
    }

    ## maybe better written with "complete.cases()" on subset

    ## y is now always a vector.  data is now always a matrix or data frame
    one.bad <- (is.na(y.or.f)) | apply(data, 1, function(x) any(is.na(x)))

    iaw$ols.nona( y.or.f[!one.bad], as.matrix(data[!one.bad,]), detail )
}
