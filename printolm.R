
#' PRINTOLM
#'
#' @name print.summary.lm( iaw$olm( rnorm(10) ~ rnorm(10) ))
#'
#' use as iaw$olm( y ~ x );  for a short printer, use iaw$printolm( y ~ x )
#'
#'   add newey-west and standardized coefficients to lm(), and return the summary.lm
#'
#'  @details
#'     Note that when y or x have different observations, the coef(lm(y~x))*sd(x)/sd(y)
#'     calculations are different from a scale(y) ~ scale(x) regression.
#'
#'     Note that the NeweyWest statistics could also be calculated as
#'
#'       library(sandwich)
#'       se.nw <- NeweyWest( lmo, lag=0, prewhite=FALSE))
#'
#'  @usage olm(..., newey.west=0, stdcoefs=TRUE)
#'
#'  @param formula
#'  @param newey.west the number of lags
#'  @param stdcoefs whether to print the standardized coefficients or not.
#'
#'  @return the adjusted R^2 of the regression model
#'
#'  @seealso stats:::lm, stats:::summary.lm
#'
#'  @examples
#'     set.seed(0)
#'     x <- rnorm(12); y <- rnorm(12); z <- rnorm(12); x[2] <-NA;
#'     (abs(x[1]-1.2630)<0.01) %or% "sorry, but your random numbers have changed"
#'     cc<-lm( y ~ x + z )
#'     (abs(sum(coef(cc))-3.25)) %or% "probably invalid calculations"
#'
#'     iaw$print.summary.lm( iaw$olm( rnorm(10) ~ rnorm(10) ))
#'


## use as iaw$olm( y ~ x );  for a short printer, use iaw$printolm( y ~ x )

iaw$printolm <- function( ..., yname=NULL, xnames=NULL, wantedcols=1:7, description=NULL ) {

    va <- list(...)
    if ((length(va) == 1) && inherits(va[[1]], "summary.lm")) {
        ## should test further for an iaw$olm summary
        olmobject <- va[[1]]
    } else {
        olmobject <- iaw$olm( ... )
    }


    ccc <- coef( olmobject )[,wantedcols]
    csigma <- olmobject$sigma

    if (!is.null(xnames)) {
        if (length(xnames) == nrow(ccc)) {
            rownames(ccc) <- xnames
        } else if (length(xnames) == nrow(ccc)-1) {
            rownames(ccc) <- c("const", xnames)
        } else {
            stop("Number of xnames: ", length(xnames), " number of variables: ", nrow(ccc), "\n")
        }
    }
    if (!is.null(description)) cat("---------------- Model: ", description, "\n")
    if (is.null(yname)) yname <- as.character(formula(olmobject))[2]
    cat("--- Explaining '", yname, "':\n")
    print(ccc)
    cat("--- ar^2= ", olmobject$adj.r.squared, " df= ", olmobject$df[2], "  ( N=",olmobject$df[2]+olmobject$df[1],"),  sde=", csigma,
        "  pctsumsqe=", olmobject$residssq, "\n\n")
    invisible( olmobject$adj.r.squared )
}

