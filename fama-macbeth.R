#' fama.macbeth.gammas
#'
#' @name fama.macbeth.gammas
#'
#' calculates the time-series average of cross-sectional regression coefficients
#'
#' @usage iaw$famamacbeth.gammas( formula, data )
#'
#' @param ...
#'
#' @examples
#' print(head(famamacbeth.gammas( ret ~ bm + m, data=crsp )))
#'
#' @seealso
#'   install.packages("plm"); library(plm);  pmg(investment ~ mvalue + kstock, data = grunfeld, index = c("FIRM", "YEAR"))
#'
#' @return data.frame of yyyymm, degrees.freedom, coefficients
#'


iaw$famamacbeth.gammas <- function(formula, data, timeid="yyyymm") {
    stopifnot(is.character(timeid) & (length(timeid)==1))

    ## consider returning an S3 object instead with / print summary methods

    gamma.by.month <- iaw$rbind.oc.by( data, data[[timeid]],
                                      function(dthismo) {
                                          lmo <- tryCatch( lm( formula, data=dthismo ), error=function(e) NULL )
                                          if (is.null(lmo)) return(NULL) else
                                          c( timeid=first(dthismo[[timeid]]), df=lmo$df.residual, coefs=coef(lmo) )
                                      })
}





#' fama.macbeth
#'
#' @name fama.macbeth
#'
#' @usage
#'
#' @examples
#' print(famamacbeth( ret ~ bm + m, data=crsp ))
#'
#' @return

iaw$famamacbeth <- function(formula, data, timeid="yyyymm", printn= TRUE) {
    gs <- iaw$famamacbeth.gammas( formula, data, timeid )
    if (printn) {
        gsminn <- subset(gs, gs$df==min(gs$df))
        gsmaxn <- subset(gs, gs$df==max(gs$df))
        cat("\nRange of df: ", gsminn$df, "(on ", gsminn[[timeid]], ") to ", gsmaxn$df, "(on ", gsmaxn[[timeid]], ").  avg=", mean(gs$df), "\n", sep="");
    }

    ##     gamma.se.nw <- apply(yearlyregs, 2,
    ##                         function(x) sqrt(NeweyWest(lm(x ~ 1),
    ##                                                    lag = lags, prewhite = FALSE)['(Intercept)','(Intercept)']))

    iaw$summary(gamma.by.month, "x")  ## contains pct pos
}
