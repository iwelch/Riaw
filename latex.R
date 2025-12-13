#' print.latex.R
#'
#' @name print.latex.R
#'
#'   take a data frame and create a latex table from it.
#'
#'  @details verbose can be a choicelist.  functions can be lists
#'
#'  @usage latex( iaw$summary (d, "all") )
#'
#'  @param usually a data frame
#'
#'  @return a long string, printable
#'
#'  @examples
#' 	dd <- data.frame( x=rnorm(100), y=rnorm(100), z=rnorm(100) ); dd$y[20] <- NA
#' 	iaw.summary(dd, verbose=c("mean", "sd"), as.latex=TRUE, extend.f = function(x) c(sqm=mean(x^2), cbm=mean(x^3)) )
#'


iaw$print.latex <- function(d, digitstring="%15.5f") {
    iaw$msg("please use library(xtable) instead of print.latex().  see also kable() and iaw$kable")
}
