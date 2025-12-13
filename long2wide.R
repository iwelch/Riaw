
#' long2wide
#'
#' easy interface to take a data frame and reshaping it.
#'
#' @name long2wide
#'
#' @param d  a data frame
#'
#' @param id1  the name of the row variable
#'
#' @param id2  the name of the col variable
#'
#' @return a vector of autocorrelation coefficients
#'
#' @examples
#'
#' @seealso long2wide( d, "id1", "id2", "val" )
#' its got perverse ideas.  we want id on the column, and time on the row.

iaw$long2wide <- function(d, id.time, id.firm, id.val) {
    stopifnot( is.data.frame(d) )
    stopifnot( is.character( id.time ) & length(id.time)==1 )
    stopifnot( is.character( id.firm ) & length(id.firm)==1 )
    stopifnot( is.character( id.val ) & length(id.val)==1 )
    stopifnot( exists( id.time, d ))
    stopifnot( exists( id.firm, d ))
    stopifnot( exists( id.val, d ))
    rv <- reshape(subset(d, TRUE, select=c(id.firm,id.time,id.val)), idvar=id.firm, timevar=id.time, direction="wide" )
    rownames(rv) <- rv[,1]
    rv <- rv[,2:ncol(rv)]
    rv
}
