
#' wide2long
#'
#' easy interface to take a data frame and reshaping it.
#'
#' @name wide2long
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
#' @seealso wide2long( d, "id1", "id2", "val" )
#'


iaw$wide2long <- function(d, valname.is="val", row.is="time", col.is="unit") {
    "note: matrx.  column is usually time, row is usually firm; output: time varies first, then unit";

    stopifnot( is.matrix(d)|is.data.frame(d) )  ## please hand a matrix to us with rownames and colnames
    stopifnot( is.character( valname.is ) & length(valname.is)==1 )
    stopifnot( is.character( col.is ) & length(col.is)==1 )
    stopifnot( is.character( row.is ) & length(row.is)==1 )

    rv <- reshape(as.data.frame(d), direction="long",
                  idvar=col.is, timevar=row.is,
                  varying= list(colnames(d)), times=colnames(d), ids= rownames(d))
           
    rv <- data.frame( rv[,1], rv[,3], rv[,2] )
    names(rv) <- c(row.is, col.is, valname.is)
    return( rv )

    iaw$wide2long( o, "tval" )
    ## reshape( d, direction="long", idvar= "unit", timevar= "time",
    ## varying= list(colnames(d)), times= colnames(d), ids= rownames(d) )
}

    ## test
    ## o <- matrix( 1:(3*7), 7, 3 )
    ## rownames(o) <- paste0("r",1:7)
    ## colnames(o) <- paste0("c",1:3)
