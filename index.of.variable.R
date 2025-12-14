
#' INDEX.OF.VARIABLE
#'
#' @name index.of.variable
#'
#' the location of a variable in a data frame
#'
#' @usage index.of.variable (namesvector, wanted)
#'
#' @param list.or.namevector usually a dataframe
#' @param wantedname the desired columnname
#'
#' @return an integer
#'
#' @aliases which.variable

iaw$index.of.variable <- function(list.or.namevector, wantedname) {
    if (is.list(list.or.namevector)) list.or.namevector <- names(list.or.namevector)
    which( list.or.namevector==wantedname, arr.ind=TRUE )
}

iaw$which.variable <- iaw$index.of.variable
