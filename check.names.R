
#' CHECK.NAMES
#'
#' @name check.names
#'
#' tell me what names are *not* in the data frame.  abort if not all are there
#'
#' @usage check.by.output( wantednames, dataframe )
#'
#' @param 
#'
#' @return
#'

iaw$check.names <- function( wanted, df ) {

    (is.data.frame(df)) %or% "second argument to iaw$checknames must be a data frame"
    (is.character(wanted)) %or% "first argument to iaw$checknames must be a character vector for names"
    
    m <- setdiff( wanted, names(df) )

    (length( m ) == 0) %or% "iaw$checknames: names do not exist: {{ print(m) }}\n"
}


