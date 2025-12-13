
iaw$kable <- function( df, both=FALSE, format="latex", booktabs=T, linesep="", ... ) {
    if (both) print( df )
    kable( df, format=format,  booktabs=booktabs, linesep=linesep, ... )
}
