preamble <- c(doc= '
@TITLE plot.timearea
@AUTHOR ivo.welch@gmail.com
@DATE 2023
@DESCRIPTION

  this is a plot that goes from left to right.
  on the x axis (as rownames) is year
  on the y axis (as colnames) are the quantities
  the data should be in a data frame

@USAGE plot.timearea( df, col, tcol, roundme )
@ARGUMENTS
@DETAILS
@SEEALSO
@EXAMPLES
  testdf <- data.frame( africa=c(5,10,15,20), china=c(10, 50, 60, 200), rest=c(30, 30, 30, 30) )
  rownames(testdf) <- c("1900", "1950", "2000", "2050e")

  plot.timearea(  testdf, c("black", "red", "yellow" ) )
', test= '
', changes= '
')


iaw$invcolor <- function( colvec ) {
    intens.vec <- (with( as.data.frame(t(col2rgb(colvec))), (red*0.299 + green*0.587 + blue*0.114)))
    ifelse( intens.vec > 106, "#000000" , "#ffffff" )
}

iaw$plot.timearea <- function( df, col, tcol=NULL, textscaler= 1.0, roundme= 0 ) {
    stopifnot(length(col) == ncol(df))
    if (length(tcol) == 0) tcol <- iaw$invcolor( col )

    stopifnot( as.integer( rownames(df)[1] ) >= 1800 )  ## we have no data before 1800
    stopifnot( ncol(df) == length(col) )

    sumbyyr <- rowSums(df)
    XMX <- max( sumbyyr )
    
    par( mar=c(0.5,0.5,0.5,0.5) )
    plot( 0, xlim=c(1, nrow(df)+1), ylim=c( -XMX/2, XMX/2 )*1.1, type="n", axes=FALSE )

    ylow <-  -(sumbyyr/2)  ## vector of years

    for (yri in 1:nrow(df)) {
        ylow.thisyr <- ylow[yri]
        for (areai in 1:ncol(df)) {
            nup1 <- ylow.thisyr + df[ yri, areai ]
            if (yri == 1) iaw$hline( c(ylow.thisyr, nup1), col="black" )
            rect( yri, ylow.thisyr, yri+1, nup1, col=col[areai], border="white", lwd=0.5  )
            text( yri+0.5, (nup1 + ylow.thisyr)/2, colnames(df)[areai], col=tcol[areai], font=2, cex=sumbyyr[yri]/XMX*textscaler )
            ylow.thisyr <- nup1
        }
        text( yri+0.5, -XMX/2, rownames(df)[yri], pos=1 )
        text( yri+0.5, XMX/2, paste("\u03a3 =",round(sumbyyr[yri], roundme)), pos=3, cex=textscaler )
    }
}


