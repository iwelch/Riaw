preamble <- c(doc= '
@TITLE contour
@AUTHOR ivo.welch@gmail.com
@DATE 2013
@DESCRIPTION
@USAGE
@ARGUMENTS
@DETAILS
@SEEALSO grid2arr
@EXAMPLES
', test= '
', changes= '
')

iaw$contour <- function( x, y, z, show.points=0, show.min=0, def.grid=200, ... ) {
    z.ls <- loess(z ~ x*y)
    xs <- seq(min(x), max(x), length.out=def.grid)
    ys <- seq(min(y), max(y), length.out=def.grid)
    grid <- expand.grid(x=xs, y=ys)
    z.fit <- predict(z.ls, grid)
    contour(xs,ys,z.fit, ...)
    if (show.points) points(x, y, pch=20, cex=0.5, col="gray")  ## what it was based on
    if (show.min) {
        xxx<-which(z.fit == min(z.fit,na.rm=TRUE), arr.ind = TRUE)
        xm <- xs[ xxx[1,1] ]
        ym <- xs[ xxx[1,2] ]
        iaw$vline(xm, col="blue")
        iaw$hline(ym, col="blue")
        text( xm, ys[1], round(xm,2), col="blue", pos=2 )
        text( xs[1], ym, round(ym,2), col="blue", pos=1 )
    }
}

iaw$grid2contour <- function( d ) {
    ## input MUST come from expand.grid
    stopifnot( is.numeric( d[,1] ) ); x <- sort(unique(d[,1]))
    stopifnot( is.numeric( d[,2] ) ); y <- sort(unique(d[,2]))
    z <- matrix( d[,3], nrow=length(x), ncol=length(y) )
    z <- z[ , ncol(z):1, drop=FALSE ]
    rownames(z) <- x
    colnames(z) <- y
    list(x=x,y=y,z=z)  # use as 'with( grid2contour(gridme), contour( x, y, z ) )'
}


preamble <- c(doc= '
@TITLE grid2arr
@AUTHOR ivo.welch@gmail.com
@DATE 2016
@DESCRIPTION

   above is the better for perfect grid.

   given x, y, and z value, try to make an two-dimensional array out of it, where
   the x,y value in each cell is z.  this makes sense to use only when most---but not
   necessarily all---points are on a regular grid.

@USAGE
@ARGUMENTS
@DETAILS
@SEEALSO contour
@EXAMPLES
', test= '
', changes= '
')

iaw$grid2arr <- function( x, y, z ) {
   xuniq <- unique(x)
   yuniq <- unique(y)
   n <- expand.grid( x=xuniq, y=yuniq ) ## now we have an assured complete grid
   n <- merge( data.frame(x,y,z), n, all.x=FALSE, all.y=TRUE)
   n <- n[order(n$y,n$x),]
   ## now we can do a quick reshape
   n <-data.frame(matrix(n$z, nrow=length(xuniq), ncol=length(yuniq)))
   rownames(n) <- xuniq
   colnames(n) <- yuniq
   n  ## the output is a matrix with appropriate row and col names
}
