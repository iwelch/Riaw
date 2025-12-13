#' latlon
#'
#' converts a (lat,lon) from (-89.5-179.5) to (89.5,179.5) into a unique id from 1 to 64800 or the reverse
#'
#' @name latlonx , mklatlonid, invlatlon
#'
#' @return latlonx returns either a pair (lat,lon) or a scalar (id), depending on call.  
#'
#' @examples
#'   > latlon( 35,-140)
#'    latlon 
#'     45041 
#'   > latlon( latlon( 35,-140 ) )
#'      lat  lon 
#'     35 -140 
#'
#' @seealso 

## The following is a dense coordinate system from latitude to longitude.  The system starts at 1 (not 0) for R convenience
##    best to think of it as undefined for fractional values.

### 3 types of calls:  latlon(35,-120) -> want id.  latlon( c(35,120) ) -> want id.  latlon( 50000 ) -> want lat lon

iaw$latlonx <- function( lat1, lon2=NULL ) {
    if (!is.null( ncol(lat1) )) {
        ## we are passing a matrix or dataframe
        if (ncol(lat1)>=2) return( iaw$mklatlonid( as.numeric(lat1[,1]), as.numeric( lat1[,2] )) ) ## a matrix of 2 vectors is standard
        return( invlatlon( as.numeric(argc[,1]) ) )  ## a matrix of 1 vector is an inverse operation
    }

    if (is.null(lon2)) {
        if (length(lat1)==1) return( iaw$invlatlon( lat1 ) )  ## latlon( 32121 )
        if (length(lat1)==2) return( iaw$mklatlonid( lat1[1], lat1[2] ) ) ## latlon( c(40,-130) )

        print(lat1); stop("[bad arguments to latlon]")
    } else {
        return(iaw$mklatlonid( lat1, lon2 ))  ## latlon( 40, -130 )
    }
}


iaw$mklatlonid <- function( lat, lon=NULL ) {
    if ((is.null(lon)) & (length(lat==2))) { lon <- lat[2]; lat <- lat[1] } ## mklatlonid( c(30,-140) ) -> mklatlonid( 30, -140 )
    stopifnot( all( is.na(lat) | (abs(lat) <= 90)) )
    stopifnot( all( is.na(lon) | (abs(lon) <= 180)) )

    lat <- as.integer( lat + 90) ## 
    lon <- as.integer( lon + 180) ## otherwise, we get the 0 line to go through alaska on a -0.5 offset

    c( latlon= as.integer( lat*360 + lon ) + 1 )
}


## note that (0,0) is interpreted as (0.5,0.5).  thus GMT = 0,0 = 0.5,0.5 = id 32581
iaw$invlatlon <- function( latlonid ) {
    stopifnot( all(latlonid > 0) )
    stopifnot( all(latlonid <= 360*180) )

    FORSURE <- 0.01  ## not needed.
    rv <- cbind( lat=as.integer( (latlonid-1 +FORSURE)/360) - 90 + 0.5, lon=as.integer((latlonid-1 +FORSURE)%%360) - 180 + 0.5 )

    if (is.null(dim(latlonid))) return(rv)
    as.numeric( lat=rv[1], lon=rv[2] )
}


iaw$testlatlon <- function( ) {
    for (lat in (-89.5):(+89.5)) {
        for (lon in (-179.5):(+179.5)) {
            id <- iaw$mklatlonid(lat, lon)
            latlon <- iaw$invlatlon( id )
            if ( abs(latlon[1] - lat) > 1e-4 ) {
                message("LAT bad latlon xfer :", lat, ",calc=", latlon[1], ": on lon=", lon, " to id=", id, " and laton=", latlon)
                stop("CHECK")
            }
            if ( abs(latlon[2] - lon) > 1e-4 ) {
                message("LON bad latlon xfer :", lon, ",calc=", latlon[2], ": on lon=", lat, " to id=", id, " and laton=", latlon)
                stop("CHECK")
            }
        }
    }
    for (id in 1:(360*180)) {
        latlon <- iaw$invlatlon( id )
        mid <- iaw$mklatlonid(latlon[1], latlon[2])

        stopifnot( mid==id )
    }
    message( "all latlon coordinates check out" )
}
