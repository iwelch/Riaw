#' Latitude/Longitude Distance
#'
#' @name latlon.distance
#'
#' Calculates distance between lat/lon coordinates.
#'
#' @param lat1 First latitude.
#' @param lon1 First longitude.
#' @param lat2 Second latitude.
#' @param lon2 Second longitude.
#'
#' @return Distance in kilometers.
#'
#' @family utilities
#' @export
#'
#' @examples
#' iaw$latlon.distance(40.7128, -74.0060, 34.0522, -118.2437)

iaw$latlon.distance <- function(lat1, lon1, lat2, lon2) {
    stopifnot(is.numeric(lat1), is.numeric(lon1))
    stopifnot(is.numeric(lat2), is.numeric(lon2))

    R <- 6371
    lat1r <- lat1 * pi / 180
    lat2r <- lat2 * pi / 180
    dlat <- (lat2 - lat1) * pi / 180
    dlon <- (lon2 - lon1) * pi / 180

    a <- sin(dlat/2)^2 + cos(lat1r) * cos(lat2r) * sin(dlon/2)^2
    c <- 2 * atan2(sqrt(a), sqrt(1-a))
    R * c
}


## The following is a dense coordinate system from latitude to longitude.  The system starts at 1 (not 0) for R convenience
##    best to think of it as undefined for fractional values.

### 3 types of calls:  latlon(35,-120) -> want id.  latlon( c(35,120) ) -> want id.  latlon( 50000 ) -> want lat lon

#' Latitude/Longitude to ID Conversion
#'
#' @name latlonx
#'
#' Converts between lat/lon coordinates and unique integer IDs (1 to 64800).
#'
#' @param lat1 Latitude, or vector c(lat, lon), or single ID for inverse.
#' @param lon2 Longitude (optional).
#'
#' @return Integer ID or c(lat, lon) depending on input.
#'
#' @family utilities
#' @export

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


#' Make Latitude/Longitude ID
#'
#' @name mklatlonid
#'
#' Converts lat/lon to unique integer ID.
#'
#' @param lat Latitude (-90 to 90).
#' @param lon Longitude (-180 to 180).
#'
#' @return Integer ID (1 to 64800).
#'
#' @family utilities
#' @export

iaw$mklatlonid <- function( lat, lon=NULL ) {
    if ((is.null(lon)) & (length(lat==2))) { lon <- lat[2]; lat <- lat[1] } ## mklatlonid( c(30,-140) ) -> mklatlonid( 30, -140 )
    stopifnot( all( is.na(lat) | (abs(lat) <= 90)) )
    stopifnot( all( is.na(lon) | (abs(lon) <= 180)) )

    lat <- as.integer( lat + 90) ##
    lon <- as.integer( lon + 180) ## otherwise, we get the 0 line to go through alaska on a -0.5 offset

    c( latlon= as.integer( lat*360 + lon ) + 1 )
}


#' Inverse Latitude/Longitude from ID
#'
#' @name invlatlon
#'
#' Converts integer ID back to lat/lon coordinates.
#'
#' @param latlonid Integer ID (1 to 64800).
#'
#' @return Matrix with lat and lon columns.
#'
#' @family utilities
#' @export

## note that (0,0) is interpreted as (0.5,0.5).  thus GMT = 0,0 = 0.5,0.5 = id 32581
iaw$invlatlon <- function( latlonid ) {
    stopifnot( all(latlonid > 0) )
    stopifnot( all(latlonid <= 360*180) )

    FORSURE <- 0.01  ## not needed.
    rv <- cbind( lat=as.integer( (latlonid-1 +FORSURE)/360) - 90 + 0.5, lon=as.integer((latlonid-1 +FORSURE)%%360) - 180 + 0.5 )

    if (is.null(dim(latlonid))) return(rv)
    as.numeric( lat=rv[1], lon=rv[2] )
}


#' Test Latitude/Longitude Conversions
#'
#' @name testlatlon
#'
#' Verifies that lat/lon to ID conversions are invertible.
#'
#' @return Message on success, stops on failure.
#'
#' @family utilities
#' @export

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
