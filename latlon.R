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
