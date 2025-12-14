#' Latitude/Longitude Coordinate Conversion
#'
#' Converts between (latitude, longitude) coordinates and a unique integer ID
#' from 1 to 64800 (180 * 360 grid cells). Useful for efficient storage and
#' lookup of geographic data.
#'
#' @name latlon
#' @rdname latlon
#'
#' @param lat1 Either a latitude value, a vector c(lat, lon), or a matrix.
#' @param lon2 Longitude value (optional if lat1 contains both).
#' @param lat Latitude value (-89.5 to 89.5).
#' @param lon Longitude value (-179.5 to 179.5).
#' @param latlonid Integer ID (1 to 64800).
#'
#' @return
#' \itemize{
#'   \item \code{latlonx}: Coordinates or ID depending on input
#'   \item \code{mklatlonid}: Integer ID for the coordinate
#'   \item \code{invlatlon}: Matrix with lat and lon columns
#' }
#'
#' @export
#'
#' @examples
#' # Coordinate to ID
#' iaw$latlonx(35, -140)
#' # 45041
#'
#' # ID back to coordinate
#' iaw$latlonx(45041)
#' # lat: 35, lon: -140
#'
#' # Vector input
#' iaw$latlonx(c(35, -140))

#' @rdname latlon
#' @export
iaw$latlonx <- function(lat1, lon2 = NULL) {
    if (!is.null(ncol(lat1))) {
        if (ncol(lat1) >= 2) {
            return(iaw$mklatlonid(as.numeric(lat1[, 1]), as.numeric(lat1[, 2])))
        }
        return(iaw$invlatlon(as.numeric(lat1[, 1])))
    }

    if (is.null(lon2)) {
        if (length(lat1) == 1) return(iaw$invlatlon(lat1))
        if (length(lat1) == 2) return(iaw$mklatlonid(lat1[1], lat1[2]))
        stop("Invalid arguments to latlon")
    }

    iaw$mklatlonid(lat1, lon2)
}

#' @rdname latlon
#' @export
iaw$mklatlonid <- function(lat, lon = NULL) {
    if (is.null(lon) && length(lat) == 2) {
        lon <- lat[2]
        lat <- lat[1]
    }
    stopifnot(all(is.na(lat) | (abs(lat) <= 90)))
    stopifnot(all(is.na(lon) | (abs(lon) <= 180)))

    lat <- as.integer(lat + 90)
    lon <- as.integer(lon + 180)

    c(latlon = as.integer(lat * 360 + lon) + 1)
}

#' @rdname latlon
#' @export
iaw$invlatlon <- function(latlonid) {
    stopifnot(all(latlonid > 0))
    stopifnot(all(latlonid <= 360 * 180))

    cbind(
        lat = as.integer((latlonid - 1) / 360) - 90 + 0.5,
        lon = as.integer((latlonid - 1) %% 360) - 180 + 0.5
    )
}
