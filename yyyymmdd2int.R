#' Date Format Conversions
#'
#' Functions for converting between YYYYMMDD integer format and R Date objects.
#'
#' @name yyyymmdd-conversions
#' @rdname yyyymmdd-conversions
#'
#' @param yyyymmdd_or_int Numeric; either a YYYYMMDD integer (like 20240101)
#'   or a day number (days since 1970-01-01).
#' @param anything A date in any recognizable format.
#' @param output Character; desired output format.
#'
#' @export
#'
#' @examples
#' # Convert YYYYMMDD to day number
#' iaw$yyyymmdd.toggle(20240101)
#' # 19724
#'
#' # Convert day number back to YYYYMMDD
#' iaw$yyyymmdd.toggle(19724)
#' # 20240101

#' @rdname yyyymmdd-conversions
#' @export
iaw$yyyymmdd.toggle <- function(yyyymmdd_or_int) {
    if (all(is.na(yyyymmdd_or_int))) return(yyyymmdd_or_int)
    stopifnot(is.numeric(yyyymmdd_or_int))

    # Check if input looks like YYYYMMDD format
    if (all(is.na(yyyymmdd_or_int) | (yyyymmdd_or_int >= 19000000))) {
        return(as.numeric(as.Date(as.character(yyyymmdd_or_int), format = '%Y%m%d')))
    }

    # Check if input looks like day number
    if (all(is.na(yyyymmdd_or_int) | ((yyyymmdd_or_int <= 50000) & (yyyymmdd_or_int >= -300000)))) {
        return(as.numeric(format(as.Date(yyyymmdd_or_int), "%Y%m%d")))
    }

    stop("Unrecognized format in yyyymmdd.toggle")
}

#' @rdname yyyymmdd-conversions
#' @export
iaw$yyyymmdd <- function(anything, output = c("posix", "gregorian", "yyyymmdd", "weeknum", "weekday")) {
    if (length(output) != 1) {
        message("Please specify output format: ", paste(output, collapse = ", "))
        stop("Output format required")
    }

    if (all(is.na(anything))) return(anything)

    is.POSIXct <- function(x) inherits(x, "POSIXct")

    # Convert to Date if needed
    if (!is.POSIXct(anything)) {
        stopifnot(is.numeric(anything))
        if (all(is.na(anything) | (anything >= 19000000))) {
            anything <- as.Date(as.character(anything), format = '%Y%m%d')
        } else if (all(is.na(anything) | ((anything <= 50000) & (anything >= -100000)))) {
            anything <- as.Date(anything, "%Y%m%d")
        }
    }

    if (output == "posix") {
        return(anything)
    } else if (output %in% c("weeknum", "wnum")) {
        return(as.integer((as.numeric(anything) - 4) / 7))
    } else if (output %in% c("weekday", "wday")) {
        return(format(anything, format = "%a"))
    }

    stop("Unknown output format: ", output)
}
