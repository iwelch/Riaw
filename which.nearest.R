#' Find Index of Nearest Value in a Vector
#'
#' Returns the index of the element in a numeric vector that is closest to a
#' target value.
#'
#' @param nvec A numeric vector to search (the haystack).
#' @param your.number The target value to find (the needle).
#'
#' @return An integer index of the element with the smallest absolute distance
#'   from \code{your.number}. If there are ties, returns the first match.
#'
#' @export
#'
#' @seealso \code{\link{iaw$nearest}}, \code{\link{which.min}}
#'
#' @examples
#' x <- c(1, 5, 10, 15, 20)
#'
#' iaw$which.nearest(x, 12)
#' # 3 (index of 10, which is closest to 12)
#'
#' iaw$which.nearest(x, 7)
#' # 2 (index of 5, which is closest to 7)
#'
#' # Use to subset
#' x[iaw$which.nearest(x, 12)]
#' # 10
#'
#' # Find nearest date
#' dates <- as.Date(c("2020-01-01", "2020-06-15", "2020-12-31"))
#' target <- as.Date("2020-07-01")
#' dates[iaw$which.nearest(as.numeric(dates), as.numeric(target))]
#' # "2020-06-15"

iaw$which.nearest <- function(nvec, your.number) {
    (is.numeric(nvec) && is.vector(nvec)) %or%
        "which.nearest requires a numeric vector, not {{class(nvec)}}"
    which.min(abs(nvec - your.number))
}
