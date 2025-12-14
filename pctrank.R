#' Percentage Ranks (Percentiles)
#'
#' Calculates percentage ranks from 0 to numcategories (e.g., 0-100 for percentiles).
#' Useful for creating quantile portfolios.
#'
#' @param x A numeric vector.
#' @param numcategories Integer; number of categories. Default 100 (percentiles).
#'
#' @return An integer vector of the same length with values from 0 to numcategories.
#'
#' @export
#'
#' @seealso \code{\link{rank}}, \code{\link{iaw$dependent.rank}}, \code{\link{quantile}}
#'
#' @examples
#' x <- rnorm(1000)
#'
#' # Percentile ranks (0-100)
#' pct <- iaw$pctrank(x)
#' table(pct)  # Should be roughly uniform
#'
#' # Decile ranks (0-10)
#' dec <- iaw$pctrank(x, numcategories = 10)
#' table(dec)
#'
#' # Quintile ranks (0-5)
#' qnt <- iaw$pctrank(x, numcategories = 5)

iaw$pctrank <- function(x, numcategories = 100) {
    as.integer((numcategories + 1) * trunc(rank(x)) / (length(x) + 1))
}
