#' Compound Sum
#'
#' @name cmpsum
#'
#' Calculates compound sum of returns.
#'
#' @param r Numeric vector of returns.
#' @param MPY Months per year for annualization. If 0 (default), no scaling.
#'
#' @return Compound sum (scalar).
#'
#' @family statistics
#' @export
#'
#' @examples
#' # Total compound return over a sequence of period returns
#' iaw$cmpsum(c(0.1, 0.05, -0.02))
#'
#' # Compare two return streams: compounding matters vs simple sum
#' r1 <- c(0.05, 0.05, 0.05)
#' r2 <- c(0.20, -0.05, 0.00)
#' iaw$cmpsum(r1)  # steady returns compound better
#' iaw$cmpsum(r2)  # volatile path has lower compound sum
#'
#' # Annualized compound return (MPY = 12 for monthly data)
#' monthly_rets <- c(0.01, 0.02, -0.01, 0.015, 0.005, 0.02,
#'                   0.01, -0.005, 0.03, 0.01, 0.015, 0.02)
#' iaw$cmpsum(monthly_rets, MPY = 12)
#'
#' # geomean is shorthand for cmpsum(x, MPY = 1)
#' iaw$geomean(c(0.05, 0.10, -0.02))
#' iaw$cmpsum(c(0.05, 0.10, -0.02), MPY = 1)  # same

iaw$cmpsum <- function(r, MPY = 0) {
    stopifnot(is.numeric(r))
    stopifnot(is.numeric(MPY), length(MPY) == 1L)
    exp(ifelse(MPY <= 0, 1, MPY / length(r)) * sum(log(1 + r))) - 1
}

#' @rdname cmpsum
#' @export
iaw$compsum <- iaw$cmpsum

#' @rdname cmpsum
#'
#' @param x Numeric vector of returns (for \code{geomean}).
#'
#' @details \code{geomean} is shorthand for \code{cmpsum(x, MPY = 1)}.
#'
#' @export
iaw$geomean <- function(x) iaw$cmpsum(x, MPY = 1)
