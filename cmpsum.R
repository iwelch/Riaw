#' Compound Sum (Total Return)
#'
#' Compounds a series of returns and returns only the final cumulative value.
#' Optionally annualizes or converts to a different frequency.
#'
#' @param rseries Numeric vector of returns (e.g., 0.05 for 5\%).
#' @param MPY Multiplier for annualization:
#'   \itemize{
#'     \item 0 or negative: Return raw compound (no adjustment)
#'     \item 12: Annualize monthly returns
#'     \item 252: Annualize daily returns
#'     \item 1: Return geometric mean per period
#'   }
#'
#' @return A single numeric value: the compounded return.
#'
#' @details
#' The formula is: \code{exp(MPY/n * sum(log(1+r))) - 1}
#' where n is the length of the series.
#'
#' @export
#'
#' @seealso \code{\link{iaw$compoundseries}}, \code{\link{iaw$geomean}}, \code{\link{prod}}
#'
#' @examples
#' # Monthly returns for a year
#' monthly_ret <- c(0.02, -0.01, 0.03, 0.01, -0.02, 0.04,
#'                  0.01, 0.02, -0.01, 0.03, 0.02, 0.01)
#'
#' # Total return over the period
#' iaw$cmpsum(monthly_ret)
#' # Approximately 0.158 (15.8%)
#'
#' # Annualized return (already 12 months, so same as total)
#' iaw$cmpsum(monthly_ret, MPY = 12)
#'
#' # Geometric mean monthly return
#' iaw$geomean(monthly_ret)

iaw$cmpsum <- function(rseries, MPY = 0) {
    multiplier <- ifelse(MPY <= 0, 1, MPY / length(rseries))
    exp(multiplier * sum(log(1 + rseries))) - 1
}

#' @rdname cmpsum
#' @export
iaw$compsum <- iaw$cmpsum

#' Geometric Mean Return
#'
#' Calculates the geometric mean of a return series (equivalent to
#' \code{cmpsum(x, MPY=1)}).
#'
#' @param x Numeric vector of returns.
#'
#' @return Geometric mean return per period.
#'
#' @export
#'
#' @examples
#' returns <- c(0.10, -0.05, 0.08, 0.03)
#' iaw$geomean(returns)

iaw$geomean <- function(x) {
    iaw$cmpsum(x, MPY = 1)
}
