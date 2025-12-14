#' Compound Returns Over Time
#'
#' Calculates cumulative compounded returns from a series of periodic returns.
#' Converts simple returns to wealth indices or rolling compounded returns.
#'
#' @param rate.of.return.timeseries A numeric vector of returns (e.g., 0.05 for 5\%).
#' @param window Integer specifying the compounding window:
#'   \itemize{
#'     \item 0: Compound from the start (cumulative return)
#'     \item n > 0: Rolling n-period compounded return
#'   }
#' @param geomean Logical; if TRUE, returns the geometric mean return instead
#'   of cumulative return. Default is FALSE.
#' @param na.is.zero Logical; if TRUE, treats NA values as zero returns.
#'   Default is FALSE (errors if NAs present).
#'
#' @return A numeric vector of the same length as input containing compounded
#'   returns.
#'
#' @details
#' The compounding formula is: \code{prod(1 + r) - 1}, computed using
#' log-sum-exp for numerical stability.
#'
#' @export
#'
#' @seealso \code{\link{iaw$pchgseries}} for computing returns,
#'   \code{\link{iaw$cumulateseries}}, \code{\link{cumprod}}
#'
#' @examples
#' # Monthly returns
#' returns <- c(0.02, -0.01, 0.03, 0.01, -0.02, 0.04)
#'
#' # Cumulative return (wealth index - 1)
#' iaw$compoundseries(returns, window = 0)
#' # Shows cumulative return at each point
#'
#' # 3-month rolling return
#' iaw$compoundseries(returns, window = 3)
#'
#' # Geometric mean return
#' iaw$compoundseries(returns, window = 0, geomean = TRUE)
#'
#' # Handle missing values
#' returns_na <- c(0.02, NA, 0.03, 0.01)
#' iaw$compoundseries(returns_na, window = 0, na.is.zero = TRUE)

iaw$compoundseries <- function(rate.of.return.timeseries, window = 0,
                                geomean = FALSE, na.is.zero = FALSE) {
    (is.numeric(rate.of.return.timeseries)) %or% "returns must be numeric"
    (iaw$is.scalar(window)) %or% "window must be a scalar"
    (is.numeric(window)) %or% "window must be numeric"
    (iaw$is.scalar(geomean)) %or% "geomean must be a scalar"
    (is.logical(geomean)) %or% "geomean must be logical"
    (iaw$is.scalar(na.is.zero)) %or% "na.is.zero must be a scalar"
    (is.logical(na.is.zero)) %or% "na.is.zero must be logical"

    if (!na.is.zero) {
        all(!is.na(rate.of.return.timeseries)) %or%
            "NAs present; set na.is.zero=TRUE to treat as zero"
    }

    xx <- rate.of.return.timeseries
    if (na.is.zero) xx[is.na(xx)] <- 0

    if (window == 0) {
        logsum <- iaw$cumulateseries(log(1 + xx))
        if (geomean) return(exp(logsum / seq_along(logsum)) - 1)

        xx.w.0 <- exp(logsum) - 1
        if (na.is.zero) xx.w.0[is.na(rate.of.return.timeseries)] <- NA
        return(xx.w.0)
    }

    cx <- c(0, iaw$compoundseries(xx, 0))
    prevseries <- 1 + iaw$lagseries(cx, window)

    rv <- (1 + cx) / ifelse(is.na(prevseries), 1.0, prevseries) - 1
    rv <- rv[2:length(rv)]

    if (na.is.zero) rv[is.na(rate.of.return.timeseries)] <- NA

    rv
}
