#' Fama-MacBeth Two-Stage Regression
#'
#' @name famamacbeth
#'
#' Implements Fama-MacBeth procedure for panel data.
#' \code{famamacbeth.gammas} runs cross-sectional regressions per time period.
#' \code{famamacbeth} calls \code{famamacbeth.gammas} and summarizes the
#' time-series distribution of coefficients.
#'
#' @param formula Regression formula.
#' @param data Data frame with panel data.
#' @param timeid Character. Column name identifying time periods (default \code{"yyyymm"}).
#'
#' @return \code{famamacbeth.gammas} returns a data frame of per-period coefficients.
#'
#' @family regression
#' @export
#'
#' @examples
#' # Build a small panel: 3 stocks x 4 months
#' set.seed(42)
#' panel <- data.frame(
#'   yyyymm = rep(c(202001, 202002, 202003, 202004), each = 3),
#'   firm   = rep(c("A", "B", "C"), times = 4),
#'   ret    = rnorm(12, mean = 0.01, sd = 0.05),
#'   beta   = rnorm(12, mean = 1, sd = 0.3),
#'   size   = rnorm(12, mean = 5, sd = 1)
#' )
#'
#' # Run full Fama-MacBeth: cross-sectional regressions each month,
#' # then summarize time-series of gammas
#' iaw$famamacbeth(ret ~ beta + size, data = panel)
#'
#' # Get the per-period gammas directly
#' gs <- iaw$famamacbeth.gammas(ret ~ beta + size, data = panel)
#' gs

iaw$famamacbeth.gammas <- function(formula, data, timeid = "yyyymm") {
    stopifnot(inherits(formula, "formula"))
    stopifnot(is.data.frame(data))
    stopifnot(is.character(timeid), length(timeid) == 1L)
    stopifnot(timeid %in% names(data))
    
    gamma.by.month <- iaw$rbind.oc.by(
        data,
        data[[timeid]],
        function(dthismo) {
            lmo <- tryCatch(lm(formula, data = dthismo), error = function(e) NULL)
            if (is.null(lmo)) return(NULL)
            c(timeid = dthismo[[timeid]][1], df = lmo$df.residual, coefs = coef(lmo))
        }
    )
    gamma.by.month
}

#' @rdname famamacbeth
#'
#' @param printn Logical. Print N diagnostics (default \code{TRUE}).
#'
#' @return \code{famamacbeth} returns summary statistics of the per-period coefficients.
#'
#' @export
iaw$famamacbeth <- function(formula, data, timeid = "yyyymm", printn = TRUE) {
    gs <- iaw$famamacbeth.gammas(formula, data, timeid)
    
    if (printn) {
        gsminn <- subset(gs, gs$df == min(gs$df))
        gsmaxn <- subset(gs, gs$df == max(gs$df))
        cat("\nRange of df:", gsminn$df[1], "to", gsmaxn$df[1], 
            "avg=", mean(gs$df), "\n")
    }
    
    iaw$summary(gs, "x")
}
