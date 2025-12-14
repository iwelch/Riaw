#' Fama-MacBeth Two-Stage Regression
#'
#' Implements the Fama-MacBeth (1973) two-pass regression procedure for panel
#' data. First runs cross-sectional regressions for each time period, then
#' computes time-series statistics of the coefficient estimates.
#'
#' @name famamacbeth
#'
#' @param formula A formula specifying the regression model (e.g., \code{ret ~ beta + size}).
#' @param data A data frame containing the variables in the formula plus a time identifier.
#' @param timeid Character string naming the time period variable. Default is \code{"yyyymm"}.
#' @param printn Logical; if TRUE (default), prints summary statistics about degrees of freedom.
#'
#' @return
#' \itemize{
#'   \item \code{famamacbeth.gammas}: A data frame with columns for time period,
#'     degrees of freedom, and each coefficient estimate.
#'   \item \code{famamacbeth}: Summary statistics of the coefficient estimates
#'     across time periods.
#' }
#'
#' @details
#' The Fama-MacBeth procedure is commonly used in asset pricing to test whether
#' certain characteristics explain cross-sectional variation in returns:
#'
#' \enumerate{
#'   \item For each time period t, run a cross-sectional regression:
#'     \deqn{r_{i,t} = \gamma_{0,t} + \gamma_{1,t} X_{1,i,t} + ... + \epsilon_{i,t}}
#'   \item Compute time-series statistics (mean, t-stat) of the \eqn{\gamma} estimates.
#' }
#'
#' The standard errors from step 2 account for cross-sectional correlation because
#' each \eqn{\gamma_t} is estimated independently.
#'
#' @section Note:
#' For Newey-West adjusted standard errors on the second-stage estimates, you can
#' apply Newey-West manually to the time-series of coefficients returned by
#' \code{famamacbeth.gammas()}.
#'
#' @references
#' Fama, E. F., & MacBeth, J. D. (1973). Risk, Return, and Equilibrium: Empirical
#' Tests. \emph{Journal of Political Economy}, 81(3), 607-636.
#'
#' @export
#'
#' @seealso \code{\link{lm}}, \code{\link[plm]{pmg}} for an alternative implementation
#'
#' @examples
#' # Simulate panel data
#' set.seed(42)
#' n_firms <- 100
#' n_months <- 60
#'
#' panel <- data.frame(
#'     firm = rep(1:n_firms, n_months),
#'     yyyymm = rep(201001:201060, each = n_firms),
#'     beta = rnorm(n_firms * n_months, 1, 0.3),
#'     size = rnorm(n_firms * n_months, 0, 1)
#' )
#' # Returns depend on beta and size plus noise
#' panel$ret <- 0.01 + 0.005 * panel$beta - 0.002 * panel$size + rnorm(nrow(panel), 0, 0.05)
#'
#' # Get coefficient estimates for each month
#' gammas <- iaw$famamacbeth.gammas(ret ~ beta + size, data = panel)
#' head(gammas)
#'
#' # Get summary statistics (means and t-stats across months)
#' iaw$famamacbeth(ret ~ beta + size, data = panel)
#'
#' # The coefficient on beta should be close to 0.005
#' # The coefficient on size should be close to -0.002

#' @rdname famamacbeth
#' @export
iaw$famamacbeth.gammas <- function(formula, data, timeid = "yyyymm") {
    stopifnot(is.character(timeid) && length(timeid) == 1)
    stopifnot(timeid %in% names(data))

    gamma.by.month <- iaw$rbind.oc.by(
        data,
        data[[timeid]],
        function(dthismo) {
            lmo <- tryCatch(
                lm(formula, data = dthismo),
                error = function(e) NULL
            )
            if (is.null(lmo)) {
                return(NULL)
            }
            c(timeid = data.table::first(dthismo[[timeid]]),
              df = lmo$df.residual,
              coefs = coef(lmo))
        }
    )

    gamma.by.month
}


#' @rdname famamacbeth
#' @export
iaw$famamacbeth <- function(formula, data, timeid = "yyyymm", printn = TRUE) {
    gs <- iaw$famamacbeth.gammas(formula, data, timeid)

    if (printn) {
        gsminn <- subset(gs, gs$df == min(gs$df))
        gsmaxn <- subset(gs, gs$df == max(gs$df))
        cat("\nRange of df: ", gsminn$df[1], " (on ", gsminn[[timeid]][1], ") to ",
            gsmaxn$df[1], " (on ", gsmaxn[[timeid]][1], "). avg=", mean(gs$df), "\n", sep = "")
    }

    iaw$summary(gs, "x")
}
