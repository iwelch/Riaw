#' Fama-MacBeth Two-Stage Regression
#'
#' @name famamacbeth
#'
#' Implements Fama-MacBeth procedure for panel data.
#'
#' @param formula Regression formula.
#' @param data Data frame.
#' @param timeid Time period variable name.
#' @param printn Print summary.
#'
#' @return Summary statistics of coefficients.
#'
#' @family regression
#' @export
#'
#' @examples
#' \dontrun{
#' iaw$famamacbeth(ret ~ beta + size, data = panel)
#' }

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
