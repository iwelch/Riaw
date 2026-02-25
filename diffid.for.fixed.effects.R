#' Demean for Fixed Effects
#'
#' @name diffid.for.fixed.effects
#'
#' Subtracts group means from each observation for use in
#' fixed-effects regressions. Handles vectors and matrices/data frames.
#'
#' @param h Variable(s) to demean (vector, matrix, or data frame).
#' @param id Group identifier (will be coerced to factor).
#'
#' @return De-meaned values with the same dimensions as h.
#'
#' @family data-manipulation
#' @export
#'
#' @examples
#' # Demean a vector within groups (firm fixed effects)
#' y  <- c(3, 5, 4, 10, 12, 11)
#' id <- c("A", "A", "A", "B", "B", "B")
#' iaw$diffid.for.fixed.effects(y, id)   # A: -1,1,0  B: -1,1,0
#'
#' # Demean a matrix of regressors - all columns demeaned within firm
#' X  <- matrix(c(1, 2, 3, 10, 11, 12,
#'                0.1, 0.2, 0.3, 1.0, 1.1, 1.2), nrow = 6)
#' iaw$diffid.for.fixed.effects(X, id)
#'
#' \dontrun{
#' # Full within-estimator for fixed-effects regression
#' newy <- iaw$diffid.for.fixed.effects(y, id)
#' newx <- iaw$diffid.for.fixed.effects(X, id)
#' iaw$olm(newy ~ newx)
#' }
#'
#' # Demeaned values within each group sum to zero
#' vals <- c(10, 20, 30, 100, 200)
#' grp  <- c("A", "A", "A", "B", "B")
#' dm <- iaw$diffid.for.fixed.effects(vals, grp)
#' tapply(dm, grp, sum)  # A=0, B=0 (within-group sums)
#'
#' # Data frame input: demean multiple columns at once
#' df <- data.frame(revenue = c(5, 10, 15, 50, 60),
#'                  cost    = c(3, 7, 12, 40, 55))
#' iaw$diffid.for.fixed.effects(df, grp)
#'
#' # Numeric group IDs work too (coerced to factor)
#' iaw$diffid.for.fixed.effects(c(1, 3, 2, 8, 6), c(1, 1, 1, 2, 2))

iaw$diffid.for.fixed.effects <- function(h, id) {
    id <- droplevels(as.factor(id))
    xnew <- apply(as.matrix(h), 2, function(x) x - tapply(x, id, mean, na.rm = TRUE)[id])
    rownames(xnew) <- NULL
    xnew
}
