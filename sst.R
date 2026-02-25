#' Total Sum of Squares
#'
#' @name sst
#'
#' Calculates total sum of squares.
#'
#' @param x Numeric vector.
#'
#' @return SST value.
#'
#' @family statistics
#' @export
#'
#' @examples
#' iaw$sst(c(1, 2, 3, 4, 5))   # 10  (sum of squared deviations from mean 3)
#'
#' # Constant vector: SST is 0
#' iaw$sst(rep(7, 5))           # 0
#'
#' # SST = (n-1) * var(x)
#' x <- c(2, 5, 1, 8, 3)
#' iaw$sst(x)
#' (length(x) - 1) * var(x)    # same value
#'
#' # Use SST to compute R-squared manually: 1 - SSR/SST
#' y <- c(1.2, 2.1, 3.0, 4.2, 5.1)
#' fit <- lm(y ~ c(1:5))
#' 1 - sum(resid(fit)^2) / iaw$sst(y)   # equals summary(fit)$r.squared

iaw$sst <- function(x) {
    stopifnot(is.numeric(x))
    sum((x - mean(x, na.rm = TRUE))^2, na.rm = TRUE)
}
