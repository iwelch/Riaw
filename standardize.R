#' Standardize a Vector
#'
#' @name standardize
#'
#' Centers and scales to mean=0, sd=1.
#'
#' @param x Numeric vector.
#'
#' @return Numeric vector.
#'
#' @family data-transformation
#' @export
#'
#' @examples
#' iaw$standardize(c(1, 2, 3, 4, 5))   # -1.26, -0.63, 0, 0.63, 1.26
#'
#' # Mean is exactly 0 and SD exactly 1 after standardizing
#' z <- iaw$standardize(c(10, 20, 30, 40, 50))
#' mean(z)   # 0
#' sd(z)     # 1
#'
#' # Standardize a column in a data frame before regression
#' d <- data.frame(y = rnorm(50), x1 = rnorm(50, mean = 100, sd = 15))
#' d$x1_z <- iaw$standardize(d$x1)
#'
#' # NA values are preserved in their positions
#' iaw$standardize(c(1, NA, 3, 4, 5))

iaw$standardize <- function(x) {
    stopifnot(is.numeric(x))
    scale(x)[, 1]
}
