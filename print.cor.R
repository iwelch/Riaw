#' Nice correlation printout
#'
#' @name print.cor
#'
#' prints nice correlation output
#'
#' @param d Data frame or matrix.
#' @param digits Number of digits to display (default 2).
#' @param use Method for handling missing values (default \code{"pairwise.complete.obs"}).
#'
#' @return an invisible correlation
#'
#' @family statistics
#' @export
#'
#' @examples
#' # Simple two-variable correlation matrix
#' set.seed(42)
#' df <- data.frame(x = rnorm(30), y = rnorm(30), z = rnorm(30))
#' iaw$print.cor(df)
#'
#' # Strongly correlated variables
#' x <- sin(1:50) + rnorm(50, sd = 0.1)
#' y <- sin(2:51) + rnorm(50, sd = 0.1)
#' iaw$print.cor(data.frame(x, y))
#'
#' # More decimal places for fine-grained display
#' iaw$print.cor(data.frame(x, y), digits = 4)
#'
#' # Matrix input also works
#' m <- matrix(c(x, y), ncol = 2, dimnames = list(NULL, c("x", "y")))
#' iaw$print.cor(m)
#'
#' # Financial factor correlation: size, value, momentum
#' set.seed(100)
#' n <- 120  # monthly observations
#' size  <- rnorm(n, 0.003, 0.02)
#' value <- 0.3 * size + rnorm(n, 0.002, 0.015)
#' mom   <- -0.1 * size + rnorm(n, 0.004, 0.025)
#' factors <- data.frame(Size = size, Value = value, Momentum = mom)
#' iaw$print.cor(factors)
#'
#' # Ignore non-numeric columns automatically
#' df2 <- data.frame(sector = c("Tech", "Fin", "Health"),
#'                   ret1 = c(0.1, 0.05, 0.08),
#'                   ret2 = c(0.12, 0.04, 0.09))
#' iaw$print.cor(df2)  # only ret1 and ret2 are correlated
#'
#' # Use complete observations instead of pairwise
#' df3 <- data.frame(a = c(1, NA, 3, 4), b = c(2, 3, NA, 5), c = c(1, 2, 3, 4))
#' iaw$print.cor(df3, use = "complete.obs")


iaw$print.cor <- function(d, digits = 2, use =  "pairwise.complete.obs") {
  stopifnot(is.data.frame(d) | is.matrix(d))

  nums <- if (is.matrix(d)) rep(is.numeric(d), ncol(d)) else sapply(d, is.numeric)
  cm <- cor(d[, nums], use = use)

  oold <- options(knitr.kable.NA = "")
  cm[upper.tri(cm, diag = TRUE)] <- NA
  diag(cm) <- 1

  print(knitr::kable(cm, digits = digits), na.print = "")
  options( oold )
  invisible(cm)
}
