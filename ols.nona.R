#' Fast OLS Regression Without NA Checking
#'
#' A fast OLS regression implementation using matrix operations. For maximum
#' speed, assumes no missing values in the data. Use \code{iaw$ols()} if your
#' data may contain NAs.
#'
#' @param y A numeric vector of dependent variable observations.
#' @param X A numeric matrix of independent variables. Should include a column
#'   of ones for the intercept.
#' @param w Optional numeric vector of weights (same length as y).
#' @param detail Integer controlling output verbosity:
#'   \itemize{
#'     \item 0: coefficients only (default)
#'     \item 1: coefficients, degrees of freedom, and sigma
#'     \item 2: adds standard errors of coefficients
#'     \item 3: adds MSE, RMSE, R-squared, and autocorrelations
#'     \item 4: adds residuals and fitted values
#'   }
#' @param checkna Logical; if TRUE, removes rows with NA values. Default FALSE
#'   for speed.
#'
#' @return Depends on \code{detail}:
#'   \itemize{
#'     \item detail=0: coefficient vector
#'     \item detail=1: list with \code{b}, \code{df}, \code{sigma}
#'     \item detail=2: adds \code{se.coefs}
#'     \item detail=3: adds \code{mse}, \code{rmse}, \code{rsq}, \code{rho1}, \code{rho2}
#'     \item detail=4: adds \code{err}, \code{fitted}
#'   }
#'
#' @details
#' Computes OLS estimates using the normal equations:
#' \deqn{\hat{\beta} = (X'X)^{-1}X'y}
#'
#' When weights are provided, transforms to weighted least squares by
#' multiplying both y and X by the square root of the weights.
#'
#' @note Consider using the \code{Rfast} package for even faster regressions
#' on large datasets.
#'
#' @export
#'
#' @seealso \code{\link{iaw$ols}}, \code{\link{iaw$olm}}, \code{\link{lm}}
#'
#' @examples
#' # Basic regression
#' n <- 1000
#' X <- cbind(1, rnorm(n), rnorm(n))
#' colnames(X) <- c("(Intercept)", "x1", "x2")
#' y <- X %*% c(1, 2, 3) + rnorm(n)
#'
#' # Coefficients only (fastest)
#' iaw$ols.nona(y, X, detail = 0)
#'
#' # With standard errors
#' result <- iaw$ols.nona(y, X, detail = 2)
#' result$b
#' result$se.coefs
#'
#' # Full output including R-squared
#' result <- iaw$ols.nona(y, X, detail = 3)
#' result$rsq
#'
#' # Weighted least squares
#' w <- runif(n, 0.5, 2)
#' iaw$ols.nona(y, X, w = w, detail = 1)

iaw$ols.nona <- function(y, X, w = NULL, detail = 0, checkna = FALSE) {

    (is.matrix(X)) %or% "X must be a matrix. For no-intercept regression, include column of zeros"
    stopifnot(nrow(X) == length(y))

    # Apply weights if provided
    if (!is.null(w)) {
        stopifnot(length(y) == length(w))
        sw <- sqrt(w)
        y <- y * sw
        X <- diag(sw) %*% X
    }

    # Optionally remove NA rows (slower)
    if (checkna) {
        rowhasna <- rowSums(cbind(apply(X, 2, is.na), is.na(y))) > 0
        X <- X[!rowhasna, ]
        y <- y[!rowhasna]
    }

    # Compute OLS coefficients using normal equations
    XtX <- crossprod(X)
    Xty <- crossprod(X, y)
    xpxi <- solve(XtX)
    b <- as.vector(xpxi %*% Xty)
    names(b) <- colnames(X)

    if (detail == 0) return(b)

    # Check for intercept
    if (!(all(X[, 1] == 1))) {
        warning("Running regression without intercept in first column")
    }

    df <- nrow(X) - ncol(X)
    f <- as.vector(X %*% b)
    e <- y - f
    sigmasq <- sum(e^2) / df

    rl <- list(b = b, df = df, sigma = sqrt(sigmasq))

    if (detail <= 1) return(rl)

    rl$se.coefs <- sqrt(diag(xpxi * sigmasq))

    if (detail <= 2) return(rl)

    rl$mse <- sum(e^2) / (nrow(X) - 1)
    rl$rmse <- sqrt(rl$mse)
    rl$rsq <- 1 - sum(e^2) / sum((y - mean(y))^2)
    rl$rho1 <- cor(e[1:(length(e) - 1)], e[2:length(e)])
    rl$rho2 <- cor(e[1:(length(e) - 2)], e[3:length(e)])

    if (detail <= 3) return(rl)

    rl$err <- e
    rl$fitted <- f

    rl
}
