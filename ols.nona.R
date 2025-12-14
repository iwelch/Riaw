#' Fast OLS Without NA Checking
#'
#' @name ols.nona
#'
#' Fast OLS assuming no missing values.
#'
#' @param y Numeric vector of dependent variable.
#' @param X Numeric matrix of regressors.
#' @param w Optional weight vector.
#' @param detail Output verbosity (0-4).
#' @param checkna If TRUE, remove NA rows.
#'
#' @return List of regression results.
#'
#' @family regression
#' @export
#'
#' @examples
#' n <- 100
#' X <- cbind(1, rnorm(n))
#' y <- X %*% c(1, 2) + rnorm(n)
#' iaw$ols.nona(y, X)

iaw$ols.nona <- function(y, X, w = NULL, detail = 0, checkna = FALSE) {
    stopifnot(is.matrix(X))
    stopifnot(is.numeric(y))
    stopifnot(nrow(X) == length(y))
    stopifnot(is.numeric(detail), length(detail) == 1L)
    
    if (!is.null(w)) {
        stopifnot(length(y) == length(w))
        sw <- sqrt(w)
        y <- y * sw
        X <- diag(sw) %*% X
    }
    
    if (checkna) {
        rowhasna <- rowSums(cbind(apply(X, 2, is.na), is.na(y))) > 0
        X <- X[!rowhasna, ]
        y <- y[!rowhasna]
    }
    
    XtX <- crossprod(X)
    Xty <- crossprod(X, y)
    xpxi <- solve(XtX)
    b <- as.vector(xpxi %*% Xty)
    names(b) <- colnames(X)
    
    if (detail == 0) return(b)
    
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
    
    if (detail <= 3) return(rl)
    rl$err <- e
    rl$fitted <- f
    rl
}
