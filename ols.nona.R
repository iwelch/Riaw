
#' Fast OLS Regressions If No NA's are in the Data
#'
#' @name ols.nona
#'
#' a fast OLS coefficient **if** there are no NAs in the data
#'
#' @details returns a coefficient vector (if no detail is requested), but a list if detail is request
#'
#' @usage ols.nona (y, X, w=NULL, detail=[0,1,2,3,4], checkna=FALSE)
#'
#' @param y a numeric vector for the dependent observations
#' @param X a numeric matrix for the independent observations.  should have a constant (1) as the first column
#' @param w a numeric weight vector.
#'
#' @return detail=0: a coefficient vector.
#' @return detail=1, a list with coef, df, and sigma.
#' @return detail=2 adds mse, rmse, rsq.
#' @return detail=3 adds residuals and fitted
#'


iaw$ols.nona <- function (y, X, w= NULL, detail = 0, checkna = FALSE) {

    message("Switch from ols.nona to Rfast library, please.")

    (is.matrix(X)) %or% "X needs to be a matrix.  if you mean to run a no-intercept regression, force it\n";
    stopifnot(nrow(X) == length(y))
    if (!is.null(w)) { stopifnot(length(y)==length(w)); sw <- sqrt(w); y <- y*sw; X <- diag(sw) %*% X }

    if (checkna) {
        ## warn("iaw$ols.nona: no speed advantage if there are NA's")
        ## checkna completely negates all the speed advantage relative to lm
        rowhasna <-  (rowSums( cbind( apply( X, 2, is.na), is.na(y) ) ) > 0)
        X <- X[!rowhasna,]
        y <- y[!rowhasna]
    } else {
        ## ignore checking and just proceed --- to speed up
    }

    XtX <- crossprod(X)
    Xty <- crossprod(X, y)
    xpxi <- solve(XtX)  ## keep intermediate

    b <- xpxi %*% Xty
                                        # bnames <- colnames(b); b <- as.vector(b); names(b) <- bnames
    b <- as.vector(b)

    if (detail == 0) return(b)

    ## syntax check only if we have time
    if (!(all(X[,1] == 1))) warning("you are running a regression without a constant in ols3.nomissing")
    df <- nrow(X)-ncol(X)
    f <- crossprod(t(X),b)
    e <- y-f
    sigmasq <- sum(e^2)/df
    rl <- list( b=as.vector(b), df=df, sigma=sqrt(sigmasq) )

    if (detail<=1) return(rl)

    rl[["se.coefs"]] <- sqrt(diag(xpxi*sigmasq))

    if (detail<=2) return(rl)

    rl[["mse"]] <- sum(e^2)/(nrow(X)-1)
    rl[["rmse"]] <- sqrt(rl[["mse"]])
    rl[["rsq"]] <- 1-sum(e^2)/sum((y-mean(y))^2)
    rl[["rho1"]] <- cor( e[1:(length(e)-1)], e[2:length(e)] )
    rl[["rho2"]] <- cor( e[1:(length(e)-2)], e[3:length(e)] )

    if (detail<=3) return(rl)

    rl[["err"]] <- e ; rl[["fitted"]] <- f
    return(rl)
}
