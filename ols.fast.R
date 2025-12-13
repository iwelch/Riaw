#' Fast Ordinary Least Squares Regression
#'
#' Performs OLS regression using matrix operations for speed. Handles missing
#' values properly and returns results in a consistent structure.
#'
#' @param y Numeric vector of dependent variable observations
#' @param X Numeric matrix of independent variables. Should include a column
#'   of ones for the intercept if desired.
#' @param weights Optional numeric vector of weights (same length as y)
#' @param na_action How to handle missing values: "fail" (default, throws error),
#'   "omit" (removes rows with any NA), or "complete.cases" (alias for omit)
#'
#' @return A list of class "ols_fast" containing:
#'   \item{coefficients}{Named vector of coefficient estimates}
#'   \item{residuals}{Vector of residuals (with NAs in original positions if na_action="omit")}
#'   \item{fitted.values}{Vector of fitted values (with NAs in original positions if na_action="omit")}
#'   \item{df.residual}{Residual degrees of freedom}
#'   \item{sigma}{Residual standard error}
#'   \item{vcov}{Variance-covariance matrix of coefficients}
#'   \item{r.squared}{R-squared}
#'   \item{adj.r.squared}{Adjusted R-squared}
#'   \item{n}{Number of observations used}
#'   \item{n_omitted}{Number of observations removed due to NAs}
#'   \item{weights}{Weights used (or NULL)}
#'   \item{call}{The matched call}
#'
#' @details
#' This function computes OLS estimates using the normal equations:
#' \deqn{\hat{\beta} = (X'X)^{-1}X'y}
#'
#' When weights are provided, the weighted least squares problem is solved:
#' \deqn{\hat{\beta} = (X'WX)^{-1}X'Wy}
#'
#' The function is faster than \code{lm()} for datasets without complex formula
#' parsing needs, but less flexible.
#'
#' @examples
#' # Simple regression
#' set.seed(123)
#' n <- 100
#' X <- cbind(Intercept = 1, x1 = rnorm(n), x2 = rnorm(n))
#' y <- 2 + 3 * X[, "x1"] - 1.5 * X[, "x2"] + rnorm(n)
#' 
#' result <- ols_fast(y, X)
#' print(result)
#' summary(result)
#' 
#' # With missing values
#' y[5] <- NA
#' X[10, 2] <- NA
#' result_na <- ols_fast(y, X, na_action = "omit")
#' 
#' # With weights
#' w <- runif(n, 0.5, 1.5)
#' result_wls <- ols_fast(y, X, weights = w, na_action = "omit")
#' 
#' # Compare with lm()
#' df <- data.frame(y = y, X[, -1])
#' lm_result <- lm(y ~ x1 + x2, data = df)
#' all.equal(coef(result_na), coef(lm_result), check.attributes = FALSE)
#'
#' @seealso \code{\link{lm}}, \code{\link{lm.fit}}
#' @export
ols_fast <- function(y, X, weights = NULL, na_action = c("fail", "omit", "complete.cases")) {
  
  # Capture call for output
  matched_call <- match.call()
  
  # Input validation
  na_action <- match.arg(na_action)
  if (na_action == "complete.cases") na_action <- "omit"
  
  if (!is.numeric(y)) {
    stop("y must be numeric, got ", class(y)[1])
  }
  if (!is.vector(y) && !is.matrix(y)) {
    stop("y must be a vector or single-column matrix, got ", class(y)[1])
  }
  if (is.matrix(y) && ncol(y) != 1) {
    stop("y must be a vector or single-column matrix, got matrix with ", ncol(y), " columns")
  }
  
  # Convert y to vector if matrix
  y <- as.vector(y)
  
  if (!is.matrix(X)) {
    stop("X must be a matrix, got ", class(X)[1], 
         ". Use as.matrix() or cbind() to convert.")
  }
  if (!is.numeric(X)) {
    stop("X must be numeric, got ", typeof(X))
  }
  
  if (nrow(X) != length(y)) {
    stop("Number of rows in X (", nrow(X), ") must equal length of y (", 
         length(y), ")")
  }
  
  if (!is.null(weights)) {
    if (!is.numeric(weights) || !is.vector(weights)) {
      stop("weights must be a numeric vector, got ", class(weights)[1])
    }
    if (length(weights) != length(y)) {
      stop("Length of weights (", length(weights), ") must equal length of y (", 
           length(y), ")")
    }
    if (any(weights < 0, na.rm = TRUE)) {
      stop("weights must be non-negative")
    }
  }
  
  # Check for intercept column (all 1s)
  has_intercept <- any(apply(X, 2, function(col) {
    all(col == 1, na.rm = TRUE) && sum(!is.na(col)) > 0
  }))
  
  if (!has_intercept) {
    warning("X does not appear to contain an intercept column (all 1s). ",
            "Regression will be through the origin. ",
            "Add intercept with: X <- cbind(Intercept = 1, X)")
  }
  
  # Store original indices for later reconstruction
  n_original <- length(y)
  original_indices <- seq_len(n_original)
  
  # Handle missing values
  if (na_action == "fail") {
    # Check for any NAs
    y_has_na <- any(is.na(y))
    X_has_na <- any(is.na(X))
    
    if (y_has_na || X_has_na) {
      na_summary <- character()
      if (y_has_na) {
        na_summary <- c(na_summary, 
                       sprintf("y has %d missing value(s)", sum(is.na(y))))
      }
      if (X_has_na) {
        na_count_by_col <- colSums(is.na(X))
        na_cols <- which(na_count_by_col > 0)
        na_summary <- c(na_summary,
                       sprintf("X has %d missing value(s) across %d column(s): %s",
                               sum(is.na(X)), 
                               length(na_cols),
                               paste(names(na_count_by_col)[na_cols], "=", 
                                     na_count_by_col[na_cols], collapse = ", ")))
      }
      
      stop("Missing values detected and na_action='fail':\n  ",
           paste(na_summary, collapse = "\n  "),
           "\nUse na_action='omit' to remove rows with missing values.")
    }
  } else if (na_action == "omit") {
    # Find complete cases
    complete_cases <- complete.cases(y, X)
    
    if (!is.null(weights)) {
      complete_cases <- complete_cases & !is.na(weights)
    }
    
    n_omitted <- sum(!complete_cases)
    
    if (n_omitted > 0) {
      message(sprintf("Removing %d row(s) with missing values (%.1f%% of data)",
                     n_omitted, 100 * n_omitted / n_original))
    }
    
    if (sum(complete_cases) == 0) {
      stop("No complete cases remaining after removing missing values")
    }
    
    # Subset to complete cases
    kept_indices <- original_indices[complete_cases]
    y <- y[complete_cases]
    X <- X[complete_cases, , drop = FALSE]
    if (!is.null(weights)) {
      weights <- weights[complete_cases]
    }
  }
  
  n_used <- length(y)
  k <- ncol(X)
  
  # Check degrees of freedom
  if (n_used <= k) {
    stop(sprintf("Insufficient observations: n=%d observations, k=%d coefficients. ",
                "Need n > k for estimation.", n_used, k))
  }
  
  # Apply weights if provided
  if (!is.null(weights)) {
    # Check for zero or near-zero weights
    if (any(weights == 0)) {
      warning("Some weights are exactly zero. These observations will be ignored.")
      nonzero_weights <- weights > 0
      y <- y[nonzero_weights]
      X <- X[nonzero_weights, , drop = FALSE]
      weights <- weights[nonzero_weights]
      kept_indices <- kept_indices[nonzero_weights]
      n_used <- length(y)
    }
    
    sqrt_weights <- sqrt(weights)
    y_weighted <- y * sqrt_weights
    X_weighted <- X * sqrt_weights  # Broadcasts correctly
  } else {
    y_weighted <- y
    X_weighted <- X
  }
  
  # Compute OLS estimates via normal equations
  # Use crossprod for efficiency: crossprod(X) = t(X) %*% X
  XtX <- crossprod(X_weighted)
  Xty <- crossprod(X_weighted, y_weighted)
  
  # Check for singularity
  XtX_cond <- tryCatch(
    kappa(XtX, exact = FALSE),
    error = function(e) Inf
  )
  
  if (is.infinite(XtX_cond) || XtX_cond > 1e10) {
    warning("X'X is nearly singular (condition number = ", 
            format(XtX_cond, scientific = TRUE, digits = 2), 
            "). Results may be unreliable. Check for multicollinearity.")
  }
  
  # Solve for coefficients
  XtX_inv <- tryCatch(
    solve(XtX),
    error = function(e) {
      stop("Cannot invert X'X matrix. Possible causes:\n",
           "  - Perfect multicollinearity in X\n",
           "  - Rank deficient design matrix\n",
           "  - Numeric instability\n",
           "Original error: ", conditionMessage(e))
    }
  )
  
  beta <- as.vector(XtX_inv %*% Xty)
  
  # Name coefficients
  if (!is.null(colnames(X))) {
    names(beta) <- colnames(X)
  } else {
    names(beta) <- paste0("X", seq_len(k))
  }
  
  # Compute fitted values and residuals
  fitted <- as.vector(X %*% beta)
  resid <- y - fitted
  
  # Degrees of freedom
  df_residual <- n_used - k
  
  # Residual standard error
  rss <- sum(resid^2)
  sigma <- sqrt(rss / df_residual)
  
  # Variance-covariance matrix of coefficients
  vcov_matrix <- sigma^2 * XtX_inv
  rownames(vcov_matrix) <- colnames(vcov_matrix) <- names(beta)
  
  # R-squared
  if (has_intercept) {
    tss <- sum((y - mean(y))^2)
    r_squared <- 1 - rss / tss
    adj_r_squared <- 1 - (rss / df_residual) / (tss / (n_used - 1))
  } else {
    # For models without intercept, use uncentered R-squared
    tss <- sum(y^2)
    r_squared <- 1 - rss / tss
    adj_r_squared <- 1 - (rss / df_residual) / (tss / n_used)
    message("Note: R-squared calculated for model without intercept (uncentered)")
  }
  
  # Reconstruct full-length residuals and fitted values with NAs
  if (na_action == "omit" && n_original != n_used) {
    full_resid <- rep(NA_real_, n_original)
    full_fitted <- rep(NA_real_, n_original)
    full_resid[kept_indices] <- resid
    full_fitted[kept_indices] <- fitted
  } else {
    full_resid <- resid
    full_fitted <- fitted
  }
  
  # Return results as a structured list
  result <- list(
    coefficients = beta,
    residuals = full_resid,
    fitted.values = full_fitted,
    df.residual = df_residual,
    sigma = sigma,
    vcov = vcov_matrix,
    r.squared = r_squared,
    adj.r.squared = adj_r_squared,
    rss = rss,
    tss = if (has_intercept) sum((y - mean(y))^2) else sum(y^2),
    n = n_used,
    n_original = n_original,
    n_omitted = if (na_action == "omit") n_original - n_used else 0L,
    weights = weights,
    has_intercept = has_intercept,
    call = matched_call
  )
  
  class(result) <- "ols_fast"
  return(result)
}


#' @export
print.ols_fast <- function(x, digits = 4, ...) {
  cat("\nCall:\n")
  print(x$call)
  
  cat("\nCoefficients:\n")
  print(round(x$coefficients, digits))
  
  cat("\nResidual standard error:", round(x$sigma, digits), 
      "on", x$df.residual, "degrees of freedom")
  
  if (x$n_omitted > 0) {
    cat("\n(", x$n_omitted, " observation(s) deleted due to missingness)", sep = "")
  }
  
  cat("\nR-squared:", round(x$r.squared, digits), 
      "  Adjusted R-squared:", round(x$adj.r.squared, digits))
  cat("\n")
  
  invisible(x)
}


#' @export
summary.ols_fast <- function(object, ...) {
  
  se <- sqrt(diag(object$vcov))
  t_stat <- object$coefficients / se
  p_value <- 2 * pt(abs(t_stat), df = object$df.residual, lower.tail = FALSE)
  
  coef_table <- cbind(
    Estimate = object$coefficients,
    `Std. Error` = se,
    `t value` = t_stat,
    `Pr(>|t|)` = p_value
  )
  
  result <- list(
    call = object$call,
    coefficients = coef_table,
    sigma = object$sigma,
    df = c(length(object$coefficients), object$df.residual, length(object$coefficients)),
    r.squared = object$r.squared,
    adj.r.squared = object$adj.r.squared,
    n = object$n,
    n_omitted = object$n_omitted,
    has_intercept = object$has_intercept
  )
  
  class(result) <- "summary.ols_fast"
  return(result)
}


#' @export
print.summary.ols_fast <- function(x, digits = 4, signif.stars = TRUE, ...) {
  cat("\nCall:\n")
  print(x$call)
  
  if (x$n_omitted > 0) {
    cat("\n(", x$n_omitted, " observation(s) deleted due to missingness)\n", sep = "")
  }
  
  cat("\nCoefficients:\n")
  printCoefmat(x$coefficients, digits = digits, signif.stars = signif.stars,
               P.values = TRUE, has.Pvalue = TRUE, ...)
  
  cat("\nResidual standard error:", round(x$sigma, digits), 
      "on", x$df[2], "degrees of freedom")
  cat("\nR-squared:", round(x$r.squared, digits), 
      "  Adjusted R-squared:", round(x$adj.r.squared, digits))
  cat("\n")
  
  invisible(x)
}


#' @export
coef.ols_fast <- function(object, ...) {
  object$coefficients
}


#' @export
residuals.ols_fast <- function(object, ...) {
  object$residuals
}


#' @export
fitted.ols_fast <- function(object, ...) {
  object$fitted.values
}


#' @export
vcov.ols_fast <- function(object, ...) {
  object$vcov
}


#' Predict method for ols_fast objects
#'
#' @param object An object of class "ols_fast"
#' @param newdata Optional data frame or matrix with same structure as original X
#' @param se.fit Logical, whether to return standard errors of predictions
#' @param interval Type of interval: "none", "confidence", or "prediction"
#' @param level Confidence level for intervals
#' @param ... Additional arguments (currently unused)
#'
#' @export
predict.ols_fast <- function(object, newdata = NULL, se.fit = FALSE,
                              interval = c("none", "confidence", "prediction"),
                              level = 0.95, ...) {
  
  interval <- match.arg(interval)
  
  if (is.null(newdata)) {
    # Return fitted values from original data
    pred <- object$fitted.values
    
    if (se.fit || interval != "none") {
      # Standard errors for fitted values would require original X matrix
      warning("Cannot compute standard errors or intervals without original X matrix. ",
              "Returning fitted values only.")
      return(pred)
    }
    return(pred)
  }
  
  # Ensure newdata is a matrix
  if (is.data.frame(newdata)) {
    newdata <- as.matrix(newdata)
  }
  if (!is.matrix(newdata)) {
    stop("newdata must be a matrix or data frame")
  }
  
  # Check dimensions
  if (ncol(newdata) != length(object$coefficients)) {
    stop("newdata has ", ncol(newdata), " columns but model has ", 
         length(object$coefficients), " coefficients")
  }
  
  # Compute predictions
  pred <- as.vector(newdata %*% object$coefficients)
  
  if (!se.fit && interval == "none") {
    return(pred)
  }
  
  # Compute standard errors if requested
  pred_var <- diag(newdata %*% object$vcov %*% t(newdata))
  pred_se <- sqrt(pred_var)
  
  if (interval == "none") {
    return(list(fit = pred, se.fit = pred_se, df = object$df.residual))
  }
  
  # Compute confidence or prediction intervals
  t_crit <- qt((1 + level) / 2, df = object$df.residual)
  
  if (interval == "confidence") {
    margin <- t_crit * pred_se
  } else {  # prediction interval
    pred_se_total <- sqrt(pred_var + object$sigma^2)
    margin <- t_crit * pred_se_total
  }
  
  result <- cbind(
    fit = pred,
    lwr = pred - margin,
    upr = pred + margin
  )
  
  if (se.fit) {
    return(list(fit = result, se.fit = pred_se, df = object$df.residual))
  } else {
    return(result)
  }
}

