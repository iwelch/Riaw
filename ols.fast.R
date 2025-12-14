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
#' \deqn{\hat{eta} = (X'X)^{-1}X'y}
#'
#' When weights are provided, the weighted least squares problem is solved:
#' \deqn{\hat{eta} = (X'WX)^{-1}X'Wy}
#'
#' The function is faster than 