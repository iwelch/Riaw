#' Enhanced Multicore Sapply
#'
#' @name mcsapply
#'
#' Wrapper for mclapply with simplification.
#'
#' @param X List or vector.
#' @param FUN Function to apply.
#' @param ... Arguments to FUN.
#'
#' @return Simplified result.
#'
#' @family parallel
#' @export
#'
#' @examples
#' \dontrun{
#' # Returns a numeric vector (like sapply) rather than a list
#' iaw$mcsapply(1:10, function(x) x^2)
#'
#' # Compute summary stats across many simulations, returning a matrix
#' stats <- iaw$mcsapply(1:100, function(i) {
#'   x <- rnorm(50)
#'   c(mean = mean(x), sd = sd(x))
#' })
#' dim(stats)   # 2 x 100 matrix; rowMeans(stats) gives grand mean and sd
#'
#' # Apply a function across column names of a data frame
#' df <- data.frame(a = rnorm(30), b = rnorm(30), c = rnorm(30))
#' iaw$mcsapply(names(df), function(col) mean(df[[col]]))
#'
#' # Parallel column-wise standard deviations
#' mat <- matrix(rnorm(300), ncol = 10)
#' iaw$mcsapply(1:ncol(mat), function(j) sd(mat[, j]))
#'
#' # Estimate multiple sample quantiles in parallel
#' data_list <- list(a = rnorm(1000), b = rexp(1000), c = runif(1000))
#' medians <- iaw$mcsapply(data_list, median)
#' medians  # named numeric vector: a ~ 0, b ~ 0.69, c ~ 0.5
#' }

iaw$mcsapply <- function(X, FUN, ...) {
    simplify2array(iaw$mclapply(X, FUN, ...))
}

#' @rdname mcsapply
#' @export
iaw$mc.sapply <- iaw$mcsapply
