#' Row Standard Deviations
#'
#' Calculates the standard deviation of each row in a matrix or data frame.
#'
#' @param x A matrix or data frame.
#' @param na.rm Logical; if TRUE, NA values are removed. Default FALSE.
#' @param dims Integer; dimension to operate over. Default 1.
#'
#' @return A named numeric vector with standard deviations for each row.
#'
#' @export
#'
#' @seealso \code{\link{iaw$colSds}}, \code{\link{rowMeans}}, \code{\link{apply}}
#'
#' @examples
#' m <- matrix(rnorm(30), nrow = 10)
#' rownames(m) <- paste0("row", 1:10)
#' iaw$rowSds(m)

iaw$rowSds <- function(x, na.rm = FALSE, dims = 1L) {
    if (is.data.frame(x)) x <- as.matrix(x)
    if (!is.array(x) || length(dn <- dim(x)) < 2L)
        stop("'x' must be an array of at least two dimensions")
    if (dims < 1L || dims > length(dn) - 1L)
        stop("invalid 'dims'")
    n <- prod(dn[id <- seq_len(dims)])
    dn <- dn[-id]
    z <- apply(x, 1, function(o) sd(as.numeric(o), na.rm = na.rm))
    if (length(dn) > 1L) {
        dim(z) <- dn
        dimnames(z) <- dimnames(x)[-id]
    } else {
        names(z) <- rownames(x)
    }
    z
}
